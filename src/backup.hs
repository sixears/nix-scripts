{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TupleSections     #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE UnicodeSyntax     #-}
{-# LANGUAGE ViewPatterns      #-}

-- XXX use of --verbose=info:{cshead^ioclasses=iocmdw,iocmdr}:/tmp/log
--     alias for ^^^ that just shows commands (no datestamp, etc.); optionally
--     to a file; optionally to an fd
import Base1T

-- base --------------------------------

import Data.Bool      ( bool )
import Data.Function  ( flip )
import Data.List      ( filter )
import Data.Maybe     ( catMaybes, fromJust, fromMaybe, isJust )

-- containers --------------------------

import qualified  Data.Map.Strict  as  Map
import qualified  Data.Set         as  Set

import Data.Map.Strict  ( (!?) )

-- domainnames -------------------------

import DomainNames.Hostname  ( Hostname, hostlocal )

-- exceptions --------------------------

import Control.Monad.Catch  ( MonadMask, bracket_ )

-- fpath -------------------------------

import FPath.Abs               ( Abs( AbsD ) )
import FPath.AbsDir            ( AbsDir, absdir, root )
import FPath.AbsFile           ( AbsFile )
import FPath.AppendableFPath   ( (⫻) )
import FPath.Basename          ( basename )
import FPath.Error.FPathError  ( AsFPathError )
import FPath.Parseable         ( parse )
import FPath.RelDir            ( RelDir, reldir )
import FPath.RelFile           ( RelFile, relfile )
import FPath.ToDir             ( toDir )

-- fstat -------------------------------

import FStat  ( FileType( SymbolicLink ), FStat, ftype, sampleLStat0 )

-- lens --------------------------------

import Control.Lens.Getter  ( view )

-- log-plus ----------------------------

import Log  ( Log, logIOT )

-- logging-effect ----------------------

import Control.Monad.Log  ( MonadLog
                          , Severity( Debug, Informational, Notice, Warning ) )

-- mockio ------------------------------

import MockIO.DoMock  ( DoMock( DoMock, NoMock ), HasDoMock( doMock ) )

-- mockio-cmds-inetutils ---------------

import MockIO.Cmds.InetUtils.Hostname  ( hostname )

-- mockio-cmds-util-linux --------------

import MockIO.Cmds.UtilLinux.FindMnt  ( FindMntData, FindMntDatum, filesystems
                                      , findmnt, findMntSource, sourceDevice )
import MockIO.Cmds.UtilLinux.Mount    ( mounts )
import MockIO.Cmds.UtilLinux.Umount   ( umounts )

-- mockio-cmds-util-rsync --------------

import MockIO.Cmds.RSync.RSync ( RSyncDeleteOpt( RSYNC_DELETE_BEFORE )
                               , RSyncOpt( RSYNC_VERBOSE, RSYNC_DELETE
                                         , RSYNC_PRESERVE_HARD_LINKS
                                         , RSYNC_PRESERVE_PARTIAL_TRANSFERS
                                         , RSYNC_NO_CROSS_FS_BOUNDARY
                                         )
                               , RSyncUsageError
                               , rsyncArchive
                               )

-- mockio-log --------------------------

import MockIO.IOClass      ( HasIOClass )
import MockIO.Log          ( mkIOL, warnIO )
import MockIO.MockIOClass  ( MockIOClass )

-- mockio-plus -------------------------

import MockIO.Directory          ( lsdir )
import MockIO.File               ( resolvelink )

-- monaderror-io -----------------------

import MonadError.IO.Error  ( IOError )

-- monadio-plus ------------------------

import MonadIO.Base                   ( getArgs )
import MonadIO.FStat                  ( isDir )
import MonadIO.Error.CreateProcError  ( AsCreateProcError )
import MonadIO.Error.ProcExitError    ( AsProcExitError )

-- mtl ---------------------------------

import Control.Monad.Reader  ( MonadReader, ReaderT, runReaderT, withReaderT )

-- natural -----------------------------

import Natural  ( Two, two )

-- stdmain -----------------------------

import StdMain             ( stdMain_ )
import StdMain.ProcOutputParseError
                           ( AsProcOutputParseError, AsTextError, ScriptError
                           , ҩ,ҩҩ )
import StdMain.StdOptions  ( DryRunLevel, StdOptions
                           , dryRunLevel, dryRunNum )
import StdMain.UsageError  ( AsUsageError, throwUsage )

-- text --------------------------------

import qualified  Data.Text  as  Text
import Data.Text  ( drop, isPrefixOf )

-- unix --------------------------------

import System.Posix.User  ( getEffectiveUserID )

--------------------------------------------------------------------------------

{-| Flag to say whether this backup target needs an explicit mount or not. -}
data DoMount = DoMount | NoDoMount
  deriving (Eq,Show)

rsync_opts ∷ Set.Set RSyncOpt
rsync_opts = Set.fromList [ RSYNC_VERBOSE, RSYNC_PRESERVE_HARD_LINKS
                          , RSYNC_PRESERVE_PARTIAL_TRANSFERS
                          , RSYNC_NO_CROSS_FS_BOUNDARY
                          , RSYNC_DELETE RSYNC_DELETE_BEFORE
                          ]

by_partlabel ∷ AbsDir
by_partlabel = [absdir|/dev/disk/by-partlabel/|]

name_repls ∷ Map.Map 𝕋 𝕋
name_repls = -- values must parse as a valid `PathComponent`
             Map.fromList [ ("EFI\\x20system\\x20partition", "efi")
                          , ("EFI\\x5cx20system\\x5cx20partition", "efi")
                          , ("swap", "NOMOUNT")
                          , ("c-archive3", "archive3")
                          , ("arch0", "archive0")
                          ]


name_repl ∷ 𝕋 → 𝕋
name_repl k = fromMaybe k $ name_repls !? k

------------------------------------------------------------

{-| Drop a prefix from the left (beginning) of some 𝕋, no-op if prefix not
    present. -}
stripL ∷ 𝕋 → 𝕋 → 𝕋
stripL p t = if p `isPrefixOf` t then drop (Text.length p) t else t

----------------------------------------

{-| Given a MockIO action with a DoMock qualifier, transform it to a MockIO
    action which effects at DryRunLevel 0 (only) (otherwise mocks). -}
ꙟ ∷ ∀ α ν η . ReaderT DoMock η α  → ReaderT (DryRunLevel ν) η α
ꙟ = withReaderT $ \ drl → if 0 < dryRunNum drl then DoMock else NoMock

{-| Given a MockIO action with a DoMock qualifier, transform it to a MockIO
    action which effects at DryRunLevel 0 or 1 (otherwise mocks). -}
ꙟꙟ ∷ ∀ α ν η . ReaderT DoMock η α  → ReaderT (DryRunLevel ν) η α
ꙟꙟ = withReaderT $ \ drl → if 1 < dryRunNum drl then DoMock else NoMock

----------------------------------------

{-| Specialization of `lsdir`, with a configurable mock value; and
    warn of any errors (but otherwise carrying on). -}
lsDirWithMockData ∷ ∀ ε ω σ μ .
                    (MonadIO μ,
                     AsIOError ε, AsFPathError ε, Printable ε, MonadError ε μ,
                     MonadReader σ μ, HasDoMock σ,
                     Default ω, HasIOClass ω, HasDoMock ω, MonadLog (Log ω) μ) ⇒
                    ([(AbsFile,FStat)],[(AbsDir,FStat)],[(AbsFile, IOError)])
                  → AbsDir
                  → μ [(AbsFile, FStat)]
lsDirWithMockData mock_data d = do
  do_mock ← view doMock
  (fs,_,es) ← lsdir @_ @IOError Informational mock_data d do_mock
  forM_ es $ \ (f,e) →
    -- like `MockIO.Log.warnIO`; but not a fixed MockIOClass to Log
    mkIOL Warning def ([fmtT|lsdir: '%T' ! %T|] f e) () (return ()) do_mock
  return fs

----------------------------------------

{-| Specialization of `lsdir`, to provide a usable mock value (@d/file@); and
    warn of any errors (but otherwise carrying on). -}
lsDir ∷ ∀ ε ω σ μ .
        (MonadIO μ, AsIOError ε, AsFPathError ε, Printable ε, MonadError ε μ,
         MonadReader σ μ, HasDoMock σ,
         Default ω, HasIOClass ω, HasDoMock ω, MonadLog (Log ω) μ) ⇒
        AbsDir → μ [(AbsFile, FStat)]
lsDir d = lsDirWithMockData ([(d ⫻ [relfile|root|],sampleLStat0)],[],[]) d

----------------------------------------

mountSources ∷ ∀ ε ω μ . (MonadIO μ, MonadLog (Log ω) μ, Default ω,
                          AsFPathError ε, AsIOError ε, MonadError ε μ) ⇒
               FindMntData → μ (Map.Map Abs [FindMntDatum])
mountSources d = do
  let mx _ 𝕹     = 𝕹
      mx x (𝕵 y) = 𝕵 (y,x)
      go = catMaybes ⊳ forM (d ⊣ filesystems) (\ x→ mx x ⊳ sourceDevice x)
      result = fmap (Map.fromListWith (⊕)) ∘ fmap (fmap (second pure)) $ go

  logIOT Debug ("hello" ∷ 𝕋)
  result


----------------------------------------

{-| Given the output of @findmnt@, and a file representing a device (by
    partition label); return a backup target directory, if there is one; along
    with a flag to say whether it needs explicitly mounting for the purposes of
    backup.
-}
findBackupTarget ∷ ∀ ε μ .
                   (MonadIO μ, MonadLog (Log MockIOClass) μ,
                    AsFPathError ε, AsIOError ε, AsTextError ε, Printable ε,
                    MonadError ε μ) ⇒
                   Hostname → FindMntData → AbsFile → FStat
                 → μ (𝕄 (AbsDir,DoMount))
findBackupTarget hstnm mntdata file (ftype → SymbolicLink) =
  logIOT Debug "findBackupTarget"  ⪼
  let -- readlink, no mocking (so mock value of / is irrelevant), log@Info
      resolve_link f = resolvelink Informational (AbsD root) f NoMock
      name = toText $ basename file
   in -- nasty hack to allow for non-standard nvme partition names on night
      if "night-a-" `isPrefixOf` name
      then return 𝕹
      else do
        dev ← resolve_link file
        mntSrcs ← mountSources mntdata
        let nme = name_repl $ stripL (toText (hostlocal hstnm) ⊕ "-") name
-- XXX findMntSourceResolveLinks or similar
            do_mount' = maybe DoMount (const NoDoMount)
                             (findMntSource mntdata (toText dev))
            do_mount = bool DoMount NoDoMount $ dev `Map.member` mntSrcs
         in if "NOMOUNT" ≢ nme
            then do
              -- we don't handle an error in this parse, because
              -- nme is a contraction or replacement of name, which
              -- is a basename; so parsing as a `RelFile`
              -- should never fail (so long as the replacements are
              -- valid).
              nme_pc ← parse @RelFile nme
              let mnt_targ ∷ AbsFile -- [absdir|/mnt/|] ⫻ nme_pc
                  mnt_targ = [absdir|/mnt/|] ⫻ nme_pc
              isDir mnt_targ ≫ \ case
                𝕿 → return ∘ 𝕵 $ (toDir mnt_targ, do_mount)
                𝕱 → ҩ $ [fmt|No such mount dir: %T|] mnt_targ
            else return 𝕹

-- We're only interested in symlinks (which everything in /dev/disk/by-partlabel
-- should be)
findBackupTarget _ _ _ _ = return 𝕹

----------------------------------------

{-| Given the output of @findmnt@, and a list of devices (by partition label);
    identify which devices need to be specially mounted to write backups to.

    The result is given as a map from device file name to mount target
    directory.
-}
findBackupTargets ∷ ∀ ε μ .
                    (MonadIO μ, MonadLog (Log MockIOClass) μ,
                     AsFPathError ε, AsIOError ε, AsTextError ε, Printable ε,
                     MonadError ε μ) ⇒
                    Hostname → FindMntData → [(AbsFile, FStat)]
                  → μ (Map.Map AbsFile (AbsDir,DoMount))

findBackupTargets hstnm mntdata files_and_stats =
  let mnts =
        sequence [ (file,) ⊳ findBackupTarget hstnm mntdata file stat |
                   (file,stat) ← files_and_stats ]
   in (Map.map fromJust ∘ Map.fromList ∘ filter (isJust ∘ snd) ) ⊳ mnts

----------------------------------------

{-| Work out the src directory for an rsync; special rules apply for @/root@, and
    @/boot/efi@. -}
-- XXX use quotePat here
srcFudge ∷ RelDir → RelDir
srcFudge ((≡ [reldir|root/|]) → 𝕿) = [reldir|./|]
srcFudge ((≡ [reldir|efi/|])  → 𝕿) = [reldir|boot/efi/|]
srcFudge d                           = d

----------------------------------------

type MonadIOClassLog η = MonadLog (Log MockIOClass) η

{-| Check that we're running as the root user; else throw.  In mock mode,
    Issues a warning (with @warnIO@), and does not throw. -}
checkIsRoot ∷ ∀ μ σ ε .
              (MonadIO μ, MonadReader σ μ, HasDoMock σ,
               MonadIOClassLog μ, AsUsageError ε, MonadError ε μ) ⇒ μ ()

checkIsRoot = do
  euid ← liftIO $ getEffectiveUserID
  case euid of
    0 → return ()
    _ → view doMock ≫ \ case
          NoMock → throwUsage ("this must be run as root"∷𝕋)
          DoMock → warnIO DoMock ("not root: would not continue"∷𝕋)

----------------------------------------

doMain ∷ ∀ ε μ .
         (MonadIO μ, MonadMask μ,
          AsProcExitError ε, AsProcOutputParseError ε, AsCreateProcError ε,
          AsFPathError ε, AsIOError ε, AsTextError ε, AsUsageError ε,
          Printable ε, MonadError ε μ, HasCallStack,
          MonadLog (Log MockIOClass) μ) ⇒
         StdOptions Two () → μ Word8
doMain opts = flip runReaderT (opts ⊣ dryRunLevel) $ do
  ꙟ checkIsRoot

  h            ← ꙟꙟ $ hostname Notice
  findmnt_data ← ꙟꙟ $ findmnt Notice
  partitions   ← ꙟꙟ $ lsDir by_partlabel

  let host_prefix = toText (hostlocal h) ⊕ "-"
      is_host_prefixed ∷ (AbsFile,ω) → 𝔹
      is_host_prefixed (f,_) = host_prefix `isPrefixOf` toText (basename f)

  tgts ← findBackupTargets h findmnt_data (filter is_host_prefixed partitions)

  let mnts = Map.map fst $ Map.filter (\ (_,m) → m ≡ DoMount) tgts
  bracket_ (ꙟ $ mounts Warning (Map.toList mnts)) (ꙟ $ umounts Warning mnts) $
    forM_ tgts $ \ (tgt,_) → do
      let src ∷ AbsDir
          src = root ⫻ srcFudge (basename tgt)
      ꙟ ∘ join ∘ ҩҩ $ rsyncArchive @RSyncUsageError Warning rsync_opts (src,tgt)
      return (0∷ℤ)
  return 0

----------------------------------------

main ∷ MonadIO μ ⇒ μ ()
main = let desc = "back up filesystems to pre-partitioned external disk"
        in getArgs ≫ stdMain_ @ScriptError two desc (pure ()) doMain

-- that's all, folks! ----------------------------------------------------------
