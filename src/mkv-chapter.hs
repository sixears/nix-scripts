{-# OPTIONS_GHC -Wall              #-}

{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude     #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE Rank2Types            #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE UnicodeSyntax         #-}

import Prelude  ( error )

-- base --------------------------------

import Control.Monad   ( return )
import Data.Bifunctor  ( first )
import Data.Either     ( either )
import Data.Function   ( ($), id )
import Data.Maybe      ( Maybe( Nothing ) )
import Data.String     ( String )
import Data.Word       ( Word8 )
import GHC.Stack       ( HasCallStack )
import System.IO       ( FilePath, IO )
import Text.Show       ( show )

-- base-unicode-functions --------------

import Data.Eq.Unicode        ( (≡) )
import Data.Function.Unicode  ( (∘) )
import Data.Monoid.Unicode    ( (⊕) )

-- data-textual ------------------------

import Data.Textual  ( Printable, toString, toText )

-- fpath -------------------------------

import FPath.AbsDir            ( AbsDir )
import FPath.AbsFile           ( AbsFile )
import FPath.Error.FPathError  ( AsFPathError, FPathIOError )
import FPath.File              ( File )
import FPath.Parseable         ( parse, parse', readM )

-- lens --------------------------------

import Control.Lens.Lens  ( Lens', lens )

-- monaderror-io -----------------------

import MonadError.IO        ( asIOError, eitherIOThrowT )
import MonadError.IO.Error  ( AsIOError, IOError )

-- monadio-plus ------------------------

import MonadIO       ( MonadIO, liftIO )
import MonadIO.FPath ( getCwd )

-- more-unicode ------------------------

import Data.MoreUnicode.Applicative  ( (⊵) )
import Data.MoreUnicode.Functor      ( (⊳) )
import Data.MoreUnicode.Lens         ( (⊣) )

-- mtl ---------------------------------

import Control.Monad.Except  ( ExceptT, MonadError )

-- natural -----------------------------

import Natural  ( One )

-- optparse-plus -----------------------

-- import OptParsePlus        ( parseOpts )

-- options-applicative -----------------

import Options.Applicative.Builder  ( command, eitherReader, argument, info
                                    , metavar, progDesc , subparser )
import Options.Applicative.Types    ( Parser )

-- proclib -----------------------------

import ProcLib.CommonOpt.DryRun
                              ( DryRun, HasDryRunLevel( dryRunLevel )
                              , dryRunP )
import ProcLib.CommonOpt.Verbose
                              ( HasVerboseLevel( verboseLevel ), Verbose
                              , verboseP )
import ProcLib.Error.ExecCreateError
                              ( ExecCreateIOError )
import ProcLib.Process        ( mkProc_, system )
import ProcLib.Types.CmdSpec  ( CmdSpec( CmdSpec ) )
import ProcLib.Types.ProcIO   ( ProcIO' )

-- stdmain -----------------------------

import StdMain  ( stdMainNoDR' )
import StdMain.UsageError  ( UsageFPathIOError )

-- text --------------------------------

import qualified  Data.Text.IO  as  TextIO

import Data.Text     ( Text )

------------------------------------------------------------
--                     local imports                      --
------------------------------------------------------------

-- import Cmds               ( ffmpeg, ffmpegInfo_ )
-- import FFMpeg.StreamInfo  ( isAudio )

--------------------------------------------------------------------------------

------------------------------------------------------------
--                        Options                         --
------------------------------------------------------------

data RunMode = ModeExtract {- _from -} File {- _to -} File
             | ModeUpdate {- _chapters -} File {- _mkv -} File

{-
absFileP ∷ Printable τ ⇒ τ → AbsDir → Parser AbsFile
absFileP meta cwd =
  argument (eitherReader $ first show ∘ parse' cwd ∘ toText)
           (metavar $ toString meta)

absFileP' ∷ Text → AbsDir → Parser AbsFile
absFileP' = absFileP
-}

modeP ∷ Parser RunMode
modeP =
  let extractDesc = progDesc "extract chapters xml from mkv"
      updateDesc  = progDesc "update mkv to use chapters found in xml"
      extractP = command "extract" (info (ModeExtract ⊳ argument readM (metavar "MKVFN")
                                                      ⊵ argument readM (metavar "XMLFN")
                                         )
                                         extractDesc
                                   )
      updateP = command "update"   (info (ModeUpdate  ⊳ argument readM (metavar "CHAP.XML")
                                                      ⊵ argument readM (metavar "MKVFN")
                                         )
                                         updateDesc
                                   )
   in subparser (extractP ⊕ updateP)

data Options = Options { _dryRun  ∷ DryRun
                       , _verbose ∷ Verbose
                       , _runMode ∷ RunMode

                       }

dryRun ∷ Lens' Options DryRun
dryRun = lens _dryRun (\ o d → o { _dryRun = d })

instance HasDryRunLevel One Options where
  dryRunLevel = dryRun

verbose ∷ Lens' Options Verbose
verbose = lens _verbose (\ o v → o { _verbose = v })

instance HasVerboseLevel One Options where
  verboseLevel = verbose

runMode ∷ Lens' Options RunMode
runMode = lens _runMode (\ o m → o { _runMode = m })

parseOpts ∷ Parser Options
parseOpts = Options ⊳ dryRunP ⊵ verboseP ⊵ modeP

------------------------------------------------------------

-- factor out run, etc. (to ProcLib?)
-- make ProcLib Errors to be Exceptions

mkvtoolnix ∷ FilePath
mkvtoolnix = "__mkvtoolnix__"

mkvextract ∷ AbsFile
mkvextract = either (error ∘ show) id $ parse' (mkvtoolnix ⊕ "/bin/mkvextract")

mkvpropedit ∷ AbsFile
mkvpropedit = either (error ∘ show) id $ parse' (mkvtoolnix ⊕ "/bin/mkvpropedit")

system' ∷ Options → ProcIO' ExecCreateIOError (ExceptT ExecCreateIOError IO) α
         → IO α
system' = system

writeFile ∷ (MonadIO μ, AsIOError ε, MonadError ε μ) ⇒ FilePath → Text → μ ()
writeFile fn = asIOError ∘ TextIO.writeFile fn

writeFile' ∷ (MonadIO μ, MonadError IOError μ) ⇒ FilePath → Text → μ ()
writeFile' = writeFile

myMain ∷ ∀ ε μ .
         (MonadIO μ, HasCallStack, AsIOError ε, AsFPathError ε, MonadError ε μ)⇒
         Options → μ Word8
myMain opts = do
  cwd  ← getCwd

  -- https://forum.videohelp.com/threads/362630-Edit-Chapter-names-in-mkv-file
  case opts ⊣ runMode of
    ModeExtract infn outfn → do let cmd = CmdSpec mkvextract [ "chapters"
                                                             , toText infn
                                                             ]
                                xmltxt ← liftIO $ system' opts $ mkProc_ cmd
                                eitherIOThrowT $ writeFile' (toString outfn) xmltxt
                                return 0
    ModeUpdate chapfn mkvfn → do let cmd = CmdSpec mkvpropedit [ toText mkvfn
                                                               , "--chapters"
                                                               , toText chapfn
                                                               ]
                                 liftIO $ system' opts $ mkProc_ @_ @() cmd
                                 return 0

main = let progDesc = "read or update mkv chapter markers"
        in stdMainNoDR' progDesc parseOpts (myMain @UsageFPathIOError)

-- that's all, folks! ----------------------------------------------------------
