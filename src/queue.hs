{-# OPTIONS_GHC -Wall #-}

{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE QuasiQuotes          #-}
{-# LANGUAGE PatternSynonyms      #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TupleSections        #-}
{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UnicodeSyntax        #-}
{-# LANGUAGE ViewPatterns         #-}

import Prelude ( Integer, maxBound )

import Base1

-- base --------------------------------

import Control.Applicative  ( optional )
import Control.Concurrent   ( killThread, myThreadId )
import Data.Int             ( Int64 )
import Data.List            ( reverse )
import Data.List.NonEmpty   ( uncons )
import Data.String          ( unlines )
import System.Environment   ( getProgName )
import System.IO            ( IOMode( ReadMode ), hPutStrLn, stderr, stdin )
import Text.Read            ( readEither )

-- filelock ----------------------------

import System.FileLock  ( SharedExclusive( Exclusive ) )

-- fpath -------------------------------

import FPath.AbsFile           ( AbsFile, absfile )
import FPath.AsFilePath        ( AsFilePath )
import FPath.File              ( File( FileA ), FileAs )
import FPath.Error.FPathError  ( AsFPathError )
import FPath.Parseable         ( readM )

-- lens --------------------------------

import Control.Lens.Getter  ( view )

-- log-plus ----------------------------

import Log  ( Log, info', notice' )

-- logging-effect ----------------------

import Control.Monad.Log  ( LoggingT, MonadLog
                          , Severity( Informational, Notice ) )

-- mockio-log --------------------------

import MockIO.IOClass      ( HasIOClass )
import MockIO.Log          ( DoMock, HasDoMock )
import MockIO.MockIOClass  ( MockIOClass )

-- mockio-plus -------------------------

import MockIO.Flock                  ( flock, flockNB, unflock )
import MockIO.OpenFile               ( readFile )
import MockIO.Process                ( CmdRW( CmdW ), doProc )
import MockIO.Process.OutputDefault  ( OutputDefault( outDef ) )

-- monadio-plus ------------------------

import MonadIO.Base                  ( getArgs )
import MonadIO.Error.CreateProcError ( AsCreateProcError )
import MonadIO.Error.ProcExitError   ( AsProcExitError )
import MonadIO.Flock                 ( NamedFileLock )
import MonadIO.FPath                 ( pResolve )
import MonadIO.NamedHandle           ( pattern ℍ )
import MonadIO.Process.ExitInfo      ( ExitInfo )
import MonadIO.Process.ExitStatus    ( ExitStatus( ExitVal ), exitVal )

-- optparse-applicative ----------------

import Options.Applicative.Builder  ( argument, eitherReader, help, long
                                    , metavar, option, short, strArgument )
import Options.Applicative.Types    ( Parser, ReadM )

-- optparse-plus -----------------------

import OptParsePlus  ( parseNE )

-- parsec ------------------------------

import Text.Parsec.Prim  ( parse )

-- parsec-plus -------------------------

import ParsecPlus  ( AsParseError, Parsecable( parsec, parser ), ParseError )

-- parser-plus -------------------------

import ParserPlus  ( convertReadParser, parseMillis )

-- parsers -----------------------------

import Text.Parser.Char         ( digit )
import Text.Parser.Combinators  ( eof )

-- stdmain -----------------------------

import StdMain             ( stdMain )
import StdMain.UsageError  ( AsUsageError, UsageParseFPProcIOError )

-- timers ------------------------------

import Control.Concurrent.Suspend  ( Delay, msDelay )
import Control.Concurrent.Timer    ( TimerIO, oneShotTimer, stopTimer )

--------------------------------------------------------------------------------

newtype PID = PID Word32

instance Parsecable PID where
  parser = let check_bound i = if i > maxBound @Word32
                               then 𝓛 $ [fmt|%d too big for Word32|] i
                               else 𝓡 $ fromIntegral i
            in PID ⊳ convertReadParser check_bound (some digit)

------------------------------------------------------------

data Options = Options { _queue_files ∷ NonEmpty File
                       , _exe         ∷ AbsFile
                       , _args        ∷ [𝕋]
                       , _timeout     ∷ 𝕄 (𝕊,Delay)
                       }

queue_files ∷ Lens' Options (NonEmpty File)
queue_files = lens _queue_files (\ o qf → o { _queue_files = qf })

exe ∷ Lens' Options AbsFile
exe = lens _exe (\ o e → o { _exe = e })

args ∷ Lens' Options [𝕋]
args = lens _args (\ o as → o { _args = as })

timeout ∷ Lens' Options (𝕄 (𝕊,Delay))
timeout = lens _timeout (\ o t → o { _timeout = t })

read_delay ∷ 𝕊 → 𝔼 𝕊 Delay
read_delay s = case readEither @Integer s of
                 𝓛 e → 𝓛 e
                 𝓡 i → if i > fromIntegral (maxBound @Int64)
                       then 𝓛 $ "too big for Int64: " ⊕ s
                       else 𝓡 $ msDelay (fromIntegral i)

readDelay ∷ ReadM (𝕊,Delay)
readDelay =
  eitherReader $
    \ s → fmap (s,) $ first show (parse (parseMillis ⋪ eof) s s) ≫ read_delay

parseOptions ∷ Parser Options
parseOptions =
  let queue_help   = help "queue against this file"
      timeout_help =
        help (unlines [ "total time limit waiting for locks, in seconds; "
                      , "takes up to 3 decimal digits, for millisecond "
                      , "precision"
                      ])
   in Options ⊳ parseNE (option readM (short 'q' ⊕ long "queue" ⊕ queue_help))
      ⊵ argument readM (metavar "EXECUTABLE")
      ⊵ many (strArgument (metavar "CMDARG"))
      ⊵ optional (option readDelay
                         (ю [ short 't', long "timeout", timeout_help ]))

data Block = Block | NoBlock
  deriving (Eq,Show)

data Locked = Locked | NotLocked
  deriving (Eq,Show)

----------------------------------------

{- | Read a file; return `𝓝` if there is no file to read. -}

----------------------------------------

data FlockPID = Flocked NamedFileLock | NotFlocked (𝕄 PID)

grab_lock ∷ (MonadIO μ, FileAs γ, Printable γ, AsFilePath γ,
             Printable ε, AsIOError ε, AsParseError ε,
             MonadError ε μ, MonadLog (Log ω) μ,
             Default ω, HasIOClass ω, HasDoMock ω) =>
            γ → DoMock → μ NamedFileLock
grab_lock fn do_mock =
  flock Notice Exclusive fn do_mock

grab_lock_nb ∷ (MonadIO μ, FileAs γ, Printable γ, AsFilePath γ,
                Printable ε, AsIOError ε, AsParseError ε,
                MonadError ε μ, MonadLog (Log ω) μ,
                Default ω, HasIOClass ω, HasDoMock ω) =>
               γ → DoMock → μ FlockPID
grab_lock_nb fn do_mock = do
  flockNB Notice Exclusive fn do_mock ≫ \ case
    -- Be sure this is the final action; so that the fl gets returned, and thus
    -- the caller has a chance to unlock it.
    𝓙 fl → return $ Flocked fl
    𝓝    → do
      txt ← readFile @_ @𝕋 Informational
                     (𝓙 $ \ f → [fmt|readFile: '%T'|] f) (return "") fn do_mock
      mpid ← case parsec @PID @ParseError (toString fn) txt of
               𝓛 e   → info' (toText e) ⪼ return 𝓝
               𝓡 pid → return $ 𝓙 pid

      return $ NotFlocked mpid

----------------------------------------

{- | Work through a list of files, trying to flock each (non-blocking) in turn.
     Stop once we have a successful lock; return a list of files tried.
 -}
find_lock_nb ∷ (MonadIO μ,
                Printable ε, AsIOError ε, AsParseError ε,
                MonadError ε μ, MonadLog (Log ω) μ,
                Default ω, HasIOClass ω, HasDoMock ω) =>
               NonEmpty AbsFile → DoMock → μ ([AbsFile], FlockPID)
find_lock_nb fns do_mock = first reverse ⊳ find_lock_nb_ fns [] do_mock

find_lock_nb_ ∷ (MonadIO μ,
                Printable ε, AsIOError ε, AsParseError ε,
                MonadError ε μ, MonadLog (Log ω) μ,
                Default ω, HasIOClass ω, HasDoMock ω) =>
               NonEmpty AbsFile → [AbsFile] → DoMock → μ ([AbsFile], FlockPID)
find_lock_nb_ fns accum do_mock = do
  let (fn,fns') = uncons fns
  grab_lock_nb fn do_mock ≫ \ case
    NotFlocked 𝓝 → do
      notice' $ [fmtT|Failed to flock '%T'|] fn
      case fns' of
        𝓝       → return (fn:accum,NotFlocked 𝓝)
        𝓙 fns'' → find_lock_nb_ fns'' (fn:accum) do_mock

    NotFlocked (𝓙 (PID pid)) → do
      notice' $ [fmtT|Failed to flock '%T': pid <%d> is already queued|] fn pid
      case fns' of
        𝓝       → return (fn:accum,NotFlocked (𝓙 (PID pid)))
        𝓙 fns'' → find_lock_nb_ fns'' (fn:accum) do_mock

    Flocked l → return (accum,Flocked l)

----------------------------------------

{- | Given a list of files, and an existing flock: wait on a flock of each file
     in turn; once we have gained one, release the prior lock (having removed
     our PID from the file).  Return with the final lock. -}
chase_flock ∷ (MonadIO μ,
                Printable ε, AsIOError ε, AsParseError ε,
                MonadError ε μ, MonadLog (Log ω) μ,
                Default ω, HasIOClass ω, HasDoMock ω) =>
               [AbsFile] → NamedFileLock → DoMock → μ NamedFileLock
chase_flock []       l _       = return l
chase_flock (fn:fns) l do_mock = do
  l' ← grab_lock fn do_mock
  unflock Notice l do_mock
  chase_flock fns l' do_mock

----------------------------------------

doWithLock ∷ ∀ ε ω μ .
             (MonadIO μ,
              AsIOError ε, AsParseError ε, Printable ε, MonadError ε μ,
              HasDoMock ω, HasIOClass ω, Default ω, MonadLog (Log ω) μ) ⇒
             𝕄 TimerIO → NonEmpty AbsFile → (NamedFileLock → μ Word8) → DoMock
           → μ Word8
doWithLock tid queue io do_mock = do
  (fns,mpid) ← find_lock_nb queue do_mock

  case mpid of
    NotFlocked _ → return 3
    Flocked l → do
      -- chase the flock along the functions in reverse order
      l' ← chase_flock (reverse fns) l do_mock
      -- we take care to always unflock the lock file
      case tid of
        𝓝   → return ()
        𝓙 t → liftIO $ stopTimer t
      io l'

----------------------------------------

flockProcRun ∷ ∀ ε .
               (HasCallStack, Printable ε, AsUsageError ε, AsParseError ε,
                AsIOError ε, AsProcExitError ε, AsCreateProcError ε,
                AsFPathError ε) ⇒
               𝕄 TimerIO → DoMock → Options
             → LoggingT (Log MockIOClass) (ExceptT ε IO) Word8
flockProcRun tid do_mock opts = do
  queue_absfiles ← mapM (pResolve @AbsFile) (opts ⊣ queue_files)
  let exit_val ∷ ExitInfo → Word8
      exit_val (view exitVal → ExitVal v) = v
      -- this is used to decode the return of doProc; doProc throws on
      -- signal, so we can never hit an ExitSig pattern
      exit_val _ = 255
      io ∷ NamedFileLock → LoggingT (Log MockIOClass) (ExceptT ε IO) Word8
      io l = do
        (x,()) ← doProc Notice CmdW (unflock Notice l do_mock)
                        outDef (ℍ stdin (FileA [absfile|/dev/stdin|]) ReadMode)
                        (opts ⊣ exe,opts ⊣ args) do_mock
        return $ exit_val x
  doWithLock tid queue_absfiles io do_mock

--------------------

{- | Start a kill timer (if indicated by opts); this will kill the main thread
     after some time.  Call `flockProcRun`, which should stop the timer if it
     gains all the locks (before the timer expires.

     We take care to run the remote executable from the main thread: when I
     tried running the executable from a subsidiary thread, I needed to hit
     'return' in ghci to make stuff happen.
-}
myMain ∷ ∀ ε .
         (HasCallStack, Printable ε, AsUsageError ε, AsParseError ε,
          AsIOError ε, AsProcExitError ε, AsCreateProcError ε, AsFPathError ε) ⇒
         DoMock → Options → LoggingT (Log MockIOClass) (ExceptT ε IO) Word8
myMain do_mock opts = do
  mainTID ← liftIO myThreadId
  killTID ∷ 𝕄 TimerIO ← liftIO $ case opts ⊣ timeout of
              𝓝       → return 𝓝
              𝓙 (s,t) → fmap 𝓙 $ flip oneShotTimer t $ do
                progn ← getProgName
                hPutStrLn stderr $ [fmt|%s: timed out after %ss|] progn s
                killThread mainTID
  flockProcRun killTID do_mock opts

----------------------------------------

main ∷ IO ()
main = do
  let progDesc ∷ 𝕋 = "queue executions"
  getArgs ≫ stdMain progDesc parseOptions (myMain @UsageParseFPProcIOError)

-- that's all, folks! ----------------------------------------------------------
