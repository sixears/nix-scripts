{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE QuasiQuotes            #-}
{-# LANGUAGE UnicodeSyntax          #-}

-- XXX Thread output (i.e., run it through a single thread)
-- XXX output threadID with output

import Base1
import Prelude  ( error, round )

-- aeson -------------------------------

import Data.Aeson  ( encode )

-- async -------------------------------

import Control.Concurrent.Async  qualified as  Async
import Control.Concurrent.Async  ( Async, async, asyncThreadId, withAsync )

-- base --------------------------------

import Data.Bool                ( bool )
import Control.Concurrent       ( ThreadId, forkIO, isEmptyMVar, myThreadId,
                                  newEmptyMVar, putMVar,takeMVar, threadDelay)
import Control.Concurrent.MVar  ( MVar, readMVar, modifyMVar_, newMVar )
import Control.Exception        ( SomeException, catch, displayException )
import Control.Monad            ( forever )
import Data.String              ( fromString )
import Data.Tuple               ( uncurry )
import System.IO                ( BufferMode( NoBuffering ), Handle,
                                  IOMode( ReadMode, ReadWriteMode ),
                                  hClose, hGetLine, hPutStrLn, hSetBuffering,
                                  openFile, putStrLn, stderr
                                )
import System.Process           ( CreateProcess( std_in, std_out, std_err ),
                                  ProcessHandle,
                                  StdStream( CreatePipe, Inherit, UseHandle ),
                                  getPid, proc, terminateProcess,
                                  withCreateProcess
                                )

-- base-unicode-symbols ----------------

import Prelude.Unicode  ( (≠) )

-- bytestring --------------------------

import Data.ByteString.Lazy  qualified as  LBS

-- containers --------------------------

import qualified Data.Map.Strict as Map

-- duration ----------------------------

import Duration  ( Duration( SECS, MINS ), asMicroseconds )

-- exited ------------------------------

import Exited  ( exitWith )

-- fpath -------------------------------

import FPath.AbsFile  ( AbsFile, absfile )

-- ip4 ---------------------------------

import IP4  ( IP4 )

-- log-plus ----------------------------

import Log  ( Log, logIO', logIOT )

-- logging-effect ----------------------

import Control.Monad.Log  ( LoggingT, Severity( Debug, Warning ) )

-- mockio-plus -------------------------

import MockIO.MockIOClass  ( MockIOClass )

-- monadio-plus ------------------------

import MonadIO.Base  ( getArgs )

-- mtl ---------------------------------

import Control.Monad.Reader  ( MonadReader, ReaderT, runReaderT, ask, asks )

-- network -----------------------------

import Network.Socket qualified

import Network.Socket ( Family( AF_INET ), PortNumber, SockAddr( SockAddrInet )
                      , Socket, SocketType( Stream )
                      , accept, getPeerName, socketToHandle, socketPort
                      )

import NetworkPlus  ( lanIPs, wanIP )

-- optparse-applicative ----------------

import Options.Applicative  ( Parser, auto, help, long, metavar, option, short
                            , showDefault, value )

-- parsec-plus-base --------------------

import Parsec.Error  ( IOParseError )

-- stdmain -----------------------------

import StdMain                       ( stdMainNoDR )
import StdMain.ProcOutputParseError  ( ScriptError )

-- text --------------------------------

import Data.Text     qualified as  T

-- time --------------------------------

-- import Data.Time ( getCurrentTime )

-- unix --------------------------------

import System.Posix.Signals  ( Handler( Catch ), installHandler, keyboardSignal )

--------------------------------------------------------------------------------

warn ∷ MonadIO μ ⇒ 𝕋 → μ ()
warn t = liftIO $ hPutStrLn stderr $ "WARN: " ◇ T.unpack t

debug ∷ MonadIO μ ⇒ 𝕋 → μ ()
debug t = liftIO $ hPutStrLn stderr $ "DEBUG: " ◇ T.unpack t

------------------------------------------------------------

data Cache = -- Map.Map LBS.ByteString LBS.ByteString
             Cache { _lanIPs ∷ [IP4]
                   , _wanIP  ∷ 𝕄 IP4
                   }

newCache ∷ MonadIO μ ⇒ μ (MVar Cache)
newCache = liftIO $ newMVar (Cache { _lanIPs = [], _wanIP = 𝓝 })

----------------------------------------

{-| call lanIPs, update the cache -}
updateLanIPs ∷ Context → IO ()
updateLanIPs ctxt = do
  debug "updateLanIPs"
  let cache = _cache ctxt
  lan_ips ← lanIPs
  -- XXX whenever lan_ips change, call updateWanIP (after 5s) if they're not null
  -- or set wanIP to 𝓝 if they are
-- XXX we need a more sophisticated approach to lanIP vs wanIP:
--     -) when lanIP is set to [], wanIP should be set to none and timer stopped
--     -) whenever lanIP is set to ![], wanIP should be updated, and a timer
--        restarted
  modifyMVar_ cache (\ c → do
                        let prev_ips = _lanIPs c
                        when (lan_ips ≠ prev_ips) -- lan_ips changed
                             (case lan_ips of
                               [] → setNoWanIP ctxt -- no lan, no wan
                               _  → updateWanIPTimer (SECS 5) ctxt) -- new lan, check wan
                        return $ c { _lanIPs = lan_ips }
                    )

----------------------------------------

updateWanIP_ ∷ (MonadIO μ, MonadReader Context μ) ⇒ μ ()
updateWanIP_ = do
  wan_ip ← ѥ $ wanIP
  ctxt ← ask
  liftIO $ modifyMVar_ (_cache ctxt) $ \ c →
    case wan_ip of
      𝓛 (e ∷ IOParseError) → do
        debug "updateWanIP: error: sleeping 1min"
        warn (toText e)
        updateWanIPTimer (MINS 1) ctxt ⪼ return c
      𝓡 ip → do
        debug "updateWanIP: got IP, sleeping 10mins"
        updateWanIPTimer (SECS 10) ctxt ⪼ return (c { _wanIP = ip })


{-| call lanIPs, update the cache -}
-- XXX update wanIP every minute after a lanIP change, until you get a real
-- result; every 10m thereafter
-- XXX single place to update _wanIP; to update _wanIPTimer
-- XXX why the jiggins is this still causing a sleep?
updateWanIP ∷ (MonadIO μ, MonadReader Context μ) ⇒ μ ()
updateWanIP = fireAndForget updateWanIP_ {- do
  ctxt ← ask -- XXX
  cache ← asks _cache
  liftIO $ modifyMVar_ cache $ \ c → flip runReaderT ctxt $ do
{-
  case _lanIPs c of
    [] → return $ c { _wanIP = 𝓝 }
    _  → do
-}
-- XXX forkIO to do this; check that there isn't already a job in flight;
-- XXX unify with other thing to do this (currently in lanWatcher)
-- XXX remove this sleep, which is here purely for diagnostics
      warn'' "sleeping 5s" ⪼ sleep (SECS 5) ⪼ warn'' "slept"
      wan_ip ← ѥ $ wanIP
      case wan_ip of
        𝓛 (e ∷ IOParseError) → do
          debug "updateWanIP: error: sleeping 1min"
          warn (toText e)
          updateWanIPTimer (MINS 1) ctxt ⪼ return c
        𝓡 ip → do
          debug "updateWanIP: got IP, sleeping 10mins"
          updateWanIPTimer (SECS 10) ctxt ⪼ return (c { _wanIP = ip })
-}

----------------------------------------

cacheResponse ∷ Cache → LBS.ByteString → LBS.ByteString
cacheResponse c "lanIPs" = encode $ _lanIPs c
cacheResponse c "wanIP"  = encode $ _wanIP  c
cacheResponse _ _        = "Unknown request"

------------------------------------------------------------

class Pollable α β | α → β where
  poll ∷ α → IO (𝕄 (𝔼 SomeException β))

------------------------------------------------------------

class Cancellable α where
  cancel ∷ α → IO ()
  cancel' ∷ α → IO α
  cancel' a = cancel a ⪼ return a

------------------------------------------------------------

newtype LanCheck = LanCheck (Async ())

instance Pollable LanCheck () where
  poll (LanCheck a) = Async.poll a

instance Cancellable LanCheck where
  cancel (LanCheck w) = Async.cancel w

----------------------------------------

newtype WanCheck = WanCheck (Async ())

instance Cancellable WanCheck where
  cancel (WanCheck w) = Async.cancel w

------------------------------------------------------------

data Context = Context { _cache         ∷ MVar Cache
                       -- it is important to ensure that lanCheck
                       -- is always defined, as there's no way to atomically
                       -- modify it if it's empty
                       , _lanIPsTimer   ∷ MVar LanCheck
                         -- ^ the timer for a lanIPs update
                       , _wanIPTimer    ∷ MVar WanCheck
                         -- ^ the timer for a wanIP update
                       , _childProcs    ∷ MVar (Map.Map 𝕋 ProcessHandle)
                       , _childThreads  ∷ MVar (Map.Map 𝕋 ThreadId)
                       , _outputChannel ∷ MVar (Severity, 𝕋)
                       , _exit          ∷ MVar Word8
                       }

newContext ∷ MonadIO μ ⇒ μ Context
newContext = liftIO $ do
  cache          ← newCache
  exit           ← newEmptyMVar
  lan_check      ← newEmptyMVar
  wan_check      ← WanCheck ⊳ async (return ()) ≫ newMVar
  child_procs    ← newMVar Map.empty
  child_threads  ← newMVar Map.empty
  output_channel ← newEmptyMVar
  let ctxt = Context { _cache         = cache
                     , _lanIPsTimer   = lan_check
                     , _wanIPTimer    = wan_check
                     -- XXX shouldn't these be MVars too?
                     , _childProcs    = child_procs
                     , _childThreads  = child_threads
                     , _exit          = exit
                     , _outputChannel = output_channel
                     }
  lanIPsTimer ctxt ⪼ return ctxt

----------------------------------------

cleanup ∷ Context → IO ()
cleanup ctxt = do
-- XXX use async for ip monitor thread?
-- XXX terminate threads first?

  putStrLn "Cleanup: terminating child processes..."
  -- XXX delete procs as we terminate them, and thus update the MVar
  ps ← readMVar (_childProcs ctxt)
  mapM_ (\ (nm,p) → do
      pid ← getPid p
      warn $ [fmt|terminating %t (%d)|] nm (pid ⧏ 0)
      terminateProcess p
      -- I don't think there's any value in waiting for the process to finish
      -- _ <- waitForProcess ph
      ) (Map.toList ps)
  putStrLn "Cleanup complete."
  putMVar (_exit ctxt) 0

----------------------------------------

lanIPsTimer ∷ Context → IO LanCheck
lanIPsTimer ctxt = {- debug "lanIPsTimer" ⪼ -} LanCheck ⊳ mkTimer (SECS 1) (updateLanIPs ctxt)

----------------------------------------

ensureLanIPsTimer ∷ Context → IO ()
ensureLanIPsTimer ctxt =
  ensureTimerSet (_lanIPsTimer ctxt) lanIPsTimer ctxt

----------------------------------------

wanIPTimer ∷ Duration → Context → IO WanCheck
wanIPTimer dur ctxt = do
  debug "wanIPTimer"
  WanCheck ⊳ mkTimer dur (flip runReaderT ctxt $ updateWanIP)

----------------------------------------

updateWanIPTimer ∷ MonadIO μ ⇒ Duration → Context → μ ()
updateWanIPTimer dur ctxt = liftIO $ do
  debug "updateWanIPTimer"
  setTimer (_wanIPTimer ctxt) (wanIPTimer dur) ctxt

----------------------------------------

cancelWanIPTimer ∷ Context → IO ()
cancelWanIPTimer ctxt = do
  debug "cancelWanIPTimer"
  modifyMVar_ (_wanIPTimer ctxt) cancel'

----------------------------------------

setNoWanIP ∷ Context → IO ()
setNoWanIP ctxt = do
  cancelWanIPTimer ctxt
  modifyMVar_ (_cache ctxt) (\ c → return $ c { _wanIP = 𝓝 })

------------------------------------------------------------

sleep ∷ MonadIO μ ⇒ Duration → μ ()
sleep dur = liftIO $ threadDelay (round $ dur ⊣ asMicroseconds)

----------------------------------------

mkTimer ∷ Duration → IO α → IO (Async α)
mkTimer duration action = do
  -- putStrLn "mkTimer"
  async $ sleep duration ⪼ action

----------------------------------------

-- Function to update cache every 10 seconds
cacheUpdater ∷ MVar Cache → IO ()
cacheUpdater _cache = forever $ do
    -- Simulate cache population
    -- XXX add update time to each factoid
    -- currentTime ← getCurrentTime
    -- updateWanIP cache
    sleep (SECS 10)

----------------------------------------

ipPath ∷ AbsFile
ipPath = [absfile|/run/current-system/sw/bin/ip|]

ipMonitor ∷ CreateProcess
ipMonitor = proc (toString ipPath) [ "-tshort", "monitor", "address" ]

----------------------------------------

{-| create a proc, close stdin, leave stderr in place, use the stdout pipe -}
withStdoutProc ∷ CreateProcess → (Handle → ProcessHandle → IO α) → IO α
withStdoutProc create_proc action = do
  let stdout_proc 𝓝 (𝓙 stdout) 𝓝 p = action stdout p
      stdout_proc sin sout serr _ =
        error $ [fmt|internal error withStdoutProc got «%w» «%w» «%w»|]
                sin sout serr
  devnull ← openFile "/dev/null" ReadMode
  let proc_ = create_proc { std_in = UseHandle devnull
                          , std_out = CreatePipe
                          , std_err = Inherit }
  withCreateProcess proc_ stdout_proc

----------------------------------------

{-| set a timer, nixing any existing timer in place
    a new one.  Note that this will block if the MVar is empty.
-}
setTimer ∷ Cancellable α ⇒ MVar α → (β → IO α) → β → IO ()
setTimer check mk_timer cache = do
  debug "setTimer"
  modifyMVar_ check $ \ c → do
    debug "setTimer (2)"
    cancel c
    mk_timer cache

----------------------------------------

{-| if a timer is running, leave it alone; but if it has terminated, then create
    a new one.  Note that this will block if the MVar is empty.
-}
ensureTimerSet ∷ Pollable α γ ⇒ MVar α → (β → IO α) → β → IO ()
ensureTimerSet check mk_timer cache =
  modifyMVar_ check $ \ l →
  poll l ≫ \ case
    𝓝   → debug "timer still running" ⪼ return l       -- timer is still running
    𝓙 _ → debug "new timer" ⪼ mk_timer cache -- set a new timer

----------------------------------------

{-| create a sub-process watching ip monitor; whenever it emits a line, start
    a timer to check the lan IPs (unless one is already running) -}
lanWatcher ∷ Context → IO ()
lanWatcher ctxt = do
  -- warn "lanWatcher - starting"
  -- warn' ctxt "lanWatcher: starting"
  debug' ctxt "lanwatcher: starting"
  -- warn "lanWatcher - still starting"
  let process_ip_monitor_lines ∷ Handle → IO ()
      process_ip_monitor_lines ipm_out = forever (do
         l ← hGetLine ipm_out
         ensureLanIPsTimer ctxt
         putStrLn $ "ip monitor: " ◇ l) ⪼ return ()

  withStdoutProc ipMonitor
    (\ ipm_out p → do
      debug' ctxt "lanWatcher: created proc"
      modifyMVar_ (_childProcs ctxt) $ return ∘ Map.insert "ip monitor address -tshort" p
      process_ip_monitor_lines ipm_out
      warn "lanWatcher: all done"
    )

----------------------------------------

class HasContext α where
  getContext ∷ α → Context

instance HasContext Context where
  getContext = id

instance HasContext (Context,β) where
  getContext = fst

log' ∷ (MonadIO μ, HasContext θ, MonadReader θ μ) ⇒ Severity → 𝕋 → μ ()
log' sev t = do
  output_channel ← asks (_outputChannel ∘ getContext)
  -- warn $ [fmt|log! %w %w|] sev t
  liftIO ∘ putMVar output_channel ∘ (sev,) $ t
  -- warn "logged"

log ∷ MonadIO μ ⇒ Severity → Context → 𝕋 → μ ()
log sev ctxt t = do
  -- warn $ [fmt|log! %w %w|] sev t
  liftIO ∘ putMVar (_outputChannel ctxt) ∘ (sev,) $ t
  -- warn "logged"

debug' ∷ MonadIO μ ⇒ Context → 𝕋 → μ ()
debug' = log Debug

debug'' ∷ (MonadIO μ, MonadReader Context μ) ⇒ 𝕋 → μ ()
debug'' t = do
  tid ← liftIO myThreadId
  log' Debug ([fmt|«%w»|] tid ◇ t)

warn' ∷ MonadIO μ ⇒ Context → 𝕋 → μ ()
warn' = log Warning

warn'' ∷ (MonadIO μ, HasContext θ, MonadReader θ μ) ⇒ 𝕋 → μ ()
warn'' = log' Warning

-- Function to handle client requests
handleClient ∷ (MonadIO μ, MonadReader Context μ) ⇒ Socket → μ ()
handleClient sock = do
  cache ← asks _cache
  peer_name ← liftIO $ getPeerName sock
  debug'' $ [fmt|new connection: %w|] peer_name
  -- We use Handle for easier IO
  handle ← liftIO $ socketToHandle sock ReadWriteMode
  liftIO $ hSetBuffering handle NoBuffering
  -- Read command
  command ← liftIO $ fromString ⊳ hGetLine handle
  -- Get cache and respond
  response ← liftIO $ readMVar cache ⊲ (flip cacheResponse) command
  liftIO $ LBS.hPutStr handle (response ◇ "\n")
  debug'' $ [fmt|done with connection: %w|] peer_name
  liftIO $ hClose handle

----------------------------------------

data Options = Options { _port ∷ PortNumber }

parseOptions ∷ Parser Options
parseOptions = Options ⊳ option auto (ю [ long "port", short 'p'
                                        , metavar "PORT"
                                        , help "port"
                                        , showDefault
                                        , value 3000
                                        ])

socket ∷ MonadIO μ ⇒ PortNumber → μ Socket
socket port = liftIO $ do
  sock ← Network.Socket.socket AF_INET Stream 0
  Network.Socket.bind sock (SockAddrInet port 0)
  Network.Socket.listen sock 5
  return sock

installCtrlCHandler ∷ MonadIO μ ⇒ Context → μ Handler
installCtrlCHandler ctxt =
  liftIO $ installHandler keyboardSignal (Catch (cleanup ctxt)) 𝓝

readerRunT ∷ β → ReaderT β η α → η α
readerRunT = flip runReaderT

-- U2d4e -- Tifinagh letter yam
ⵎ ∷ ∀ α η γ . (Context, Async γ) → ReaderT (Context, Async γ) η α → η α
ⵎ = readerRunT

catchLog ∷ SomeException → IO ()
catchLog e = hPutStrLn stderr $ "Error in thread: " ◇ displayException e

fireAndForget ∷ (MonadIO μ, MonadReader Context μ) ⇒
                 ReaderT Context IO () → μ ()
fireAndForget io = do
  ctxt ∷ Context ← ask
  liftIO $ async $ catch (runReaderT io ctxt) catchLog
  return ()

acceptor ∷ Socket → Context → IO ThreadId
acceptor sock ctxt = do
  port ← socketPort sock
  -- hPutStrLn stderr $ "listening on port " ◇ (show $ port)

  forkIO $ forever $ do
    (conn, _addr) ← accept sock

    -- handle each connection in a separate thread
    -- fireAndForget (flip runReaderT ctxt $ handleClient conn)
    flip runReaderT ctxt $ fireAndForget (handleClient conn)

-- XXX use logging warn/info/debug
main ∷ IO ()
main = let desc ∷ 𝕋 = "monitor & report some facts to interested callers"
       in  getArgs ≫ stdMainNoDR @ScriptError desc parseOptions go
       where go ∷ Options → LoggingT (Log MockIOClass) (ExceptT ScriptError IO) ()
             go opts = do
               sock ← socket (_port opts)
               ctxt ← newContext
               _ ← installCtrlCHandler ctxt

               -- XXX MonadReader for context
               liftIO $ do
                 -- gentlemen, start your engines
                 -- beware! withAsync waits for its result
                 -- withAsync (cacheUpdater (_cache ctxt)) $ \ _ →
                 async $
                   withAsync (lanWatcher ctxt) $ \ þ → do
                     -- debug' ctxt "debug'"
                     -- warn' ctxt "warn'"
                     -- XXX this is the threadID

                     -- beware! withAsync waits for its result
{-
                     async $
                     -- withAsync (return()) $ \ _ →
                       forever $ do
                         -- isEmptyMVar (_outputChannel ctxt) ≫ warn ∘ [fmt|zzz %t|] ∘ bool "false" "true"
                         readMVar (_outputChannel ctxt) ≫ warn ∘ T.pack ∘ show
                         sleep (SECS 1)
-}
                     runReaderT (warn'' "who's that at the door?") (ctxt,þ)
                     _ ← acceptor sock ctxt
                     warn "accepting (1)"
                     runReaderT (warn'' "accepting (2)") (ctxt,þ)
                     warn "accepting (3)"
--                     logIOT Debug "debug"
--                     _ ← forever $ liftIO (takeMVar (_outputChannel ctxt)) ≫ uncurry logIOT
                     warn "all done"
                     return ()
{-
                     -- Accept connections loop
                     hPutStrLn stderr $ "listening on port " ◇ (show $ _port opts)
                     _ ← forkIO $ forever $ do
                           (conn, _addr) ← accept sock
                           -- Handle each connection in a separate thread
                           forkIO $ handleClient conn (_cache ctxt)

                     _ ← takeMVar (_exit ctxt) ≫ exitWith
                     return ()
-}
               warn "here"
               forever $ liftIO (takeMVar (_outputChannel ctxt)) ≫ uncurry logIOT


-- that's all, folks! ----------------------------------------------------------
