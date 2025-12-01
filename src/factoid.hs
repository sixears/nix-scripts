{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE QuasiQuotes            #-}
{-# LANGUAGE UnicodeSyntax          #-}

import Base1
import Prelude  ( round )

-- aeson -------------------------------

import Data.Aeson  ( ToJSON( toJSON ), defaultOptions, encode
                   , fieldLabelModifier, genericToJSON, omitNothingFields )

-- async -------------------------------

import Control.Concurrent.Async  qualified as  Async
import Control.Concurrent.Async  ( Async,
                                   Concurrently( Concurrently, runConcurrently ),
                                   async
                                 )

-- base --------------------------------

import Control.Concurrent       ( ThreadId, forkIO, myThreadId, newEmptyMVar,
                                  putMVar,takeMVar, threadDelay)
import Control.Concurrent.MVar  ( MVar, readMVar, modifyMVar_, newMVar,
                                  tryPutMVar, tryReadMVar, tryTakeMVar )
import Control.Exception        ( SomeException, catch, displayException )
import Control.Monad            ( forever )
import Data.Char                ( isUpper, toLower )
import Data.List                ( dropWhile, isPrefixOf )
import Data.Semigroup           ( sconcat )
import Data.String              ( fromString )
import Data.Tuple               ( uncurry )
import GHC.Generics             ( Generic )
import System.IO                ( BufferMode( NoBuffering ), Handle,
                                  IOMode( ReadMode, ReadWriteMode ),
                                  hClose, hGetLine, hSetBuffering, openFile
                                )
import System.IO.Unsafe         ( unsafePerformIO )
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

import Log  ( Log, logIOT )

-- logging-effect ----------------------

import Control.Monad.Log  ( LoggingT, MonadLog,
                            Severity( Error, Warning, Debug ) )

-- mockio-plus -------------------------

import MockIO.MockIOClass  ( MockIOClass )

-- monadio-plus ------------------------

import MonadIO.Base  ( getArgs )

-- mtl ---------------------------------

import Control.Monad.Reader  ( MonadReader, ReaderT, runReaderT, ask )

-- natural -----------------------------

import Natural.Length     ( len_ )
import Natural.Replicate  ( drop_ )

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

import Data.Text  qualified as  T

-- time --------------------------------

-- XXX add time to all cached values / check time & changed time? access time?
import Data.Time ( UTCTime, getCurrentTime )

-- unix --------------------------------

import System.Posix.Signals  ( Handler( Catch ), installHandler, keyboardSignal )

--------------------------------------------------------------------------------

ipPath ∷ AbsFile
ipPath = [absfile|/run/current-system/sw/bin/ip|]

----------------------------------------
--          UTILITY FUNCTIONS         --
----------------------------------------

ø :: MonadIO μ ⇒ IO α → μ ()
ø io = liftIO io ⪼ return ()

----------------------------------------

sleep ∷ MonadIO μ ⇒ Duration → μ ()
sleep dur = liftIO $ threadDelay (round $ dur ⊣ asMicroseconds)

----------------------------------------

readerRunT ∷ ∀ α β η . β → ReaderT β η α → η α
readerRunT = flip runReaderT

--------------------

-- U2d4e -- Tifinagh letter yam
ⵎ ∷ ∀ α β η . β → ReaderT β η α → η α
ⵎ = readerRunT

----------------------------------------

{-| fire off a number concurrent threads (each a MonadReader) -}
forks ∷ MonadIO μ ⇒ β → NonEmpty (ReaderT β IO ()) → μ ()
forks ctxt =
  ø ∘ async ∘ runConcurrently ∘ sconcat ∘ fmap (Concurrently ∘ readerRunT ctxt)

----------------------------------------

{-| run some IO (that is a ReaderT) in a thread, without ever
    collecting -}
fireAndForget' ∷ (MonadIO μ) ⇒ α → ReaderT α IO () → μ ()
fireAndForget' oc io =
  let displaySomeException = displayException @SomeException
      catchE e = error $ [fmt|Error in thread: %s|] $ displaySomeException e
  in ø $ async $ catch (runReaderT io oc) catchE

--------------------

{-| run some IO (that is a ReaderT) in a thread, without ever
    collecting (suitable for an OutputChannel Reader context) -}
fireAndForget ∷ (MonadIO μ, MonadReader α μ) ⇒ ReaderT α IO () → μ ()
fireAndForget io = ask ≫ \ oc → fireAndForget' oc io

------------------------------------------------------------
--                        Pollable                        --
------------------------------------------------------------

class Pollable α β | α → β where
  poll ∷ α → IO (𝕄 (𝔼 SomeException β))

------------------------------------------------------------
--                       Cancellable                      --
------------------------------------------------------------

class Cancellable α where
  cancel ∷ α → IO ()
  cancel' ∷ α → IO α
  cancel' a = cancel a ⪼ return a

------------------------------------------------------------

{-| create an Async that waits some time before executing an IO -}
mkTimer ∷ Duration → IO α → IO (Async α)
mkTimer duration action = async $ sleep duration ⪼ action

----------------------------------------

{-| Set an MVar, nixing any existing Cancellable in place before replacing it
    with a new one.  Note that this will block if the MVar is empty.
-}
setCancelMVar ∷ (MonadIO μ, Cancellable α)⇒ MVar α → IO α → μ ()
setCancelMVar mv mk = liftIO $ modifyMVar_ mv $ \ c → cancel c ⪼ mk

----------------------------------------

{-| If a Pollable is running, leave it alone; but if it has terminated, then
    create a new one.  Note that this will block if the MVar is empty.

    `update` allows for an update to the MVar value if it polls (and therefore
    a new one isn't created).
-}

ensureMVarSet ∷ (MonadIO μ, Pollable α γ) ⇒ MVar α → IO α → (α → IO α) → μ ()
ensureMVarSet mvar mkMVar update = do
  liftIO (tryReadMVar mvar) ≫ \ case
    𝓝 → ø $ mkMVar ≫ tryPutMVar mvar
    𝓙 _ → liftIO $ modifyMVar_ mvar $ \ l →
            poll l ≫ \ case
              𝓝   → update l -- async not completed
              𝓙 _ → mkMVar -- some result; async has completed: make a new one

------------------------------------------------------------
--                        LanCheck                        --
------------------------------------------------------------

-- the Async() is a possible timer/action (poll to see if it's done).
-- the 𝕄 Duration is "when this timer is done: do another (with that duration"
data LanCheck = LanCheck (Async ())

----------

instance Pollable LanCheck () where poll (LanCheck a) = Async.poll a

----------

instance Cancellable LanCheck where cancel (LanCheck w) = Async.cancel w

------------------------------------------------------------
--                        WanCheck                        --
------------------------------------------------------------

newtype WanCheck = WanCheck (Async ())

----------

instance Cancellable WanCheck where
  cancel (WanCheck w) = Async.cancel w

------------------------------------------------------------
--                       TimeStamped                      --
------------------------------------------------------------

data TimeStamped α = TimeStamped { _lastChecked ∷ 𝕄 UTCTime
                                 , _lastChanged ∷ 𝕄 UTCTime
                                 , _value       ∷ α
                                 }
  deriving (Generic, Show)

----------

instance (ToJSON α, Show α) ⇒ ToJSON (TimeStamped α) where
  -- XXX add a newtype UTC with toJSON to write as seconds (incl. precision)
  --     in addition to human-readable?
  toJSON =
    let hyphenate (c:cs) | isUpper c = '-' : toLower c : hyphenate cs
                         | otherwise = c : hyphenate cs
        hyphenate []                 = []
        fieldLabelModifier = hyphenate ∘ dropWhile (≡'_')
    in  genericToJSON (defaultOptions { omitNothingFields = 𝓣
                                      , fieldLabelModifier = fieldLabelModifier })

--------------------

newTS ∷ α → TimeStamped α
newTS a = TimeStamped 𝓝 𝓝 a

tsValue ∷ TimeStamped α → α
tsValue (TimeStamped _ _ a) = a

tsUpdate ∷ (MonadIO μ, Eq α, Show α) ⇒ TimeStamped α → α → μ (TimeStamped α)
tsUpdate (TimeStamped _ c a) a' = liftIO $ do
  now ← getCurrentTime
  let c' = case c of
             𝓝 → now
             𝓙 ĉ → if a ≡ a' then ĉ else now
  return $ TimeStamped (𝓙 now) (𝓙 c') a'

------------------------------------------------------------
--                          Cache                         --
------------------------------------------------------------

data Cache = Cache { _lanIPs ∷ TimeStamped [IP4]
                   , _wanIP  ∷ TimeStamped (𝕄 IP4)
                   }
  deriving Show

----------

newCache ∷ MonadIO μ ⇒ μ (MVar Cache)
newCache = liftIO $ newMVar (Cache { _lanIPs = newTS [], _wanIP = newTS 𝓝 })

----------------------------------------

cacheLanIPs ∷ Lens' Cache (TimeStamped [IP4])
cacheLanIPs = lens _lanIPs (\ c ip4s → c { _lanIPs = ip4s })

----------------------------------------

updateCacheLanIPs ∷ MonadIO μ ⇒ Cache → [IP4] → μ Cache
updateCacheLanIPs c ip4s = do
  ip4sUp ← tsUpdate (c ⊣ cacheLanIPs) ip4s
  return $ c & cacheLanIPs ⊢ ip4sUp

----------------------------------------

cacheWanIP ∷ Lens' Cache (TimeStamped (𝕄 IP4))
cacheWanIP = lens _wanIP (\ c ip4 → c { _wanIP = ip4 })

----------------------------------------

updateCacheWanIP ∷ MonadIO μ ⇒ Cache → (𝕄 IP4) → μ Cache
updateCacheWanIP c ip4 = do
  ip4Up ← tsUpdate (c ⊣ cacheWanIP) ip4
  return $ c & cacheWanIP ⊢ ip4Up

------------------------------------------------------------
--                     OutputChannel                      --
------------------------------------------------------------

newtype OutputChannel = OutputChannel (MVar (Severity,𝕋))

----------------------------------------

newOutputChannel ∷ MonadIO μ ⇒ μ OutputChannel
newOutputChannel = liftIO $ OutputChannel ⊳ newEmptyMVar

----------------------------------------

output ∷ MonadIO μ ⇒ OutputChannel → Severity → 𝕋 → μ ()
output (OutputChannel mv) sv t = liftIO $ putMVar mv (sv,t)

----------------------------------------

writeOutput ∷ (MonadIO μ, MonadLog (Log ω) μ, Default ω) ⇒ OutputChannel → μ ()
writeOutput (OutputChannel mv) = liftIO (takeMVar mv) ≫ uncurry logIOT

----------------------------------------

globalOutput ∷ OutputChannel
globalOutput = unsafePerformIO $ newOutputChannel

----------------------------------------

log ∷ MonadIO μ ⇒ Severity → 𝕋 → μ ()
log sev t = liftIO $ do
  let removePrefix ∷ 𝕊 → 𝕊 → 𝕊
      removePrefix prefix str
        | prefix `isPrefixOf` str = drop_ (len_ prefix) str
        | otherwise = str
  tid ← (removePrefix "ThreadId " ∘ show) ⊳ myThreadId
  output globalOutput sev $ [fmt|«%05s» |] tid ◇ t

--------------------

error ∷ MonadIO μ ⇒ 𝕋 → μ ()
error = log Error

--------------------

warn ∷ MonadIO μ ⇒ 𝕋 → μ ()
warn = log Warning

--------------------

debug ∷ MonadIO μ ⇒ 𝕋 → μ ()
debug = log Debug


------------------------------------------------------------
--                        Context                         --
------------------------------------------------------------

data Context = Context { _cache         ∷ MVar Cache
                       -- it is important to ensure that lanCheck
                       -- is always defined, as there's no way to atomically
                       -- modify it if it's empty
                       , _lanIPsTimer   ∷ MVar LanCheck
                         -- ^ the timer for a lanIPs update
                       , _lanCheckQueue ∷ MVar 𝔹
                         -- ^ if set, queue another lan check after the one
                         --   is executed
                       , _wanIPTimer    ∷ MVar WanCheck
                         -- ^ the timer for a wanIP update
                       , _childProcs    ∷ MVar (Map.Map 𝕋 ProcessHandle)
                       , _childThreads  ∷ MVar (Map.Map 𝕋 ThreadId)
                       , _outputChannel ∷ OutputChannel
                       , _exit          ∷ MVar Word8
                       }

--------------------

newContext ∷ MonadIO μ ⇒ μ Context
newContext = liftIO $ do
  cache           ← newCache
  exit            ← newEmptyMVar
  lan_check       ← newEmptyMVar
  wan_check       ← WanCheck ⊳ async (return ()) ≫ newMVar
  child_procs     ← newMVar Map.empty
  child_threads   ← newMVar Map.empty
  lan_check_queue ← newMVar 𝓕
--  output_channel ← newOutputChannel
  let ctxt = Context { _cache         = cache
                     , _lanIPsTimer   = lan_check
                     , _lanCheckQueue = lan_check_queue
                     , _wanIPTimer    = wan_check
                     , _childProcs    = child_procs
                     , _childThreads  = child_threads
                     , _exit          = exit
                     , _outputChannel = globalOutput -- output_channel
                     }
  lanIPsTimer ctxt ⪼ return ctxt

----------------------------------------

cacheResponse ∷ MonadIO μ ⇒ Context → LBS.ByteString → μ LBS.ByteString
cacheResponse ctxt "updateLanIPs" = liftIO $ updateLanIPs ctxt ⪼ return "OK"
cacheResponse ctxt "updateWanIP"  = liftIO $ updateWanIP  ctxt ⪼ return "OK"
-- XXX factor out the encoding to a common core
cacheResponse ctxt "lanIPs" = encode ∘ _lanIPs ⊳ liftIO (readMVar (_cache ctxt))
cacheResponse ctxt "wanIP"  = encode ∘ _wanIP ⊳ liftIO (readMVar (_cache ctxt))
cacheResponse _ _        = return "Unknown request"

----------------------------------------

cleanup ∷ Context → IO ()
cleanup ctxt = do
-- XXX use async for ip monitor thread?
-- XXX terminate threads first?

  debug "Cleanup: terminating child processes..."
  -- XXX delete procs as we terminate them, and thus update the MVar
  ps ← readMVar (_childProcs ctxt)
  mapM_ (\ (nm,p) → do
      pid ← getPid p
      warn $ [fmt|terminating %t (%d)|] nm (pid ⧏ 0)
      terminateProcess p
      -- I don't think there's any value in waiting for the process to finish
      -- _ <- waitForProcess ph
      ) (Map.toList ps)
  putMVar (_exit ctxt) 0
  debug "cleanup complete: exiting"

------------------------------------------------------------

-------------------- Process Utilities ---------------------

{-| create a proc, close stdin, leave stderr in place, use the stdout pipe -}
withStdoutProc ∷ CreateProcess → (Handle → ProcessHandle → IO ()) → IO ()
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

--------------------- LAN IPs Handling ---------------------

-- XXX check the timing in effect
{-| call lanIPs, update the cache; if there's a change, set a timer to update
    the WanIP (unless there's no LanIPs: in which case, set the WanIP to 𝓝 -}
updateLanIPs ∷ Context → IO ()
updateLanIPs ctxt = readerRunT ctxt $ do
  lan_ips ← lanIPs
  liftIO $ modifyMVar_ (_cache ctxt)
           (\ c → do
              let prev_ips = tsValue $ c ⊣ cacheLanIPs
              when (lan_ips ≠ prev_ips) -- lan_ips changed
                   (do debug $ [fmt|updateLanIPs: %w|] lan_ips
                       case lan_ips of
                         []  → ø $ setNoWanIP ctxt c          -- no lan: no wan
                         _xs → updateWanIPTimer (SECS 5) ctxt)-- new lan: upd wan
              updateCacheLanIPs c lan_ips
           )
  liftIO $ modifyMVar_ (_lanCheckQueue ctxt) $ \ case
    𝓕 → return 𝓕
    𝓣 → readerRunT ctxt ensureLanIPsTimer ⪼ return 𝓕



----------------------------------------

lanIPsTimer ∷ Context → IO LanCheck
lanIPsTimer ctxt =
  LanCheck ⊳ mkTimer (SECS 1) (updateLanIPs ctxt)

----------------------------------------

ensureLanIPsTimer ∷ (MonadIO μ, MonadReader Context μ) ⇒ μ ()
ensureLanIPsTimer = do
  ctxt ← ask
  -- XXX this SECS 1 should be unified with other SECS 1
  -- XXX use a LanCheck function rather than re-constructing it
  ensureMVarSet (_lanIPsTimer ctxt) (lanIPsTimer ctxt)
                (\ l → do -- if there's already a check pending, signal to run
                          -- another straight after
                    modifyMVar_ (_lanCheckQueue ctxt) (const $ return 𝓣)
                    return l)

----------------------------------------

{-| create a sub-process watching ip monitor; whenever it emits a line, start
    a timer to check the lan IPs (unless one is already running) -}
lanWatcher ∷ (MonadIO μ, MonadReader Context μ) ⇒ μ ()
lanWatcher = do
  ctxt ← ask
  debug "lanwatcher: starting"
  let process_ip_monitor_lines ∷ Handle → IO ()
      process_ip_monitor_lines ipm_out = forever $ ø (ⵎ ctxt $ do
         l ← liftIO $ hGetLine ipm_out
         debug $ [fmt|ip monitor: %s|] l
         ensureLanIPsTimer)

  let ipArgs    = [ "-tshort", "monitor", "address" ]
      ipMonitor = proc (toString ipPath) ipArgs

  liftIO $ withStdoutProc ipMonitor
    (\ ipm_out p → do
      debug "lanWatcher: created proc"
      -- XXX ip seems to be inheriting the listener port!
      let procName = T.intercalate " " $ "ip" : (T.pack ⊳ ipArgs)
      modifyMVar_ (_childProcs ctxt) $ return ∘ Map.insert procName p
      process_ip_monitor_lines ipm_out
      warn "lanWatcher: ¡finished!"
    )

--------------------- WAN IP Handling ----------------------

{-| call lanIPs, update the cache -}
-- XXX update wanIP every minute after a lanIP change, until you get a real
-- result; every 10m thereafter
-- XXX single place to update _wanIP; to update _wanIPTimer
updateWanIP ∷ MonadIO μ ⇒ Context → μ ()
updateWanIP ctxt =
  fireAndForget' ctxt updateWanIP_
  where
    updateWanIP_ ∷ (MonadIO μ, MonadReader Context μ) ⇒ μ ()
    updateWanIP_ = do
      wan_ip ← ѥ $ wanIP
      liftIO $ modifyMVar_ (_cache ctxt) $ \ c →
        case wan_ip of
          𝓛 (e ∷ IOParseError) → do
            debug "updateWanIP: error: sleeping 1min"
            warn (toText e)
            updateWanIPTimer (MINS 1) ctxt ⪼ return c
          𝓡 ip → do
            debug "updateWanIP: got IP, sleeping 10mins"
            updateWanIPTimer (MINS 10) ctxt
            debug $ [fmt|updateWanIP: returning %w|] ip
            updateCacheWanIP c ip

----------------------------------------

wanIPTimer ∷ Duration → Context → IO WanCheck
wanIPTimer dur ctxt = do
  debug $ [fmt|wanIPTimer %T|] dur
  WanCheck ⊳ mkTimer dur (updateWanIP ctxt)

----------------------------------------

updateWanIPTimer ∷ MonadIO μ ⇒ Duration → Context → μ ()
updateWanIPTimer dur ctxt = liftIO $ do
  debug "updateWanIPTimer"
  setCancelMVar (_wanIPTimer ctxt) (wanIPTimer dur ctxt)

----------------------------------------

cancelWanIPTimer ∷ Context → IO ()
cancelWanIPTimer ctxt = do
  debug "cancelWanIPTimer"
  modifyMVar_ (_wanIPTimer ctxt) cancel'

----------------------------------------

-- we take a Cache here in addition to the Context because the caller
-- should already have grabbed the lock on the cache (i.e., within a modifyMVar_
-- or similar
setNoWanIP ∷ Context → Cache → IO Cache
setNoWanIP ctxt cache = do
  cancelWanIPTimer ctxt
  updateCacheWanIP cache 𝓝

----------------------------------------

-- Function to update cache every 10 seconds
{-
cacheUpdater ∷ MVar Cache → IO ()
cacheUpdater _cache = forever $ do
    -- Simulate cache population
    -- XXX add update time to each factoid
    -- currentTime ← getCurrentTime
    -- updateWanIP cache
    sleep (SECS 10)
-}

----------------- General Client Handling ------------------

handleClient ∷ (MonadIO μ, MonadReader Context μ) ⇒ Socket → μ ()
handleClient sock = do
  peer_name ← liftIO $ getPeerName sock
  debug $ [fmt|new connection: %w|] peer_name
  handle ← liftIO $ socketToHandle sock ReadWriteMode
  liftIO $ hSetBuffering handle NoBuffering
  command ← liftIO $ fromString ⊳ hGetLine handle
  ctxt ← ask
  response ← liftIO $ cacheResponse ctxt command
  liftIO $ LBS.hPutStr handle (response ◇ "\n")
  liftIO $ hClose handle

--------------------- Options Handling ---------------------

data Options = Options { _port ∷ PortNumber }

--------------------

parseOptions ∷ Parser Options
parseOptions = Options ⊳ option auto (ю [ long "port", short 'p'
                                        , metavar "PORT"
                                        , help "port"
                                        , showDefault
                                        , value 3000
                                        ])

--------------------------- main ---------------------------

socket ∷ MonadIO μ ⇒ PortNumber → μ Socket
socket port = liftIO $ do
  sock ← Network.Socket.socket AF_INET Stream 0
  Network.Socket.bind sock (SockAddrInet port 0)
  Network.Socket.listen sock 5
  return sock

----------------------------------------

acceptor ∷ (MonadIO μ, MonadReader Context μ) ⇒ Socket → μ ThreadId
acceptor sock = do
  ctxt ← ask
  port ← liftIO $ socketPort sock
  warn $ [fmt|listening on port %d|] port

  liftIO $ forkIO $ forever $ do
    (conn, _addr) ← accept sock

    -- handle each connection in a separate thread
    flip runReaderT ctxt $ fireAndForget (handleClient conn)

----------------------------------------

installCtrlCHandler ∷ MonadIO μ ⇒ Context → μ Handler
installCtrlCHandler ctxt =
  liftIO $ installHandler keyboardSignal (Catch (cleanup ctxt)) 𝓝

-- XXX use logging warn/info/debug
main ∷ IO ()
main = let desc ∷ 𝕋 = "monitor & report some facts to interested callers"
       in  getArgs ≫ stdMainNoDR @ScriptError desc parseOptions go
       where go ∷ Options → LoggingT (Log MockIOClass) (ExceptT ScriptError IO) ()
             go opts = do
               sock ← socket (_port opts)
               ctxt ← newContext
               -- XXX add handler for sigTERM
               _ ← installCtrlCHandler ctxt

               forks ctxt $ lanWatcher :| [ ø ∘ ⵎ ctxt $ acceptor sock ]
               -- this has to run at the top level, to benefit from the StdMain
               -- log instance
               forever $ do
                 writeOutput (_outputChannel ctxt)
                 -- liftIO (takeMVar (_outputChannel ctxt)) ≫ uncurry logIOT
                 liftIO (tryTakeMVar (_exit ctxt)) ≫ \ case
                   𝓝 → return ()
                   𝓙 e → ø $ exitWith e


-- that's all, folks! ----------------------------------------------------------
