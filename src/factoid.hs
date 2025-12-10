{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE LambdaCase             #-}
{-# LANGUAGE NoImplicitPrelude      #-}
{-# LANGUAGE OverloadedStrings      #-}
{-# LANGUAGE QuasiQuotes            #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE UnicodeSyntax          #-}

import Base1
import Prelude  ( round )

-- aeson -------------------------------

import Data.Aeson  ( ToJSON( toJSON ), (.=), defaultOptions, encode,
                     fieldLabelModifier, genericToJSON, object,
                     omitNothingFields
                   )

-- async -------------------------------

import Control.Concurrent.Async  qualified as  Async
import Control.Concurrent.Async  ( Async, async, asyncThreadId )

-- base --------------------------------

import qualified System.Timeout

import Control.Concurrent       ( ThreadId, myThreadId,newEmptyMVar,threadDelay )
import Control.Concurrent.Chan  ( Chan, getChanContents, newChan, writeChan )
import Control.Concurrent.MVar  ( MVar, readMVar, modifyMVar_, newMVar,
                                  tryPutMVar, tryReadMVar )
import Control.Exception        ( SomeException, catch, displayException )
import Control.Monad            ( forever )
import Data.Char                ( isUpper, toLower )
import Data.List                ( dropWhile, isPrefixOf )
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
                                  getPid, proc, terminateProcess, waitForProcess,
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

import Exited  ( ToExitCode, exitWith )

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

import Data.Time              ( UTCTime, getCurrentTime )
import Data.Time.Clock.POSIX  ( utcTimeToPOSIXSeconds )

-- unix --------------------------------

import System.Posix.Signals  ( Handler( Catch ), installHandler, keyboardSignal )

--------------------------------------------------------------------------------

ipPath ∷ AbsFile
ipPath = [absfile|/run/current-system/sw/bin/ip|]

----------------------------------------

instance Ord κ ⇒ HasIndex (Map.Map κ ν) where
  type Indexer (Map.Map κ ν) = κ
  type Elem (Map.Map κ ν) = ν
  index = Map.lookup

----------------------------------------
--          UTILITY FUNCTIONS         --
----------------------------------------

ø :: MonadIO μ ⇒ IO α → μ ()
ø io = liftIO io ⪼ return ()

----------------------------------------

sleep ∷ MonadIO μ ⇒ Duration → μ ()
sleep dur = liftIO $ threadDelay (round $ dur ⊣ asMicroseconds)

----------------------------------------

timeout ∷ Duration → IO α → IO (𝕄 α)
timeout dur = System.Timeout.timeout (round $ dur ⊣ asMicroseconds)

----------------------------------------

readerRunT ∷ ∀ α β η . β → ReaderT β η α → η α
readerRunT = flip runReaderT

--------------------

-- U2d4e -- Tifinagh letter yam
ⵎ ∷ ∀ α β η . β → ReaderT β η α → η α
ⵎ = readerRunT

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
--                        AsyncTool                       --
------------------------------------------------------------

data AsyncTool β = AsyncTool { _name     ∷ 𝕋
                             , _threadID ∷ ThreadId
                             , _cancel   ∷ () → IO ()
                             , _poll     ∷ () → IO (𝕄 (𝔼 SomeException β))
                             }

----------

instance Cancellable (AsyncTool β) where
  cancel ast = _cancel ast $ ()

----------

instance Pollable (AsyncTool β) β where
  poll ast = (_poll ast) ()

--------------------

mkAsyncTool ∷ 𝕋 → Async β → AsyncTool β
mkAsyncTool nm ast = AsyncTool { _name     = nm
                               , _threadID = asyncThreadId ast
                               , _cancel   = const$ Async.cancel ast
                               , _poll     = const $ Async.poll ast
                               }

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
              𝓙 _ → mkMVar   -- some result; async has completed: make a new one

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
--                         GMTime                         --
------------------------------------------------------------

newtype GMTime = GMTime UTCTime
  deriving (Eq,Show)

instance ToJSON GMTime where
  toJSON (GMTime u) =
    let s = utcTimeToPOSIXSeconds u
    in  object [ "epoch-seconds"  .= s
               , "human-readable" .= [fmtT|%.3z|] u]

------------------------------------------------------------
--                       TimeStamped                      --
------------------------------------------------------------

data TimeStamped α = TimeStamped { _lastChecked ∷ 𝕄 GMTime
                                 , _lastChanged ∷ 𝕄 GMTime
                                 , _value       ∷ α
                                 }
  deriving (Generic, Show)

----------

instance (ToJSON α, Show α) ⇒ ToJSON (TimeStamped α) where
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
  now ← GMTime ⊳ getCurrentTime
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

----------------------------------------

globalOutput ∷ Chan (Severity,𝕋,𝕄 ExitCode)
globalOutput = unsafePerformIO $ newChan

----------------------------------------

threadID ∷ ThreadId → 𝕋
threadID tid =
  let removePrefix ∷ 𝕊 → 𝕊 → 𝕊
      removePrefix prefix str
        | prefix `isPrefixOf` str = drop_ (len_ prefix) str
        | otherwise = str
  in  [fmt|«%05s»|] (removePrefix "ThreadId " $ show tid)

----------------------------------------

log ∷ MonadIO μ ⇒ Severity → 𝕋 → μ ()
log sev t = liftIO $ do
  tid ← threadID ⊳ myThreadId
  writeChan globalOutput (sev, [fmt|%t %t|] tid t, 𝓝)

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
                       , _childThreads  ∷ MVar (Map.Map ThreadId (AsyncTool ()))
                       , _exit          ∷ MVar Word8
                       }

--------------------

addChildThread ∷ MonadIO μ ⇒ Context → 𝕋 → ReaderT Context IO () → μ ()
addChildThread ctxt nm io = liftIO $ do
  ast ← mkAsyncTool nm ⊳ async (readerRunT ctxt io)
  modifyMVar_ (_childThreads ctxt) (return ∘ Map.insert (_threadID ast) ast)

childThreads ∷ MonadIO μ ⇒ Context → μ [AsyncTool ()]
childThreads ctxt = liftIO $ Map.elems ⊳ readMVar (_childThreads ctxt)

childThreadName ∷ MonadIO μ ⇒ Context → ThreadId → μ (𝕄 𝕋)
childThreadName ctxt tid = liftIO $ _name⊳⊳(tid !) ⊳ readMVar(_childThreads ctxt)

newContext ∷ MonadIO μ ⇒ μ Context
newContext = liftIO $ do
  cache           ← newCache
  exit            ← newEmptyMVar
  lan_check       ← newEmptyMVar
  wan_check       ← WanCheck ⊳ async (return ()) ≫ newMVar
  child_procs     ← newMVar Map.empty
  child_threads   ← newMVar Map.empty
  lan_check_queue ← newMVar 𝓕
  let ctxt = Context { _cache         = cache
                     , _lanIPsTimer   = lan_check
                     , _lanCheckQueue = lan_check_queue
                     , _wanIPTimer    = wan_check
                     , _childProcs    = child_procs
                     , _childThreads  = child_threads
                     , _exit          = exit
                     }
  lanIPsTimer ctxt ⪼ return ctxt

----------------------------------------

cacheResponse ∷ MonadIO μ ⇒ Context → LBS.ByteString → μ LBS.ByteString
cacheResponse ctxt msg =
  let update f = liftIO $ f ctxt ⪼ return "OK"
      fromCache ∷ (MonadIO μ, ToJSON α) ⇒ (Cache → α)  → μ LBS.ByteString
      fromCache f = encode ∘ f ⊳ liftIO (readMVar (_cache ctxt))
  in  case msg of
        "updateLanIPs" → update updateLanIPs
        "updateWanIP"  → update updateWanIP
        "lanIPs"       → fromCache _lanIPs
        "wanIP"        → fromCache _wanIP
        _              → return "Unknown request"

----------------------------------------

{-| fire off a number concurrent threads (each a MonadReader); log each as a
    childThread in the Context -}
forks ∷ MonadIO μ ⇒ Context → NonEmpty (𝕋, ReaderT Context IO ()) → μ ()
forks ctxt = liftIO ∘ mapM_ (uncurry $ addChildThread ctxt)

----------------------------------------

catchAll ∷ MonadIO μ ⇒ Context → IO () → μ ()
catchAll ctxt io =
    let displaySomeException = displayException @SomeException
        catchE e = do
          tid ← myThreadId
          tnm ← childThreadName ctxt tid
          let tid' = threadID tid
              tnm' = tnm ⧏ "UNKNOWN"
              tex  = displaySomeException e
          error $ [fmt|Error in thread: %t (%t): %s|] tid' tnm' tex
    in  liftIO $ catch io catchE

----------------------------------------

{-| run some IO (that is a ReaderT) in a thread, without ever
    collecting -}
fireAndForget' ∷ MonadIO μ ⇒ Context → 𝕋 → ReaderT Context IO () → μ ()
fireAndForget' ctxt nm io =
  addChildThread ctxt nm $ catchAll ctxt (runReaderT io ctxt)

--------------------

{-| run some IO (that is a ReaderT) in a thread, without ever
    collecting (suitable for an OutputChannel Reader context) -}
fireAndForget ∷ (MonadIO μ,MonadReader Context μ)⇒𝕋 → ReaderT Context IO ()→μ ()
fireAndForget nm io = ask ≫ \ ctxt → fireAndForget' ctxt nm io

----------------------------------------

cleanup ∷ Context → IO ()
cleanup ctxt = do
-- XXX terminate threads

  debug "Cleanup: terminating child processes..."
  -- XXX delete procs as we terminate them, and thus update the MVar
  ps ← readMVar (_childProcs ctxt)
  debug $ [fmt|child procs: %d|] (len_ ps)
  forM_  (Map.toList ps) (\ (nm,p) → do
      pid ← getPid p
      warn $ [fmt|terminating %t (%d)|] nm (pid ⧏ 0)
      terminateProcess p
      let dur = SECS 1
      timeout dur (waitForProcess p) ≫ \ case
        𝓝   → warn $ [fmt|process %t (%d) did not terminate within %T|]
                      nm (pid ⧏ 0) dur
        𝓙 x → warn $ [fmt|process %t (%d) exited %w|] nm (pid ⧏ 0) x
    )
  ts ← childThreads ctxt
  forM_ ts (\ ast → do
               let nm = [fmt|thread: %t %t|] (_name ast) (threadID $ _threadID ast)
               poll ast ≫ \ case
                 𝓙 _ → debug $ [fmt|ignoring closed thread: %t|] nm
                 𝓝   → debug ([fmt|closing thread: %t|] nm) ⪼ cancel ast
               return ()
           )
  -- XXX 0 if instructed with "quit" command
  writeChan globalOutput (Warning, "cleanup complete: exiting",
                          𝓙 $ ExitFailure 255)

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
    (\ ipm_out p → catchAll ctxt $ do
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
  fireAndForget' ctxt "updateWanIP" updateWanIP_
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

  liftIO $ forever $ do
    (conn, addr) ← accept sock

    -- handle each connection in a separate thread
    let nm = [fmt|handle client %w|] addr
    flip runReaderT ctxt $ fireAndForget nm (handleClient conn)

----------------------------------------

installCtrlCHandler ∷ MonadIO μ ⇒ Context → μ Handler
installCtrlCHandler ctxt =
  liftIO $ installHandler keyboardSignal (Catch (cleanup ctxt)) 𝓝

logAndMaybeQuit ∷ (MonadIO μ, Default ω, MonadLog (Log ω) μ, ToExitCode ξ) ⇒
                  (Severity, 𝕋, 𝕄 ξ) → μ ()
logAndMaybeQuit (sev,msg,ext) =
  case ext of
    𝓝    → logIOT sev msg
    𝓙 xt → logIOT sev msg ⪼ ø (exitWith xt)

-- XXX use logging warn/info/debug
main ∷ IO ()
main = let desc ∷ 𝕋 = "monitor & report some facts to interested callers"
       in  getArgs ≫ stdMainNoDR @ScriptError desc parseOptions go
       where go ∷ Options → LoggingT (Log MockIOClass)(ExceptT ScriptError IO)()
             go opts = do
               sock ← socket (_port opts)
               ctxt ← newContext
               -- XXX add handler for sigTERM
               _ ← installCtrlCHandler ctxt

               forks ctxt $ ("lan watcher", lanWatcher) :|
                            [ ("client acceptor", ø ∘ ⵎ ctxt $ acceptor sock) ]
               -- this has to run at the top level, to benefit from the StdMain
               -- log instance
               liftIO (getChanContents globalOutput) ≫ mapM_ logAndMaybeQuit

-- that's all, folks! ----------------------------------------------------------
