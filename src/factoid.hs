{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE UnicodeSyntax     #-}

import Base1
import Prelude  ( error, round )

-- aeson -------------------------------

import Data.Aeson  ( encode )

-- async -------------------------------

import Control.Concurrent.Async  ( Async, async, poll, withAsync )

-- base --------------------------------

import Control.Concurrent       ( ThreadId, forkIO, newEmptyMVar, putMVar,
                                  takeMVar, threadDelay, tryPutMVar, tryReadMVar)
import Control.Concurrent.MVar  ( MVar, readMVar, modifyMVar_, newMVar )
import Control.Monad            ( forever )
import Data.String              ( fromString )
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

-- bytestring --------------------------

import Data.ByteString.Lazy  qualified as  LBS

-- containers --------------------------

import qualified Data.Map.Strict as Map

-- duration ----------------------------

import Duration  ( Duration( SECS ), asMicroseconds )

-- exited ------------------------------

import Exited  ( exitWith )

-- fpath -------------------------------

import FPath.AbsFile  ( AbsFile, absfile )

-- ip4 ---------------------------------

import IP4  ( IP4 )

-- monadio-plus ------------------------

import MonadIO.Base  ( getArgs )

-- network -----------------------------

import Network.Socket ( Family( AF_INET ), PortNumber, SockAddr( SockAddrInet )
                      , Socket, SocketType( Stream )
                      , accept, bind, listen, socket, socketToHandle )

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

------------------------------------------------------------

data Cache = -- Map.Map LBS.ByteString LBS.ByteString
             Cache { _lanIPs ∷ [IP4]
                   , _wanIP  ∷ 𝕄 IP4
                   }

newCache ∷ IO (MVar Cache)
newCache = newMVar (Cache { _lanIPs = [], _wanIP = 𝓝 })

----------------------------------------

{-| call lanIPs, update the cache -}
updateLanIPs ∷ MVar Cache → IO ()
updateLanIPs cache = do
  lan_ips ← lanIPs
  -- XXX whenever lan_ips change, call updateWanIP (after 5s) if they're not null
  -- or set wanIP to 𝓝 if they are
  modifyMVar_ cache (\ c → return $ c { _lanIPs = lan_ips })

----------------------------------------

{-| call lanIPs, update the cache -}
-- XXX update wanIP every minute after a lanIP change, until you get a real
-- result; every 10m thereafter
-- XXX we need a more sophisticated approach to lanIP vs wanIP:
--     -) when lanIP is set to [], wanIP should be set to none and timer stopped
--     -) whenever lanIP is set to ![], wanIP should be updated, and a timer
--        restarted
updateWanIP ∷ MVar Cache → IO ()
updateWanIP cache = modifyMVar_ cache $ \ c → do
  case _lanIPs c of
    [] → return $ c { _wanIP = 𝓝 }
    _  → do
-- XXX forkIO to do this; check that there isn't already a job in flight;
-- XXX unify with other thing to do this (currently in lanWatcher)
-- XXX remove this sleep, which is here purely for diagnostics
      warn "sleeping" ⪼ sleep (SECS 5) ⪼ warn "slept"
      wan_ip ← ѥ $ wanIP
      case wan_ip of
        𝓛 (e ∷ IOParseError) → warn (toText e) ⪼ return c
        𝓡 ip → return $ c { _wanIP = ip }

----------------------------------------

cacheResponse ∷ Cache → LBS.ByteString → LBS.ByteString
cacheResponse c "lanIPs" = encode $ _lanIPs c
cacheResponse c "wanIP"  = encode $ _wanIP  c
cacheResponse _ _        = "Unknown request"

------------------------------------------------------------

data Context = Context { _cache        ∷ MVar Cache
                       , _lanCheck     ∷ MVar (Async ())
                       , _childProcs   ∷ MVar (Map.Map 𝕋 ProcessHandle)
                       , _childThreads ∷ MVar (Map.Map 𝕋 ThreadId)
                       , _exit         ∷ MVar Word8
                       }

newContext ∷ MVar Cache → IO Context
newContext cache = do
  exit          ← newEmptyMVar
  lan_check     ← newEmptyMVar
  child_procs   ← newMVar Map.empty
  child_threads ← newMVar Map.empty
  return $ Context { _cache        = cache
                   , _lanCheck     = lan_check
                   -- XXX shouldn't these be MVars too?
                   , _childProcs   = child_procs
                   , _childThreads = child_threads
                   , _exit         = exit
                   }

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

lanCheck ∷ Lens' Context (MVar (Async()))
lanCheck = lens _lanCheck (\ c l → c { _lanCheck = l })

----------------------------------------

newLanCheck ∷ Context → IO ()
newLanCheck ctxt = do
  t ← mkTimer (SECS 1) (updateLanIPs (_cache ctxt))
  _ ← tryPutMVar (_lanCheck ctxt) t
  return ()
--  return $ ctxt & lanCheck ⊩ t

------------------------------------------------------------

sleep ∷ MonadIO μ ⇒ Duration → μ ()
sleep dur = liftIO $ threadDelay (round $ dur ⊣ asMicroseconds)

----------------------------------------

mkTimer ∷ Duration → IO α → IO (Async α)
mkTimer duration action = do
  putStrLn "mkTimer"
  async $ sleep duration ⪼ action

----------------------------------------

-- Function to update cache every 10 seconds
cacheUpdater ∷ MVar Cache → IO ()
cacheUpdater cache = forever $ do
    -- Simulate cache population
    -- XXX add update time to each factoid
    -- currentTime ← getCurrentTime
    updateWanIP cache
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

{-| create a sub-process watching ip monitor; whenever it emits a line, start
    a timer to check the lan IPs (unless one is already running) -}
lanWatcher ∷ Context → IO ()
lanWatcher ctxt = do
  warn "lanWatcher: starting"
  let process_ip_monitor_lines ∷ Handle → IO ()
      process_ip_monitor_lines ipm_out = forever (do
         l ← hGetLine ipm_out
         tryReadMVar (ctxt ⊣ lanCheck) ≫ \ case
           𝓝 → newLanCheck ctxt -- set a new timer
           𝓙 c →
             poll c ≫ \ case
               𝓝   → return ()         -- timer already exists, do nothing
               𝓙 _ → newLanCheck ctxt  -- timer has ended
         putStrLn $ "ip monitor: " ◇ l) ⪼ return ()

  withStdoutProc ipMonitor
    (\ ipm_out p → do
      warn "lanWatcher: created proc"
      modifyMVar_ (_childProcs ctxt) $ return ∘ Map.insert "ip monitor address -tshort" p
      process_ip_monitor_lines ipm_out
      warn "lanWatcher: all done"
    )

----------------------------------------

-- Function to handle client requests
handleClient ∷ Socket → MVar Cache → IO ()
handleClient sock cache = do
    -- We use Handle for easier IO
    handle ← socketToHandle sock ReadWriteMode
    hSetBuffering handle NoBuffering
    -- Read command
    command ← fromString ⊳ hGetLine handle
    -- Get cache and respond
    response ← readMVar cache ⊲ (flip cacheResponse) command
    LBS.hPutStr handle (response ◇ "\n")
    hClose handle

----------------------------------------

data Options = Options { _port ∷ PortNumber }

parseOptions ∷ Parser Options
parseOptions = Options ⊳ option auto (ю [ long "port", short 'p'
                                        , metavar "PORT"
                                        , help "port"
                                        , showDefault
                                        , value 3000
                                        ])

-- XXX use logging warn/info/debug
main ∷ IO ()
main = let desc ∷ 𝕋 = "monitor & report some facts to interested callers"
       in  getArgs ≫ stdMainNoDR @ScriptError desc parseOptions go
       where go opts = liftIO $ do
               -- Create a socket
               sock ← socket AF_INET Stream 0
               bind sock (SockAddrInet (_port opts) 0)
               listen sock 5

               -- Initialize cache & context
               cache   ← newCache
               ctxt    ← newContext cache
               updateLanIPs cache

               -- install cleanup handler
               _ ← installHandler keyboardSignal (Catch (cleanup ctxt)) 𝓝
               -- gentlemen, start your engines
--               _ ← forkIO (cacheUpdater cache)
             --  _ ← forkIO (lanWatcher ctxt cache)
             --  _ ← withAsync (return ()) $ \ _ → (lanWatcher ctxt cache)
               withAsync (cacheUpdater cache) $ \ _ →
                 withAsync (lanWatcher ctxt) $ \ _ → do
                   -- Accept connections loop
                   hPutStrLn stderr $ "listening on port " ◇ (show $ _port opts)
                   _ ← forkIO $ forever $ do
                         (conn, _addr) ← accept sock
                         -- Handle each connection in a separate thread
                         forkIO $ handleClient conn cache

                   _ ← takeMVar (_exit ctxt) ≫ exitWith
                   return ()


-- that's all, folks! ----------------------------------------------------------
