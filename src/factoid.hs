{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE UnicodeSyntax     #-}

import Base1

-- aeson -------------------------------

import Data.Aeson  ( encode )

-- async -------------------------------

import Control.Concurrent.Async  ( Async, async, poll )

-- base --------------------------------

import Control.Concurrent       ( forkIO, threadDelay )
import Control.Concurrent.MVar  ( MVar, readMVar, modifyMVar_, newMVar )
import Control.Monad            ( forever )
import Data.Maybe               ( fromJust )
import Data.String              ( fromString )
import System.IO                ( BufferMode( NoBuffering ), Handle,
                                  IOMode( ReadWriteMode ),
                                  hClose, hGetLine, hPutStrLn, hSetBuffering,
                                  stderr
                                )
import System.Process           ( CreateProcess( std_in, std_out ),
                                  StdStream( CreatePipe, NoStream ),
                                  createProcess, proc, withCreateProcess
                                )

-- bytestring --------------------------

import Data.ByteString.Lazy  qualified as  LBS

-- containers --------------------------

import qualified Data.Map.Strict as Map

-- duration ----------------------------

import Duration  ( Duration( SECS ), asMicroseconds )

-- fpath -------------------------------

import FPath.AbsFile  ( AbsFile, absfile )

-- ip4 ---------------------------------

import IP4  ( IP4 )

-- more-unicode ------------------------

import Data.MoreUnicode.Lens  ( (⊩) )

-- network -----------------------------

import Network.Socket ( Family( AF_INET ), PortNumber, SockAddr( SockAddrInet )
                      , Socket, SocketType( Stream )
                      , accept, bind, listen, socket, socketToHandle )

import NetworkPlus  ( lanIPs )

-- text --------------------------------

import Data.Text     qualified as  T
import Data.Text.IO  qualified as  T_IO

-- time --------------------------------

import Data.Time ( getCurrentTime )

--------------------------------------------------------------------------------

newtype Cache = -- Map.Map LBS.ByteString LBS.ByteString
             Cache { _lanIPs ∷ [IP4] }

newCache ∷ IO (MVar Cache)
newCache = newMVar (Cache { _lanIPs = [] })

----------------------------------------

{-| call lanIPs, update the cache -}
updateLanIPs ∷ MVar Cache → IO ()
updateLanIPs cache = do
  lan_ips ← lanIPs
  modifyMVar_ cache (\ c → return $ c { _lanIPs = lan_ips })

----------------------------------------

cacheResponse ∷ Cache → LBS.ByteString → LBS.ByteString
cacheResponse c "lanIPs" = encode (_lanIPs c)
cacheResponse c _ = "Unknown request"

------------------------------------------------------------

data Context = Context { _cache ∷ MVar Cache
                       , _lanCheck ∷ 𝕄 (Async ()) }

newContext ∷ MVar Cache → IO (MVar Context)
newContext cache = newMVar (Context { _lanCheck = 𝓝, _cache = cache })

----------------------------------------

lanCheck ∷ Lens' Context (𝕄 (Async()))
lanCheck = lens _lanCheck (\ c l → c { _lanCheck = l })


newLanCheck ∷ Context → IO Context
newLanCheck ctxt = do
  t ← mkTimer (SECS 1) (updateLanIPs (_cache ctxt))
  return $ ctxt & lanCheck ⊩ t

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
    currentTime ← getCurrentTime
    sleep (SECS 10)

----------------------------------------

ipPath ∷ AbsFile
ipPath = [absfile|/run/current-system/sw/bin/ip|]

{-| create a proc, close stdin, leave stderr in place, return the stdout pipe -}
procPipe ∷ MonadIO μ ⇒ AbsFile → [𝕋] → μ Handle
procPipe cmd args = liftIO $ do
  (_,𝓙 stdout,_,_) ← createProcess ((proc (toString cmd) (toString ⊳ args)) { std_in = NoStream, std_out = CreatePipe })
  return stdout

lanWatcher ∷ MVar Context → MVar Cache → IO ()
lanWatcher context cache = do
  -- CR martyn: path

  let ip_monitor = ((proc (toString ipPath) [ "-tshort", "monitor", "address" ])
                    { std_out = CreatePipe })
--  (_,𝓙 ipm_out,_,_) ← createProcess ip_monitor
--  ipm_out ← procPipe ipPath [ "-tshort", "monitor", "address" ]
  let process_ip_monitor_lines ∷ Handle → IO ()
      process_ip_monitor_lines ipm_out = forever (do
         l ← hGetLine ipm_out
         modifyMVar_ context $ \ c → do
           let currentLanCheck = c ⊣ lanCheck
           case currentLanCheck of
             𝓙 c' →
               poll c' ≫ \ case
                 𝓙 _ → newLanCheck c  -- timer has ended
                 𝓝   → return c  -- Timer already exists, do nothing
             𝓝 → newLanCheck c -- set a new timer
         putStrLn $ "ip monitor: " ◇ l) ⪼ return ()


  withCreateProcess ip_monitor
    (\ _ (𝓙 ipm_out) _ _ → process_ip_monitor_lines ipm_out)

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
    cache ← readMVar cache
    let response = {- case Map.lookup command cache of
                        𝓙 val → val
                        𝓝     → "Unknown command" -}
          cacheResponse cache command
    LBS.hPutStr handle (response ◇ "\n")
    hClose handle

----------------------------------------

main ∷ IO ()
main = do
  -- Create a socket
  sock ← socket AF_INET Stream 0
  let port ∷ PortNumber = fromIntegral (3000∷ℕ)
  bind sock (SockAddrInet port 0)
  listen sock 5

  -- Initialize cache & context
  lan_ips ← lanIPs
  cache   ← newCache
  updateLanIPs cache
  context ← newContext cache

  -- Start cache updater thread
  _ ← forkIO (cacheUpdater cache)

  _ ← forkIO (lanWatcher context cache)

  -- Accept connections loop
  hPutStrLn stderr $ "listening on port " ◇ (show port)
  forever $ do
      (conn, addr) ← accept sock
      -- Handle each connection in a separate thread
      forkIO $ handleClient conn cache

-- that's all, folks! ----------------------------------------------------------
