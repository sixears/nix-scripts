{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
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
import System.IO                ( BufferMode( NoBuffering ),
                                  IOMode( ReadWriteMode ),
                                  hClose, hGetLine, hSetBuffering
                                )
import System.Process           ( CreateProcess( std_out ), StdStream( CreatePipe ),
                                  createProcess, proc )

-- bytestring --------------------------

import Data.ByteString.Lazy  qualified as  LBS

-- containers --------------------------

import qualified Data.Map.Strict as Map

-- duration ----------------------------

import Duration  ( Duration( SECS ), asMicroseconds )

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

cacheResponse ∷ Cache → LBS.ByteString → LBS.ByteString

cacheResponse c "lanIPs" = encode (_lanIPs c)
cacheResponse c _ = "Unknown request"

------------------------------------------------------------

data Context = Context { _cache ∷ MVar Cache
                       , _lanCheck ∷ 𝕄 (Async ()) }

lanCheck ∷ Lens' Context (𝕄 (Async()))
lanCheck = lens _lanCheck (\ c l → c { _lanCheck = l })

newLanCheck ∷ Context → IO Context
newLanCheck c = do
  t ← mkTimer (SECS 1) (do lan_ips ← lanIPs
                           modifyMVar_ (_cache c) (\ c' → return $ c' { _lanIPs = lan_ips })
                           )
  return $ c & lanCheck ⊩ t

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

lanWatcher ∷ MVar Context → MVar Cache → IO ()
lanWatcher context cache = do
  -- CR martyn: path
  (_,𝓙 ip_monitor,_,_) ← createProcess ((proc "/run/current-system/sw/bin/ip"
                      [ "-tshort", "monitor", "address" ]) { std_out = CreatePipe })
  forever $ do
    l ← hGetLine ip_monitor
    modifyMVar_ context $ \ c → do
      let currentLanCheck = c ⊣ lanCheck
      case currentLanCheck of
        𝓙 c' →
          poll c' ≫ \ case
            𝓙 _ → newLanCheck c  -- timer has ended
            𝓝   → return c  -- Timer already exists, do nothing
        𝓝 → newLanCheck c -- set a new timer
    putStrLn $ "ip monitor: " ◇ l
  return ()

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
  cache   ← newMVar (Cache   { _lanIPs = lan_ips })
  context ← newMVar (Context { _lanCheck = 𝓝, _cache = cache })

  -- Start cache updater thread
  _ ← forkIO (cacheUpdater cache)

  _ ← forkIO (lanWatcher context cache)

  -- Accept connections loop
  forever $ do
      (conn, addr) ← accept sock
      -- Handle each connection in a separate thread
      forkIO $ handleClient conn cache

-- that's all, folks! ----------------------------------------------------------
