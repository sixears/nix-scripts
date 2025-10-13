{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UnicodeSyntax     #-}


import Base1


-- base --------------------------------

import Control.Concurrent       ( forkIO, threadDelay )
import Control.Concurrent.MVar  ( MVar, readMVar, modifyMVar_, newMVar )
import Control.Monad            ( forever )
import System.IO                ( BufferMode( NoBuffering ),
                                  IOMode( ReadWriteMode ),
                                  hClose, hSetBuffering
                                )

-- containers --------------------------

import qualified Data.Map.Strict as Map

-- duration ----------------------------

import Duration  ( Duration( SECS ), asMicroseconds )

-- network -----------------------------

import Network.Socket ( Family( AF_INET ), PortNumber, SockAddr( SockAddrInet )
                      , Socket, SocketType( Stream )
                      , accept, bind, listen, socket, socketToHandle )

-- text --------------------------------

import Data.Text     qualified as  T
import Data.Text.IO  qualified as  T_IO

-- time --------------------------------

import Data.Time ( getCurrentTime )

--------------------------------------------------------------------------------

type Cache = Map.Map 𝕋 𝕋

------------------------------------------------------------

sleep ∷ MonadIO μ ⇒ Duration → μ ()
sleep dur = liftIO $ threadDelay (round $ dur ⊣ asMicroseconds)

----------------------------------------

-- Function to update cache every 10 seconds
cacheUpdater :: MVar Cache → IO ()
cacheUpdater cache_var = forever $ do
    -- Simulate cache population
    currentTime ← getCurrentTime
    let newCache = Map.fromList [ ("timestamp", T.pack $ show currentTime)
                                , ("message", "Hello from cache!")]
    modifyMVar_ cache_var (\ _ → return newCache)
    sleep (10 SECS)

----------------------------------------

-- Function to handle client requests
handleClient :: Socket → MVar Cache → IO ()
handleClient sock cache_var = do
    -- We use Handle for easier IO
    handle ← socketToHandle sock ReadWriteMode
    hSetBuffering handle NoBuffering
    -- Read command
    command ← T_IO.hGetLine handle
    -- Get cache and respond
    cache ← readMVar cache_var
    let response = case Map.lookup command cache of
                        𝓙 val → val
                        𝓝     → "Unknown command"
    T_IO.hPutStrLn handle response
    hClose handle

----------------------------------------

main :: IO ()
main = do
    -- Create a socket
    sock ← socket AF_INET Stream 0
    let port ∷ PortNumber = fromIntegral (3000∷ℕ)
    bind sock (SockAddrInet port 0)
    listen sock 5

    -- Initialize cache
    cache_var ← newMVar Map.empty

    -- Start cache updater thread
    _ ← forkIO (cacheUpdater cache_var)

    -- Accept connections loop
    forever $ do
        (conn, addr) ← accept sock
        -- Handle each connection in a separate thread
        forkIO $ handleClient conn cache_var

-- that's all, folks! ----------------------------------------------------------
