{-# LANGUAGE OverloadedStrings #-}

import Network.Socket
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import Data.Time
import System.IO

import qualified Data.Map.Strict as Map

type Cache = Map.Map String String

main :: IO ()
main = do
    -- Create a socket
    sock <- socket AF_INET Stream 0
    let port = 3000
    bind sock (SockAddrInet (fromIntegral port) 0)
    listen sock 5
    putStrLn $ "Server listening on port " ++ show port

    -- Initialize cache
    cacheVar <- newMVar Map.empty

    -- Start cache updater thread
    _ <- forkIO (cacheUpdater cacheVar)

    -- Accept connections loop
    forever $ do
        (conn, addr) <- accept sock
        putStrLn $ "Accepted connection from " ++ show addr
        -- Handle each connection in a separate thread
        forkIO $ handleClient conn cacheVar

-- Function to update cache every 10 seconds
cacheUpdater :: MVar Cache -> IO ()
cacheUpdater cacheVar = forever $ do
    -- Simulate cache population
    currentTime <- getCurrentTime
    let newCache = Map.fromList [("timestamp", show currentTime), ("message", "Hello from cache!")]
    modifyMVar_ cacheVar (\_ -> return newCache)
    putStrLn "Cache updated."
    threadDelay (10 * 1000000) -- 10 seconds

-- Function to handle client requests
handleClient :: Socket -> MVar Cache -> IO ()
handleClient sock cacheVar = do
    -- We use Handle for easier IO
    handle <- socketToHandle sock ReadWriteMode
    hSetBuffering handle NoBuffering
    -- Read command
    command <- C.hGetLine handle
    putStrLn $ "Received command: " ++ C.unpack command
    -- Get cache and respond
    cache <- readMVar cacheVar
    let response = case Map.lookup (C.unpack command) cache of
                        Just val -> val
                        Nothing  -> "Unknown command"
    C.hPutStrLn handle (C.pack response)
    hClose handle
