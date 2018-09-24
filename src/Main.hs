module Main where

import Network.Socket

import Control.Exception (bracket)
import Logger

resolve :: HostName -> ServiceName -> IO AddrInfo
resolve address port = do
    let hints = defaultHints {
          addrFlags = [AI_PASSIVE]
        , addrSocketType = Stream
        }
    addr:_ <- getAddrInfo (Just hints) (Just address) (Just port)
    return addr

open :: AddrInfo -> IO Socket
open addr = do
    sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
    -- Useful during testing, to avoid "already bound address" errors
    setSocketOption sock ReuseAddr 1
    bind sock (addrAddress addr)
    listen sock 10
    return sock


-- Accept new connections
-- For now accept only one connection and don't fork
acceptLoop :: Socket -> IO ()
acceptLoop sock = do
    (conn, peer) <- accept sock
    logInfo $ "Connection from: " ++ show peer
    serverMain conn


main :: IO ()
main = withSocketsDo $ do
    bindAddress <- resolve "127.0.0.1" "143"
    bracket (open bindAddress) close acceptLoop



-- Server logic
serverMain :: Socket -> IO ()
serverMain conn = do
    return ()

