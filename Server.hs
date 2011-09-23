module Server where

import Control.Concurrent
import Control.Concurrent.STM
import Data.Map as M
import Network
import System.IO

import Broadcast
import Message

main = do
  incoming <- atomically $ newTChan
  userStore <- atomically $ newTVar M.empty
  roomStore <- atomically $ newTVar M.empty
  forkIO $ waitForClients userStore roomStore incoming

waitForClients userStore roomStore incoming = withSocketsDo $ do
  serverSock <- listenOn (PortNumber 5)
  (handle, host, port) <- accept serverSock
  waitForClients userStore roomStore incoming

spawnClientThreads handle broadcast userStore roomStore = do
  outgoing <- atomically $ newTChan
  forkIO $ clientListener handle broadcast ""
  forkIO $ clientMessageThread outgoing
  forkIO $ handlerThread userStore roomStore broadcast outgoing
  
clientMessageThread chan = do
  (to, msg) <- atomically $ readTChan chan
  hPutStrLn to (show msg)
  
clientListener handle incoming current = do
  hSetBuffering handle LineBuffering
  line <- hGetLine handle
  let soFar = current ++ line
  case parseMsg line of
    Just msg -> atomically $ writeTChan incoming (handle, msg)
    Nothing -> clientListener handle incoming soFar