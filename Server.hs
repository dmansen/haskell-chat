module Server where

import Control.Concurrent
import Control.Concurrent.STM

import Network
import System.IO

import Broadcast

main = do
  incoming <- atomically $ newTChan
  outgoing <- atomically $ newTChan
  userStore <- atomically $ newTVar
  roomStore <- atomically $ newTVar
  forkIO $ clientMessageThread incoming outgoing
  forkIO $ waitForClients userStore roomStore incoming outgoing

waitForClients userStore roomStore incoming outgoing = withSocketsDo $ do
  serverSock <- listenOn (PortNumber 5)
  (handle, host, port) <- accept serverSock
  forkIO $ clientListener handle incoming outgoing ""
  
clientMessageThread chan = do
  (to, msg) <- readTChan chan
  hPutStrLn to (show msg)
  
clientListener handle incoming current = do
  hSetBuffering handle LineBuffering
  line <- hGetLine handle
  let soFar = current ++ line
  case parseMsg line of
    Just msg -> atomically $ writeTChan (handle, parseMsg soFar)
    Nothing -> clientListener handle incoming soFar