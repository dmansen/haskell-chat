module Server where

import Control.Concurrent
import Control.Concurrent.STM

import Debug.Trace
import Data.Map as M
import Network
import System.IO

import Broadcast
import Message

server = withSocketsDo $ do
  userStore <- atomically $ newTVar M.empty
  roomStore <- atomically $ newTVar M.empty
  serverSock <- trace "Listening" $ listenOn (PortNumber 9000)
  waitForClients serverSock userStore roomStore

waitForClients serverSock userStore roomStore = do
  (handle, host, port) <- trace "accepting socket" $ accept serverSock
  spawnClientThreads handle userStore roomStore
  trace "waiting for clients" $ waitForClients serverSock userStore roomStore

spawnClientThreads handle userStore roomStore = do
  outgoing <- atomically $ newTChan
  incoming <- atomically $ newTChan
  forkIO $ trace "forking listener" $ clientListener handle incoming ""
  forkIO $ trace "forking message thread" $ clientMessageThread outgoing
  forkIO $ trace "forking handler" $ handlerThread userStore roomStore incoming outgoing
  
clientMessageThread chan = do
  (to, msg) <- atomically $ readTChan chan
  hPutStrLn to (show msg)
  clientMessageThread chan
  
clientListener handle incoming current = do
  hSetBuffering handle LineBuffering
  line <- hGetLine handle
  let soFar = current ++ line
  case parseMsg line of
    Just msg -> do
                atomically $ writeTChan incoming (handle, msg)
                trace ("msg: " ++ show msg) $ clientListener handle incoming ""
    Nothing -> clientListener handle incoming soFar