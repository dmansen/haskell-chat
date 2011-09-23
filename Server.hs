module Server where

import Control.Concurrent
import Control.Concurrent.STM

import Debug.Trace
import Data.Map as M
import Network
import System.IO

import Broadcast
import Message

server :: IO ()
server = withSocketsDo $ do
  userStore <- atomically $ newTVar M.empty
  roomStore <- atomically $ newTVar M.empty
  serverSock <- trace "Listening" $ listenOn (PortNumber 9000)
  waitForClients serverSock userStore roomStore

waitForClients :: Socket ->
                  UserStore ->
                  RoomStore ->
                  IO ()
waitForClients serverSock userStore roomStore = do
  (handle, host, port) <- trace "accepting socket" $ accept serverSock
  spawnClientThreads handle userStore roomStore
  trace "waiting for clients" $ waitForClients serverSock userStore roomStore

spawnClientThreads :: Handle ->
                      UserStore ->
                      RoomStore ->
                      IO (ThreadId, ThreadId, ThreadId)
spawnClientThreads handle userStore roomStore = do
  outgoing <- atomically $ newTChan
  incoming <- atomically $ newTChan
  listener <- forkIO $ trace "forking listener" $ clientListener handle incoming ""
  messenger <- forkIO $ trace "forking message thread" $ clientMessageThread outgoing
  dispatcher <- forkIO $ trace "forking dispatcher" $ dispatcherThread userStore roomStore incoming outgoing
  return (listener, messenger, dispatcher)

clientMessageThread :: TChan (Handle, ClientMessage) ->
                       IO ()
clientMessageThread chan = do
  (to, msg) <- atomically $ readTChan chan
  hPutStrLn to (show msg)
  clientMessageThread chan

clientListener :: Handle ->
                  TChan (Handle, ServerMessage) ->
                  String ->
                  IO ()
clientListener handle incoming current = do
  hSetBuffering handle LineBuffering
  line <- hGetLine handle
  let soFar = current ++ line
  case parseMsg line of
    Just msg -> do
                atomically $ writeTChan incoming (handle, msg)
                trace ("msg: " ++ show msg) $ clientListener handle incoming ""
    Nothing -> clientListener handle incoming soFar