module Server where

import Control.Concurrent
import Control.Concurrent.STM

import Control.Exception as E

import Control.Monad (forever)

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
waitForClients serverSock userStore roomStore =
    (do
      (handle, host, port) <- accept serverSock
      spawnClientThreads handle userStore roomStore
      waitForClients serverSock userStore roomStore)
    `E.catch`
    ((\_ -> waitForClients
            serverSock
            userStore
            roomStore) :: IOException -> IO ())

spawnClientThreads :: Handle ->
                      UserStore ->
                      RoomStore ->
                      IO (ThreadId, ThreadId, ThreadId)
spawnClientThreads handle userStore roomStore = do
  outgoing   <- atomically $ newTChan
  incoming   <- atomically $ newTChan
  listener   <- forkIO $ trace "forking listener" $
                  forever $ clientListener handle incoming
  messenger  <- forkIO $ trace "forking message thread" $
                  forever $ clientMessageThread outgoing
  dispatcher <- forkIO $ trace "forking dispatcher" $
                  loginThread userStore roomStore incoming outgoing
  return (listener, messenger, dispatcher)

clientMessageThread :: TChan (Handle, ClientMessage) ->
                       IO ()
clientMessageThread chan = do
  (to, msg) <- atomically $ readTChan chan
  hPutStrLn to (show msg)

clientListener :: Handle ->
                  TChan (Handle, ServerMessage) ->
                  IO ()
clientListener handle incoming = do
  hSetBuffering handle LineBuffering
  line <- hGetLine handle
  case parseMsg line of
    Just msg -> do
                atomically $ writeTChan incoming (handle, msg)
    Nothing -> return ()