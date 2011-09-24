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
      hSetBuffering handle LineBuffering
      hSetNewlineMode handle (NewlineMode CRLF CRLF)
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
                      IO ThreadId
spawnClientThreads handle userStore roomStore = do
  outgoing   <- atomically $ newTChan
  incoming   <- atomically $ newTChan
  forkIO $ trace "forking dispatcher" $
    loginThread userStore roomStore handle