module Server (server) where

import Control.Concurrent
import Control.Concurrent.STM

import Control.Exception as E

import Control.Monad (forever)

import Debug.Trace
import Data.Map as M
import Network
import System.IO

import Broadcast
import DataStores
import Message

server :: PortID -> IO ()
server port = withSocketsDo $ do
  userStore <- atomically $ newTVar M.empty
  roomStore <- atomically $ newTVar M.empty
  serverSock <- trace "Server listening" $ listenOn port
  (waitForClients serverSock userStore roomStore
   `finally`
   sClose serverSock)

waitForClients :: Socket ->
                  UserStore ->
                  RoomStore ->
                  IO ()
waitForClients serverSock userStore roomStore = do
  (handle, host, port) <- accept serverSock
  hSetBuffering handle LineBuffering
  hSetNewlineMode handle (NewlineMode CRLF CRLF)
  launchClientThread handle userStore roomStore
  waitForClients serverSock userStore roomStore
  `E.catch`
  listenThreadExceptionHandler (waitForClients serverSock userStore roomStore)

listenThreadExceptionHandler :: IO () -> IOException -> IO ()
listenThreadExceptionHandler continue _ =
  trace "Exception in socket wait thread caught." $ continue

launchClientThread :: Handle ->
                      UserStore ->
                      RoomStore ->
                      IO ThreadId
launchClientThread handle userStore roomStore = do
  forkIO $ trace "Socket accepted, forking dispatcher" $
    loginThreadWrapper userStore roomStore handle