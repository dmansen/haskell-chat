module Server (server) where

import Control.Concurrent
import Control.Concurrent.STM

import Control.Exception as E

import Control.Monad (forever)

import Debug.Trace
import Data.Map as M
import Network
import System.IO

import Dispatch
import DataStores
import Message

server :: PortID -> IO ()
server port = withSocketsDo $ do
  userStore <- atomically $ newTVar M.empty
  roomStore <- atomically $ newTVar M.empty
  serverSock <- listenOn port
  (waitForClientsWrapper serverSock userStore roomStore
   `finally`
   sClose serverSock)

waitForClientsWrapper :: Socket ->
                         UserStore ->
                         RoomStore ->
                         IO ()
waitForClientsWrapper serverSock userStore roomStore = do
  waitForClients serverSock userStore roomStore
  `E.catch`
  listenThreadExceptionHandler (waitForClients serverSock userStore roomStore)

waitForClients :: Socket ->
                  UserStore ->
                  RoomStore ->
                  IO ()
waitForClients serverSock userStore roomStore = forever $ do
  (handle, _, _) <- accept serverSock
  hSetBuffering handle LineBuffering
  hSetNewlineMode handle (NewlineMode CRLF CRLF)
  forkIO $ trace "Socket accepted, forking dispatcher" $
    loginThreadWrapper userStore roomStore handle

listenThreadExceptionHandler :: IO () -> IOException -> IO ()
listenThreadExceptionHandler continue e = continue