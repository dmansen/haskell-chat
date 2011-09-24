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
    
cleanup :: Handle ->
           ThreadId ->
           ThreadId ->
           IO ()
cleanup handle listener messenger = do
  trace "Closing handle" $ hClose handle
  trace "Killing listener" $ killThread listener
  trace "Killing messenger" $ killThread messenger

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
  let clean = cleanup handle listener messenger
  dispatcher <- forkIO $ trace "forking dispatcher" $
                  loginThread userStore roomStore incoming outgoing clean
  return (listener, messenger, dispatcher)

clientMessageThread :: TChan (Handle, ClientMessage) ->
                       IO ()
clientMessageThread chan = do
  (to, msg) <- atomically $ readTChan chan
  hPutStrLn to (show msg)
  `E.catch`
  ((\_ -> putStrLn "Listener thread killed") :: AsyncException -> IO ())  

clientListener :: Handle ->
                  TChan (Handle, ServerMessage) ->
                  IO ()
clientListener handle incoming = do
  line <- hGetLine handle
  let msg = parseMsg line in do
    putStrLn ("Got msg " ++ line)
    atomically $ writeTChan incoming (handle, msg)
  `E.catch`
  ((\_ -> putStrLn "Listener thread killed") :: AsyncException -> IO ())