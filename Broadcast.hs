module Broadcast where

import Control.Concurrent.STM
import Control.Concurrent
import Control.Exception
import Data.Map as M hiding (filter, map)
import Debug.Trace
import System.IO

import Prelude hiding (catch)

import DataStores
import Message

-- This module is responsible for broadcasting messages
-- to the proper users.

sendMessages :: [((Handle, MVar ()), ClientMessage)] -> IO ThreadId
sendMessages = forkIO . foldr (>>) (return ()) . map safePutMsg

safePutMsg :: ((Handle, MVar ()), ClientMessage) -> IO ()
safePutMsg ((handle, lock), msg) = do
  takeMVar lock
  hPutStrLn handle (show msg)
  putMVar lock ()

readMessage :: Handle -> IO ServerMessage
readMessage handle = do
  line <- hGetLine handle
  let msg = parseMsg line in return msg
  
loginThreadWrapper userStore roomStore handle =
  loginThread userStore roomStore handle
  `finally`
  loginExceptionHandler handle

loginThread :: UserStore ->
               RoomStore ->
               Handle ->
               IO ()
loginThread users rooms handle = do
  let repeat = loginThread users rooms handle in do
    msg <- readMessage handle
    (responseMsg, cont) <- do
      case msg of
        Login name -> do
          user <- tryLogin users name handle (return ())
          case user of
            Just u ->
              return (Ok, trace (name ++ " logged in") $ dispatcherThreadWrapper u users rooms handle)
            Nothing ->
              return (Error "Username already in use", repeat)
        otherwise ->
          return (Error "Not logged in", repeat)
    hPutStrLn handle (show responseMsg)
    cont

loginExceptionHandler :: Handle -> IO ()
loginExceptionHandler handle = trace "Doing final cleanup." $ hClose handle

dispatcherThreadWrapper user userStore roomStore handle =
  dispatcherThread user userStore roomStore handle
  `finally`
  dispatcherExceptionHandler user userStore roomStore handle

dispatcherThread :: User ->
                    UserStore ->
                    RoomStore ->
                    Handle ->
                    IO ()
dispatcherThread user users rooms handle = do
  let repeat = dispatcherThread user users rooms handle in do
    msg <- readMessage handle
    (responseMsg, cont) <- do
      let name = (userName user)
      case msg of
        Login _ ->
          return (Error "Already logged in", repeat)
        SPrivateMessage to msg ->
          privateMessage users name to msg repeat
        SRoomMessage room msg ->
          roomMessage rooms name room msg repeat
        Join room ->
          joinRoom users rooms name room repeat
        Part room ->
          partRoom users rooms name room repeat
        Logout ->
          logout users rooms name
        Invalid err ->
          return (Error ("Invalid Command: " ++ err), repeat)
    sendMessages [(connection user, responseMsg)]
    cont

dispatcherExceptionHandler :: User ->
                              UserStore ->
                              RoomStore ->
                              Handle ->
                              IO ()
dispatcherExceptionHandler user users rooms handle = do
  logout users rooms (userName user)
  trace ("Thread for " ++ (userName user) ++ " dying.") $ return ()

tryLogin :: UserStore ->
            String ->
            Handle ->
            IO () ->
            IO (Maybe User)
tryLogin users name handle cont = do
  newLock <- newMVar ()
  atomically $ do
    user <- maybeGrabFromSTM users name
    case user of
      Just u -> return Nothing
      Nothing -> do
        let newUser = makeUser name handle newLock
        updateSTM users newUser
        return (Just newUser)

logout :: UserStore ->
          RoomStore ->
          String ->
          IO (ClientMessage, IO ())
logout userStore roomStore name = atomically $ do
  maybeUser <- maybeGrabFromSTM userStore name
  userMap <- readTVar userStore
  writeTVar userStore (M.delete name userMap)
  return (Ok,
          (atomically $
            removeUserFromRooms maybeUser userStore roomStore) >> (
         trace (name ++ " has left") $ return ()))

privateMessage :: UserStore ->
                  String ->
                  String ->
                  String ->
                  IO () ->
                  IO (ClientMessage, IO ())
privateMessage userStore fromName toName msg cont = do
  maybeUser <- atomically $
               maybeGrabFromSTM userStore toName
  case maybeUser of
    Just toUser -> return (Ok,
                            (sendMessages [(buildPrivateMessage toUser fromName msg)]) >>
                           cont)
    Nothing -> return (Error "User is not logged in", cont)

roomMessage :: RoomStore ->
               String ->
               String ->
               String ->
               IO () ->
               IO (ClientMessage, IO ())
roomMessage roomStore fromName toRoom msg cont = do
  maybeRoom <- atomically $ maybeGrabFromSTM roomStore toRoom
  case maybeRoom of
    Just room -> return (Ok,
                          (sendMessages (buildRoomMessages room fromName msg)) >>
                         cont)
    Nothing -> return (Error "Room does not exist", cont)

joinRoom :: UserStore ->
            RoomStore ->
            String ->
            String ->
            IO () ->
            IO (ClientMessage, IO ())
joinRoom userStore roomStore userName roomName cont = do
  atomically $ do
    maybeUser <- maybeGrabFromSTM userStore userName
    case maybeUser of
      Just user -> do
        room <- createRoomIfNeeded roomStore roomName
        let newUser = (user { rooms = room : (rooms user) } )
        let newRoom = (room { users = user : (users room) } )
        updateSTM userStore newUser
        updateSTM roomStore newRoom
        return (Ok, cont)
      Nothing -> -- this is a bizarre situation
        return (Error "Somehow, you don't seem to be logged in. This is a serious error.", cont)

partRoom :: UserStore ->
            RoomStore ->
            String ->
            String ->
            IO () ->
            IO (ClientMessage, IO ())
partRoom userStore roomStore uName rName cont = do
  atomically $ do
    maybeUser <- maybeGrabFromSTM userStore uName
    case maybeUser of
      Just user -> do
        room <- createRoomIfNeeded roomStore rName
        let newUser = (user { rooms = filter
                                        (\r -> roomName r /= roomName room)
                                        (rooms user)
                            })
        let newRoom = (room { users = filter
                                        (\u -> userName u /= userName user)
                                        (users room)
                            })
        updateSTM userStore newUser
        updateSTM roomStore newRoom
        return (Ok, cont)
      Nothing ->
        return (Error "Somehow, you don't seem to be logged in. This is a serious error.", cont)

buildPrivateMessage :: User ->
                       String ->
                       String ->
                       ((Handle, MVar ()), ClientMessage)
buildPrivateMessage to fromName msg =
  (connection to, CPrivateMessage fromName msg)

buildRoomMessages :: Room ->
                     String ->
                     String ->
                     [((Handle , MVar ()), ClientMessage)]
buildRoomMessages room from msg =
  map (\u -> (connection u, CRoomMessage from (roomName room) msg))
  (filter (\u -> userName u /= from) (users room))