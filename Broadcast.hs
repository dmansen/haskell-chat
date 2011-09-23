module Broadcast where

import Control.Concurrent.STM
import Data.Map as M
import Debug.Trace
import System.IO

import Message

-- This module is responsible for broadcasting messages
-- to the proper users.

data Room = Room {
  roomName :: String,
  users :: [User]
} deriving (Show, Eq)

data User = User {
  userName :: String,
  handle :: Handle,
  rooms :: [Room]
} deriving (Show, Eq)

class StringKey a where
  stringKey :: a -> String
  
instance StringKey User where
  stringKey = userName

instance StringKey Room where
  stringKey = roomName

makeUser name handle = User { userName = name, handle = handle, rooms = [] }
makeRoom name = Room { roomName = name, users = [] }

type UserStore = TVar (Map String User)
type RoomStore = TVar (Map String Room)

sendMessage :: User -> ClientMessage -> IO ()
sendMessage to msg = do
  let hdl = handle to
  hPutStr hdl ((show msg) ++ "\r\n")
  return ()
  
handlerThread :: UserStore -> 
                 RoomStore -> 
                 TChan (Handle, ServerMessage) ->
                 TChan (Handle, ClientMessage) ->
                 IO ()
handlerThread users rooms incoming outgoing = do
  let continuation = handlerThread users rooms incoming outgoing in do
    (handle, msg) <- atomically $ readTChan incoming
    print ("Got message: " ++ show msg)
    (responseMsg, cont) <- do
      case msg of
        Login name -> trace ("Login: " ++ name) $ login users name handle continuation
        SPrivateMessage from to msg -> privateMessage users from to msg continuation outgoing
        SRoomMessage from room msg -> roomMessage rooms from room msg continuation outgoing
        Join name room -> trace ("Joining") joinRoom users rooms name room continuation
        Part name room -> partRoom users rooms name room continuation
        Logout name -> logout users rooms name
        Invalid -> return (Error "Invalid Command", continuation)
    atomically $ writeTChan outgoing (handle, responseMsg)
    trace "continuing" cont
  
login :: UserStore -> 
         String -> 
         Handle -> 
         IO () ->
         IO (ClientMessage, IO ())
login users name handle cont = atomically $ do
      user <- maybeGrabFromSTM users name
      case user of
        Just u ->
          return (Error "Username already in use", return ())
        Nothing -> do
          updateSTM users (makeUser name handle)
          return (Ok, cont)
          
logout :: UserStore ->
          RoomStore ->
          String ->
          IO (ClientMessage, IO ())
logout userStore roomStore name = atomically $ do
  maybeUser <- maybeGrabFromSTM userStore name
  userMap <- readTVar userStore
  writeTVar userStore (M.delete name userMap)
  return (Ok, trace "handler dying" $ removeUserFromRooms maybeUser userStore roomStore)

removeUserFromRooms :: Maybe User -> UserStore -> RoomStore -> IO ()
removeUserFromRooms maybeUser userStore roomStore =
  return ()

privateMessage :: UserStore -> 
                  String -> 
                  String -> 
                  String -> 
                  IO () ->
                  TChan (Handle, ClientMessage) ->
                  IO (ClientMessage, IO ())
privateMessage userStore fromName toName msg cont chan = do
  maybeUser <- atomically $ maybeGrabFromSTM userStore toName
  case maybeUser of
    Just toUser -> return (Ok, sendPrivateMessage toUser fromName msg chan >> cont)
    Nothing -> return (Error "User is not logged in", return ())
    
sendPrivateMessage :: User -> String -> String -> TChan (Handle, ClientMessage) -> IO ()
sendPrivateMessage to fromName msg chan = atomically $
  writeTChan chan (handle to, CPrivateMessage fromName msg)

sendRoomMessage :: Room -> String -> String -> TChan (Handle, ClientMessage) -> IO ()
sendRoomMessage room from msg chan = atomically $
  (sequence (Prelude.map (\u -> writeTChan chan (handle u, CRoomMessage from (roomName room) msg)) (Prelude.filter (\u -> userName u /= from) (users room)))) >>
  return ()
    
roomMessage :: RoomStore -> 
               String -> 
               String -> 
               String -> 
               IO () ->
               TChan (Handle, ClientMessage) ->
               IO (ClientMessage, IO ())
roomMessage roomStore fromName toRoom msg cont chan = do
  maybeRoom <- atomically $ maybeGrabFromSTM roomStore toRoom
  case maybeRoom of
    Just room -> return (Ok, sendRoomMessage room fromName msg chan >> cont)
    Nothing -> return (Error "Room does not exist", return ())
    
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
        return (Error "Somehow, you don't seem to be logged in. Disconnecting.", return ())
  
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
        let newUser = (user { rooms = Prelude.filter 
                                        (\r -> roomName r /= roomName room) 
                                        (rooms user) 
                            })
        let newRoom = (room { users = Prelude.filter 
                                        (\u -> userName u /= userName user) 
                                        (users room) 
                            })
        updateSTM userStore newUser
        updateSTM roomStore newRoom
        return (Ok, cont)
      Nothing ->
        return (Error "Somehow, you don't seem to be logged in. Disconnecting.", return ())

updateSTM :: (StringKey a) => TVar (Map String a) -> a -> STM ()
updateSTM store a = do
  map <- readTVar store
  let newMap = M.insert (stringKey a) a map
  writeTVar store newMap
  
createRoomIfNeeded :: RoomStore ->
                      String ->
                      STM Room
createRoomIfNeeded roomStore name = do
  roomStoreMap <- readTVar roomStore
  case M.lookup name roomStoreMap of
    Just existing -> return existing
    Nothing -> 
      do
        let newRoom = makeRoom name
        let newMap = M.insert (roomName newRoom) newRoom roomStoreMap
        writeTVar roomStore newMap
        return newRoom
  
maybeGrabFromSTM :: TVar (Map String a) -> String -> STM (Maybe a)
maybeGrabFromSTM mapVar name = do
  map <- readTVar mapVar
  case M.lookup name map of
    Just a -> return (return a)
    Nothing -> return Nothing