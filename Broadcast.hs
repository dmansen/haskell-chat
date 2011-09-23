module Broadcast where

import Data.Map as M
import Handle

import Message

-- This module is responsible for broadcasting messages
-- to the proper users.

data Room {
  name :: String
  users :: [User]
} deriving (Show, Eq)

data User {
  name :: String,
  handle :: Handle,
  rooms :: [Room]
} deriving (Show, Eq)

makeUser name handle = User { name = name, handle = handle }
makeRoom name = Room { name = name, users = [] }

type UserStore = TVar (Map String (TVar User))
type RoomStore = RoomStore

sendMessage :: User -> ClientMessage -> IO ()
sendMessage to msg = do
  let hdl = handle to
  hPutStr handle (msg ++ "\r\n")
  return ()
  
handlerThread :: UserStore -> 
                 RoomStore -> 
                 TChan (Handle, ServerMessage) ->
                 TChan (Handle, ClientMessage)
                 IO ()
handlerThread users rooms incoming outgoing = do
  (handle, msg) <- atomically $ readTChan msgQueue
  (responseActions, cont) <- atomically $
    case msg of
      Login name -> login users name handle
      PrivateMessage from to msg -> privateMessage users from to msg
      RoomMessage from room msg -> roomMessage rooms from room msg
      Join name room -> joinRoom users rooms name room
      Part name room -> partRoom users rooms name room
      Logout name -> logout name
      Invalid -> Error "Invalid Command"
  atomically $ putTVar (handle, responseMsg)
  cont
  
login :: UserStore -> 
         String -> 
         Handle -> 
         IO (ClientMessage, IO ()))
login users name handle = atomically $ do
      user <- maybeGetUser users name
      case user of
        Just u ->
          (Error "Username already in use", return ())
        Nothing -> do
          updateUser users (makeUser name handle)
          return (Ok, return ())
          
privateMessage :: UserStore -> 
                  String -> 
                  String -> 
                  String -> 
                  IO (ClientMessage, IO ())
privateMessage userStore fromName toName msg = do
  maybeUser <- atomically $ maybeGetUser userStore toName
  case maybeUser of
    Just toUser -> return (Ok, sendPrivateMessage toUser fromName msg)
    Nothing -> (Error "User is not logged in", return ())
    
roomMessage :: RoomStore -> 
               UserStore ->
               String -> 
               String -> 
               String -> 
               IO (ClientMessage, IO ())
roomMessage roomStore fromName toRoom msg = do
  maybeRoom <- atomically $ maybeGetRoom roomStore toRoom
  case maybeRoom of
    Just room -> return (Ok, sendRoomMessage room fromName msg)
    Nothing -> return (Error "Room does not exist", return ())
    
joinRoom :: UserStore ->
            RoomStore ->
            String ->
            String ->
            IO (ClientMessage, IO ())
joinRoom userStore roomStore userName roomName = do
  atomically $ do
    case maybeGetUser userStore userName of
      Just user -> do
        room <- createRoomIfNeeded roomStore roomName
        updateUser userStore (user { rooms = room : (rooms user) } )
        updateRoom roomStore (room { users = user : (users room) } )
        return (Ok, return ())
      Nothing -> -- this is a bizarre situation
        return (Error "Somehow, you don't seem to be logged in", return ())
  
partRoom :: UserStore ->
            RoomStore ->
            String ->
            String ->
            IO (ClientMessage, IO ())
partRoom userStore roomStore userName roomName = do
  atomically $ do
    case maybeGetUser of
      Just user -> do
        room <- createRoomIfNeeded roomStore roomName
        updateUser userStore (user { rooms = filter (/= room) (rooms user) } )
        updateRoom roomStore (room { users = filter (/= user) (users room) } )

updateUser :: UserStore -> 
              User -> 
              STM ()
updateUser users user = do
  userVar <- newTVar user
  userStoreMap <- readTVar users
  let newMap = M.insert (name user) userVar userStoreMap
  writeTVar userStoreMap newMap
  
updateRoom :: RoomStore -> 
              Room -> 
              STM ()
updateRoom roomStore room = do
  roomVar <- newTVar room
  roomStoreMap <- readTVar roomStore
  let newMap = M.insert (name room) roomVar roomStoreMap
  writeTVar roomStoreMap newMap
  
createRoomIfNeeded :: RoomStore ->
                      String ->
                      STM Room
createRoomIfNeeded roomStore roomName = do
  roomStoreMap <- readTVar roomStore
  case M.lookup roomName roomStoreMap of
    Just existing -> return existing
    Nothing -> 
      do
        let newRoom = makeRoom name
        roomVar <- newTVar newRoom
        let newMap = M.insert (name room) roomVar roomStoreMap
        writeTVar roomStoreMap newMap
        return newRoom
  
maybeGetRoom :: RoomStore -> String -> STM (Maybe Room)
maybeGetRoom rooms name = do
  roomMap <- readTVar rooms
  case M.lookup name roomMap of
    Just roomVar -> do
      room <- readTVar roomVar
      return room
    Nothing -> return Nothing
   
maybeGetUser :: UserStore -> String -> STM (Maybe User)
maybeGetUser userStore name = do
  userMap <- readTVar userStore
  case M.lookup name userMap of
    Just userVar -> do
      user <- readTVar userVar
      return user
    Nothing -> return Nothing
