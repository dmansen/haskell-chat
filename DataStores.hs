module DataStores where

import Control.Concurrent
import Control.Concurrent.STM

import Data.Map as M hiding (map, filter)

import System.IO

data Room = Room {
  roomName :: String,
  users :: [User]
}

instance Eq Room where
  r == q = (roomName r) == (roomName q)

data User = User {
  userName :: String,
  connection :: TMVar Handle,
  rooms :: [Room]
}

instance Eq User where
  u == p = (userName u) == (userName p)

class StringKey a where
  stringKey :: a -> String

instance StringKey User where
  stringKey = userName

instance StringKey Room where
  stringKey = roomName

type UserStore = TVar (Map String User)
type RoomStore = TVar (Map String Room)

createRoomIfNeeded :: RoomStore ->
                      String ->
                      STM Room
createRoomIfNeeded roomStore name = do
  roomStoreMap <- readTVar roomStore
  case M.lookup name roomStoreMap of
    Just existing -> return existing
    Nothing -> do
      let newRoom = Room {
            roomName = name,
            users = []
          }
          newMap = M.insert (roomName newRoom) newRoom roomStoreMap
      writeTVar roomStore newMap
      return newRoom

addUserToRoom :: UserStore ->
                 RoomStore ->
                 User ->
                 String ->
                 STM (User, Bool)
addUserToRoom userStore roomStore user roomName = do
  room <- createRoomIfNeeded roomStore roomName
  let newUser = (user { rooms = room : (rooms user) } )
      newRoom = (room { users = user : (users room) } )
  if not (user `elem` (users room)) -- only add if necessary
     then do
       updateSTM userStore newUser
       updateSTM roomStore newRoom
       return (newUser, True)
     else return (user, False)


removeUserFromRoom :: UserStore ->
                      RoomStore ->
                      User ->
                      String ->
                      STM (User, Bool)
removeUserFromRoom userStore roomStore user roomName = do
  room <- createRoomIfNeeded roomStore roomName
  let newUser =
        (user { rooms = filter (/= room) (rooms user) })
      newRoom =
        (room { users = filter (/= user) (users room) })
  if user `elem` (users room)
     then do
       updateSTM userStore newUser
       updateSTM roomStore newRoom
       return (newUser, True)
     else return (user, False)

removeUserFromRooms :: User ->
                       UserStore ->
                       RoomStore ->
                       STM ()
removeUserFromRooms user userStore roomStore = do
  let userRooms = rooms user
  -- this somewhat tricky. first creates a list of actions:
  -- each one updates a room in STM to have the user removed. then,
  -- use foldr to apply >> to each one to create a single again
  -- which runs all of them in sequence.
  foldr (>>) (return ()) $ map (\newRoom -> updateSTM roomStore newRoom) $
    map (\r -> r {
            users = filter (/= user) $ users r
            }) userRooms

updateSTM :: (StringKey a) =>
             TVar (Map String a) ->
             a ->
             STM ()
updateSTM store a = do
  map <- readTVar store
  let newMap = M.insert (stringKey a) a map
  writeTVar store newMap

maybeGrabFromSTM :: TVar (Map String a) ->
                    String ->
                    STM (Maybe a)
maybeGrabFromSTM mapVar name = do
  map <- readTVar mapVar
  let maybeObj = M.lookup name map in
    case maybeObj `seq` maybeObj of
      Just a -> return (return a)
      Nothing -> return Nothing