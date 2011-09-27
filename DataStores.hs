module DataStores where

import Control.Concurrent
import Control.Concurrent.STM

import Data.Map as M hiding (map, filter)

import System.IO

data Room = Room {
  roomName :: String,
  users :: [User]
} deriving (Eq)

data User = User {
  userName :: String,
  connection :: TMVar Handle,
  rooms :: [Room]
} deriving (Eq)

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