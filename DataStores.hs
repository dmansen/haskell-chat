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

makeUser name lock = User {
  userName = name,
  connection = lock,
  rooms = [] }
makeRoom name = Room { roomName = name, users = [] }

type UserStore = TVar (Map String User)
type RoomStore = TVar (Map String Room)

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

removeUserFromRooms :: Maybe User ->
                       UserStore ->
                       RoomStore ->
                       STM ()
removeUserFromRooms maybeUser userStore roomStore =
  case maybeUser of
    Just user -> do
      let userRooms = rooms user
      foldr (>>) (return ()) $ map (\newRoom -> updateSTM roomStore newRoom) $
        map (\r -> r {
                        users = filter (\u ->
                                         userName u /= userName user) $
                                users r
                        }) userRooms
    Nothing -> return ()

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