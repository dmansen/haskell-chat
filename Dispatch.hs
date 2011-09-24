module Dispatch (loginThreadWrapper) where

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Data.Map as M hiding (filter, map)
import Debug.Trace
import System.IO

import Prelude hiding (catch)

import DataStores
import Message

-- This module is responsible for broadcasting messages
-- to the proper users.

-- send a list of messages. this spawns another thread so that
-- our main handler thread doesn't need to wait for it to finish.
-- uses safePutMsg to ensure that the messages are sent in the
-- proper order.
sendMessages :: [(TMVar Handle, ClientMessage)] ->
                IO ThreadId
sendMessages = forkIO .
               foldr (>>) (return ()) .
               map (\(h, msg) -> safePutMsg h msg)

-- each user has an MVar () which is used as a mutex to
-- ensure no interleaving of messages.
safePutMsg :: TMVar Handle -> ClientMessage -> IO ()
safePutMsg lock msg = do
  handle <- atomically $ takeTMVar lock
  unsafePutMsg handle msg
  atomically $ putTMVar lock handle

-- puts a message on a handle without a lock. used
-- in the login thread (since we haven't created an
-- MVar for them yet)
unsafePutMsg :: Handle -> ClientMessage -> IO ()
unsafePutMsg handle msg = hPutStrLn handle (show msg)

-- grabs the next message from the handle and parses it
readMessage :: Handle -> IO ServerMessage
readMessage handle = do
  line <- hGetLine handle
  let msg = parseMsg line in return msg

-- wrap the thread in a function - necessary because loginThread uses
-- recursion to continue. if we didn't wrap this, the function would
-- add another finally clause every time the function was called.
loginThreadWrapper userStore roomStore handle =
  loginThread userStore roomStore handle
  `finally`
  loginExceptionHandler handle

-- waits on the provided handle for the user's login message
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
          user <- tryLogin users name handle
          case user of
            Just u ->
              return (Ok, trace (name ++ " logged in") $
                          dispatcherThreadWrapper u users rooms handle)
            Nothing ->
              return (Error "Username already in use", repeat)
        otherwise ->
          return (Error "Not logged in", repeat)
    unsafePutMsg handle responseMsg
    cont

-- make sure we always close out the user's handle
loginExceptionHandler :: Handle -> IO ()
loginExceptionHandler handle = trace "Doing final cleanup." $ hClose handle

-- same comment above as loginThreadWrapper - recursion forces us to
-- wrap this
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
          -- we don't need to call logout explicity
          -- (see the finally block above, and exception handler below)
          return (Ok, return ())
        Invalid err ->
          return (Error ("Invalid Command: " ++ err), repeat)
    sendMessages [(connection user, responseMsg)]
    cont

-- make sure we always go through the logout procedure,
-- even in the event of an error
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
            IO (Maybe User)
tryLogin users name handle = do
  -- create the lock first, so that we can ensure everything after
  -- happens in a single STM transaction
  atomically $ do
    user <- maybeGrabFromSTM users name
    case user of
      Just u -> return Nothing
      Nothing -> do
        newLock <- newTMVar handle
        let newUser = makeUser name newLock
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
  return (Ok, trace (name ++ " has left")
              (atomically $ removeUserFromRooms maybeUser userStore roomStore))

privateMessage :: UserStore ->
                  String ->
                  String ->
                  String ->
                  IO () ->
                  IO (ClientMessage, IO ())
privateMessage userStore fromName toName msg cont = atomically $ do
  maybeUser <- maybeGrabFromSTM userStore toName
  case maybeUser of
    Just toUser -> return (Ok,
                            (sendMessages
                             [(buildPrivateMessage toUser fromName msg)]) >>
                           cont)
    Nothing -> return (Error "User is not logged in", cont)

roomMessage :: RoomStore ->
               String ->
               String ->
               String ->
               IO () ->
               IO (ClientMessage, IO ())
roomMessage roomStore fromName toRoom msg cont = atomically $ do
  maybeRoom <- maybeGrabFromSTM roomStore toRoom
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
joinRoom userStore roomStore userName roomName cont = atomically $ do
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
partRoom userStore roomStore uName rName cont = atomically $ do
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
                       (TMVar Handle, ClientMessage)
buildPrivateMessage to fromName msg =
  let cMessage = CPrivateMessage fromName msg
      conn = connection to in
    (conn `seq` conn, cMessage `seq` cMessage)

buildRoomMessages :: Room ->
                     String ->
                     String ->
                     [(TMVar Handle, ClientMessage)]
buildRoomMessages room from msg =
  map (\u ->
          let cMessage = CRoomMessage from (roomName room) msg
              conn = connection u in
            (conn `seq` conn, cMessage `seq` cMessage))
  (filter (\u -> userName u /= from) (users room))