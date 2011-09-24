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
unsafeReadMessage :: Handle -> IO ServerMessage
unsafeReadMessage handle = do
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
    msg <- unsafeReadMessage handle
    (responseMsg, cont) <- do
      case msg of
        Login name -> do
          user <- atomically $ tryLogin users name handle
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

-- This is the main handler loop for a client. It is fairly straightforward
-- except for one thing: each one of the functions to process a
-- message return a message to forward to this user, and an IO
-- thunk containing the rest of the stuff to do (forwarding
-- messages to other users, etc). We separate them so that we
-- can respond to the user quickly and do all of the heavy lifting
-- of dispatching messages in a different thread.
dispatcherThread :: User ->
                    UserStore ->
                    RoomStore ->
                    Handle ->
                    IO ()
dispatcherThread user users rooms handle = do
  let repeat = dispatcherThread user users rooms handle in do
    msg <- unsafeReadMessage handle
    (responseMsg, cont) <- atomically $ do
      case msg of
        Login _ ->
          return (Error "Already logged in", repeat)
        SPrivateMessage to msg ->
          privateMessage users user to msg repeat
        SRoomMessage room msg ->
          roomMessage rooms user room msg repeat
        Join room ->
          joinRoom users rooms user room repeat
        Part room ->
          partRoom users rooms user room repeat
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
  atomically $ logout users rooms (userName user)
  trace ("Thread for " ++ (userName user) ++ " dying.") $ return ()

tryLogin :: UserStore ->
            String ->
            Handle ->
            STM (Maybe User)
tryLogin users name handle = do
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
          STM (ClientMessage, IO ())
logout userStore roomStore name = do
  maybeUser <- maybeGrabFromSTM userStore name
  userMap <- readTVar userStore
  writeTVar userStore (M.delete name userMap)
  return (Ok, trace (name ++ " has left")
              (atomically $ removeUserFromRooms maybeUser userStore roomStore))

privateMessage :: UserStore ->
                  User ->
                  String ->
                  String ->
                  IO () ->
                  STM (ClientMessage, IO ())
privateMessage userStore from toName msg cont = do
  maybeUser <- maybeGrabFromSTM userStore toName
  case maybeUser of
    Just toUser ->
      return (Ok,
              (sendMessages
               [(buildPrivateMessage toUser (userName from) msg)]) >>
              cont)
    Nothing -> return (Error "User is not logged in", cont)

roomMessage :: RoomStore ->
               User ->
               String ->
               String ->
               IO () ->
               STM (ClientMessage, IO ())
roomMessage roomStore user toRoom msg cont = do
  maybeRoom <- maybeGrabFromSTM roomStore toRoom
  case maybeRoom of
    Just room ->
      if user `elem` (users room)
         then return (Ok,
                      (sendMessages (buildRoomMessages room (userName user) msg)) >>
                      cont)
         else return (Error ("Not in room: " ++ (roomName room)), cont)
    Nothing -> return (Error ("Not in room: " ++ toRoom), cont)

joinRoom :: UserStore ->
            RoomStore ->
            User ->
            String ->
            IO () ->
            STM (ClientMessage, IO ())
joinRoom userStore roomStore user roomName cont = do
  room <- createRoomIfNeeded roomStore roomName
  let newUser = (user { rooms = room : (rooms user) } )
      newRoom = (room { users = user : (users room) } )
  updateSTM userStore newUser
  updateSTM roomStore newRoom
  return (Ok, cont)

partRoom :: UserStore ->
            RoomStore ->
            User ->
            String ->
            IO () ->
            STM (ClientMessage, IO ())
partRoom userStore roomStore user rName cont = do
  room <- createRoomIfNeeded roomStore rName
  let newUser =
        (user { rooms = filter
                        (\r -> roomName r /= roomName room)
                        (rooms user)
              })
      newRoom =
        (room { users = filter
                        (\u -> userName u /= userName user)
                        (users room)
              })
  updateSTM userStore newUser
  updateSTM roomStore newRoom
  return (Ok, cont)

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