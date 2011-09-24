import Message

import Network
import System.IO

import Control.Concurrent

connect :: PortNumber -> String -> IO Handle
connect port name  = do
  handle <- connectTo "localhost" (PortNumber port)
  hSetNewlineMode handle (NewlineMode CRLF CRLF)
  hPutStrLn handle ("LOGIN " ++ name)
  hFlush handle
  response <- hGetLine handle
  return handle

joinRoom :: Handle -> String -> IO ()
joinRoom handle room = do
  hPutStrLn handle ("JOIN #" ++ room)
  hFlush handle
  return ()

messageRoom :: Handle -> String -> String -> IO ()
messageRoom handle room msg = do
  hPutStrLn handle ("MSG #" ++ room ++ " " ++ msg)
  hFlush handle
  return ()

logout handle = do
  hPutStrLn handle "LOGOUT"
  hFlush handle
  return ()

connectAndLogout port name = connect port name >>= logout

connect500 :: PortNumber -> String -> IO [Handle]
connect500 port namePrefix = do
  sequence $ map (\n -> connect port (namePrefix ++ (show n))) [1..500]

logoutAll :: [Handle] -> IO ()
logoutAll users = foldr (>>) (return ()) $ map (\h -> logout h) users

allJoinRoom :: [Handle] -> String -> IO ()
allJoinRoom users room = foldr (>>) (return ()) $ map (\h -> joinRoom h room) users

allMessageRoom :: [Handle] -> String -> String -> IO ()
allMessageRoom users room msg = foldr (>>) (return ()) $ map (\h -> messageRoom h room msg) users