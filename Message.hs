module Message (ServerMessage(..),
                ClientMessage(..),
                parseMsg) where

import Debug.Trace

import Text.ParserCombinators.Parsec
import Text.ParserCombinators.Parsec.Error

-- Messages from the client to the server
data ServerMessage =
  Login String |
  SPrivateMessage String String |
  SRoomMessage String String |
  Join String |
  Part String |
  Logout |
  Invalid String |
  StopThread deriving (Read, Show, Eq)

-- Messages from the server to the client
data ClientMessage =
  Ok |
  Error String |
  CPrivateMessage String String |
  CRoomMessage String String String deriving (Read, Eq)

instance Show ClientMessage where
  show Message.Ok = "OK"
  show (CPrivateMessage from msg) = "GOTUSERMSG " ++ from ++ " " ++ msg
  show (CRoomMessage from room msg) = "GOTROOMMSG " ++ from ++ " #" ++ room ++ " " ++ msg
  show (Message.Error msg) = "ERROR " ++ msg

parseMsg :: String -> ServerMessage
parseMsg msg = case parse messageParser "Network" msg of
  Left err -> Invalid ("Error parsing: " ++ (show err))
  Right msg -> trace ("Got message: " ++ (show msg)) $ msg


messageParser :: Parser ServerMessage
messageParser =
      try parseLogin
  <|> try parseJoin
  <|> try parsePrivateMessage
  <|> try parseRoomMessage
  <|> try parsePart
  <|> try parseLogout
  <|> return (Invalid "Didn't parse")

parseLogin = do
  s <- string "LOGIN "
  name <- many (noneOf " ")
  eof
  return (Login name)

parseJoin = do
  string "JOIN #"
  room <- many (noneOf " ")
  eof
  return (Join room)

parsePrivateMessage = do
  string "MSG "
  to <- many (noneOf " #")
  char ' '
  msg <- many anyChar
  eof
  return (SPrivateMessage to msg)

parseRoomMessage = do
  string "MSG #"
  to <- many (noneOf " ")
  char ' '
  msg <- many anyChar
  eof
  return (SRoomMessage to msg)

parsePart = do
  string "PART #"
  room <- many (noneOf " ")
  eof
  return (Part room)

parseLogout = do
  string "LOGOUT"
  eof
  return Logout