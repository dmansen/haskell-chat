module Main where

import Control.Concurrent
import Control.Exception hiding (try)
import Control.Monad
import Message

import Network
import System.IO
import Text.ParserCombinators.Parsec

main = client "localhost" (PortNumber 9000)

client host port = withSocketsDo $ do
  handle <- connectTo host port
  hSetBuffering handle LineBuffering
  hSetNewlineMode handle (NewlineMode CRLF CRLF)
  forkIO $ loop handle
  listener handle
  
loop handle = (forever $ do
  command <- getLine
  case parse parseCommand "input" command of
    Left err -> 
      putStrLn ("Error: " ++ show err)
    Right c -> do
      hPutStrLn handle c)
  `finally`
  hClose handle
  
listener handle = forever $ do
  response <- hGetLine handle
  case parse parseResponse "response" response of
    Left err -> do
      putStrLn ("Error: " ++ show err)
    Right action -> 
      action

parseCommand :: Parser String
parseCommand =
      try parseLogin
  <|> try parseJoin
  <|> try parsePrivateMessage
  <|> try parseRoomMessage
  <|> try parsePart
  <|> try parseLogout
  <|> return "Couldn't parse"
  
parseLogin = do
  string "/c "
  name <- many (noneOf " ")
  eof
  return ("LOGIN " ++ name);

parseJoin = do
  string "/j #"
  room <- many (noneOf " ")
  eof
  return ("JOIN #" ++ room)

parseRoomMessage = do
  string "/m #"
  room <- many (noneOf " ")
  char ' '
  msg <- many anyChar
  eof
  return ("MSG #" ++ room ++ " " ++ msg)
  
parsePrivateMessage = do
  string "/m "
  to <- many (noneOf " ")
  char ' '
  msg <- many anyChar
  eof
  return ("MSG " ++ to ++ " " ++ msg)
  
parsePart = do
  string "/p #"
  room <- many (noneOf " ")
  eof
  return ("PART #" ++ room)
  
parseLogout = do
  string "/q"
  eof
  return "LOGOUT"
  
parseResponse :: Parser (IO ())
parseResponse = 
      try parseOk
  <|> try parseRoomMsg
  <|> try parseUserMsg
  <|> try parseError
  <|> return (putStrLn "Unknown response from server")
  
parseOk = do
  string "OK"
  eof
  return (return ())

parseRoomMsg = do
  string "GOTROOMMSG "
  user <- many (noneOf " ")
  string " #"
  room <- many (noneOf " ")
  msg <- many anyChar
  eof
  return (putStrLn ("#" ++ room ++ " " ++ user ++ ": " ++ msg))
  
parseUserMsg = do
  string "GOTUSERMSG "
  user <- many (noneOf " ")
  char ' '
  msg <- many anyChar
  eof
  return (putStrLn (user ++ ": " ++ msg))
  
parseError = do
  string "ERROR "
  reason <- many anyChar
  eof
  return (putStrLn ("ERROR " ++ reason))