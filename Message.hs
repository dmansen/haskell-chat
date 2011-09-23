module Message where

-- Messages from the client to the server
data ServerMessage =
  Login String |
  SPrivateMessage String String String |
  SRoomMessage String String String |
  Join String String |
  Part String String |
  Logout String |
  Invalid |
  StopThread

-- Messages from the server to the client
data ClientMessage =
  Ok |
  Error String |
  CPrivateMessage String String |
  CRoomMessage String String String

instance Show ClientMessage where
  show Ok = "OK\r\n"
  show (CPrivateMessage from msg) = "GOTUSERMSG " ++ from ++ " " ++ msg ++ "\r\n"
  show (CRoomMessage from room msg) = "GOTROOMMSG " ++ from ++ " #" ++ room ++ " " ++ msg ++ "\r\n"
  show (Error msg) = "ERROR " ++ msg ++ "\r\n"
  
parseMsg :: String -> ServerMessage
parseMsg msg = undefined