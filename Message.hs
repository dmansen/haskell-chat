module Message where

-- Messages from the client to the server
data ServerMessage =
  Login String |
  PrivateMessage String String String |
  RoomMessage String String String |
  Join String String |
  Part String String |
  Logout String |
  Invalid |
  StopThread

-- Messages from the server to the client
data ClientMessage =
  Ok |
  Error String |
  PrivateMessage String String |
  RoomMessage String String String

instance Show ClientMessage where
  show Ok = "OK\r\n"
  show (PrivateMessage from msg) = "GOTUSERMSG " ++ from ++ " " ++ msg ++ "\r\n"
  show (RoomMessage from room msg) = "GOTROOMMSG " ++ from ++ " #" ++ room ++ " " ++ msg ++ "\r\n"
  show (Error msg) = "ERROR " ++ msg ++ "\r\n"