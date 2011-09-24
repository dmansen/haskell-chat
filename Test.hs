import Message

import Network
import System.IO

connect :: PortNumber -> String -> IO Handle
connect port name  = do
  handle <- connectTo "localhost" (PortNumber port)
  hSetNewlineMode handle (NewlineMode CRLF CRLF)
  hPutStrLn handle ("LOGIN " ++ name)
  hFlush handle
  response <- hGetLine handle
  return handle
  
joinRoom :: Handle -> IO ()
joinRoom handle = do
  hPutStrLn handle "JOIN #test"
  hFlush handle
  return ()

logout handle = do
  hPutStrLn handle "LOGOUT"
  hFlush handle
  return ()
  
connectAndLogout port name = connect port name >>= logout

connect1000 :: IO [Handle]
connect1000 = do
  sequence $ map (\n -> connect 9000 ("Test" ++ (show n))) [1..500]
  
logoutAll :: [Handle] -> IO ()
logoutAll users = foldr (>>) (return ()) $ map (\h -> logout h) users

allJoinRoom :: [Handle] -> IO ()
allJoinRoom users = foldr (>>) (return ()) $ map (\h -> joinRoom h) users

connectThenLogout1000 = connect1000 >>= logoutAll