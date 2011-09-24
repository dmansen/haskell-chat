module Main where

import Debug.Trace

import Network

import System (getArgs)
import System.Console.GetOpt

import Server

defaultPort = 9000

main = do
  args <- getArgs
  let (flags, nonOpts, msgs) = getOpt Permute options args
  if null flags
    then trace ("Starting server on default port (" ++ (show defaultPort) ++ ").") $ server (PortNumber (fromIntegral defaultPort))
    else server (head flags)

options :: [OptDescr PortID]
options = [ Option ['p'] ["port"]
            (ReqArg makePortNumber "port number")
            "server port number" ]

makePortNumber :: String -> PortID
makePortNumber s =
  case reads s :: [(Int, String)] of
    (p, _):rest -> trace ("Starting server on port " ++ (show p)) $ PortNumber (fromIntegral p)
    otherwise   -> trace ("Error parsing port. Starting server on default port (" ++ (show defaultPort) ++ ").") $ PortNumber (fromIntegral defaultPort)
