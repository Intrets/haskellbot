module Bot.Irc.Connection where

import qualified Network.Socket as N
import Bot
import GHC.IO.Handle (Handle)
import Control.Exception -- base
import System.IO

connect :: String -> N.PortNumber -> IO Bot
connect server port =
  notify $ do
    h <- connectTo server port
    return (Bot h)
  where
    notify a =
      bracket_
        (putStrLn ("Connecting to " ++ server ++ " ...") >> hFlush stdout)
        (putStrLn "done.")
        a

-- Connect to the server and return a Handle (helper for connect)
connectTo :: N.HostName -> N.PortNumber -> IO Handle
connectTo host port = do
  addr:_ <- N.getAddrInfo Nothing (Just host) (Just (show port))
  sock <-
    N.socket (N.addrFamily addr) (N.addrSocketType addr) (N.addrProtocol addr)
  N.connect sock (N.addrAddress addr)
  N.socketToHandle sock ReadWriteMode
