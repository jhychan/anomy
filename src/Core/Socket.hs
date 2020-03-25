-- | Management, reading and writing of BNCS messages over TCP sockets.
module Core.Socket (
        Socket
      , newSocket
      , connectSocket
      , closeSocket
      , recvMsg
      , sendMsg
) where

import Util.String (bigToInt)

import Control.Monad (void)
import Network (HostName)
import Network.BSD (getHostByName, hostAddress, getProtocolNumber)
import Network.Socket
      ( Socket, Family (AF_INET), SocketType (Stream), SockAddr (..), ShutdownCmd (ShutdownBoth)
      , withSocketsDo, socket, connect, send, recvLen, shutdown, sClose)


-- | Initialise the socket to be used for the Anomy session.
newSocket :: IO Socket
newSocket = withSocketsDo $ do
      proto <- getProtocolNumber "tcp"
      sock <- socket AF_INET Stream proto
      return $! sock


-- | Connect to the PvPGN server.
connectSocket :: Socket -> HostName -> Int -> IO ()
connectSocket netsock nethostname netport = do
      nethostentry <- getHostByName nethostname
      connect netsock (SockAddrInet (fromIntegral netport) (hostAddress nethostentry))


-- | Shutdown the socket from the PvPGN server and return a fresh socket.
closeSocket :: Socket -> IO ()
closeSocket netsock = do
      shutdown netsock ShutdownBoth
      sClose netsock


-- | Grabs complete BNCS message packets for further processing.
recvMsg :: Socket -> IO String
recvMsg netsock = do
      msghead <- recv' [] netsock 4
      msgtail <- recv' [] netsock (bigToInt (drop 2 msghead) - 4)
      return $ msghead ++ msgtail


-- | Guarantee 'Network.Socket.recv' returns exactly the number of bytes specific in its argument.
recv' :: String -> Socket -> Int -> IO String
recv' msg _ 0 = return msg
recv' msghead netsock nbytes = do
      (msgtail, remnbytes) <- recvLen netsock nbytes
      recv' (msghead ++ msgtail) netsock (nbytes - remnbytes)


-- | Sends completed BNCS message packets to the connected PvPGN socket.
sendMsg :: Socket -> String -> IO ()
sendMsg netsock msg = do
      void $ send netsock msg