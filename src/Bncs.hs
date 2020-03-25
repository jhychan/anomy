-- | This module handles the link between Anomy and the PvPGN server. Converts messages from internal Anomy types to
-- BNCS messages for writing and BNCS messages to internal types for reading.
module Bncs (
        reader
      , writer
) where

import Bncs.Reader (readMsg)
import Bncs.Writer (Bncs, writeMsg)
import Core (Inc, inc
            , Socket, recvMsg, sendMsg
            , TChan, readTChanIO, writeTChanIO)


-- | Reads and converts messages from the server and pushes them into the incoming channel for Anomy to process.
reader :: Socket -> TChan Inc -> IO ()
reader netsock chancore = do
      m <- recvMsg netsock
      writeTChanIO chancore (inc (readMsg m))
      reader netsock chancore


-- | Grabs and converts messages from the outgoing channel of Anomy and sends them to the server.
writer :: Socket -> TChan Bncs -> IO ()
writer netsock chanbncs = do
      m <- readTChanIO chanbncs
      sendMsg netsock (writeMsg m)
      writer netsock chanbncs