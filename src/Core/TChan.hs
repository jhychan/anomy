-- | This module defines operations over the Transactional Channels found in 'Control.Concurrent.STM.TChan'. Aims to
-- ease the use the STM functions within the Anomy monad.
module Core.TChan (
        TChan
      , newTChanIO
      , readTChanM
      , writeTChanM
      , readTChanIO
      , writeTChanIO
      , emptyTChanIO
) where

import Control.Concurrent.STM (TChan, atomically, newTChan, readTChan, writeTChan, isEmptyTChan)
import Control.Monad (unless)
import Control.Monad.Trans (MonadIO, liftIO)


-- | Convenenience.
newTChanIO :: IO (TChan a)
newTChanIO = atomically newTChan


-- | Read the next value stored in a TChan within any MonadIO class.
readTChanM :: MonadIO m => TChan a -> m a
readTChanM = liftIO . readTChanIO


-- | Write the given value into the TChan within any MonadIO class.
writeTChanM :: MonadIO m => TChan a -> a -> m ()
writeTChanM chan = liftIO . writeTChanIO chan


-- | Read the next value stored in a TChan.
readTChanIO :: TChan a -> IO a
readTChanIO = atomically . readTChan


-- | Write the given value into the TChan.
writeTChanIO :: TChan a -> a -> IO ()
writeTChanIO chan = atomically . writeTChan chan


-- | Manually empty the transactional channel.
emptyTChanIO :: TChan a -> IO ()
emptyTChanIO chan = do
      empty <- atomically (isEmptyTChan chan)
      unless empty $ do
            _ <- readTChanIO chan
            emptyTChanIO chan