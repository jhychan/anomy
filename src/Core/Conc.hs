-- | Concurrency and exception functions for use within the 'Anomy' monad.
module Core.Conc (
        spawn
      , throwA
      , module Control.Concurrent
      , module Control.Exception
) where

import Core.Types (Anomy, io)

import Control.Concurrent (ThreadId, myThreadId, forkIO, killThread, threadDelay)
import Control.Exception
      ( Exception (..), SomeException, AsyncException (ThreadKilled), IOException
      , throwIO, throwTo, catch, bracket )
import Prelude hiding (catch)

-- | Wraps 'forkIO' with a simple exception handler passing exceptions back to the main thread.
spawn :: IO () -> IO ThreadId
spawn child = do
      parent <- myThreadId
      forkIO $ child `catch` throwAllTo parent


throwAllTo :: ThreadId -> SomeException -> IO ()
throwAllTo tid excep = do
      case (fromException excep) of
            Just ThreadKilled -> return ()
            _ -> throwTo tid excep


throwA :: Exception e => e -> Anomy a
throwA = io . throwIO