-- | The log file component of anomy. Currently does not do any logging - merely provides functions to obtain
-- the log file resource ('Handle'). In the future it may be developed to take care of proper 'Auis' message
-- logging and parsing.
module Core.Log (
        newLog
      , getLogDir
      , writeLog
      , closeLog
      , stdout
) where

import Control.Monad (when)
import System.Directory (createDirectoryIfMissing, getCurrentDirectory, removeFile)
import System.FilePath (replaceBaseName)
import System.IO
      ( Handle, IOMode (AppendMode), BufferMode (NoBuffering)
      , openFile, hSetBuffering, hPutStr, hClose, hFileSize, stdout )
import System.Locale (defaultTimeLocale)
import System.Time (getClockTime, toCalendarTime, formatCalendarTime)


-- | Log file directory
logDir :: FilePath
logDir = "../logs/"


-- | Full path of log directory
getLogDir :: IO FilePath
getLogDir = getCurrentDirectory >>= return . (flip replaceBaseName) "logs"


-- | Obtain 'Handle' to log file. Returns handle and filename
newLog :: IO (Handle, String)
newLog = do
      createDirectoryIfMissing False logDir
      logfile <- newLogFile
      loghandle <- openFile (logDir ++ logfile) AppendMode
      hSetBuffering loghandle NoBuffering
      return (loghandle, logfile)


-- | Log file naming and creation
newLogFile :: IO FilePath
newLogFile = do
      ct <- getClockTime >>= toCalendarTime
      return $ formatCalendarTime defaultTimeLocale "session-%Y-%m-%d-%k.%M.txt" ct


-- | Function to write text to logs
writeLog :: Handle -> String -> IO ()
writeLog h t = do
      hPutStr h t


-- | Close log file
closeLog :: Handle -> String -> IO ()
closeLog loghandle logfile =
      when (not (null logfile)) $ do
            -- check file size for deleting
            logsize <- hFileSize loghandle
            hClose loghandle
            when (logsize == 0) $ do
                  catch (removeFile (logDir ++ logfile)) (\_ -> return ())
                  return ()
