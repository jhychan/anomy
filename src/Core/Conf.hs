-- | The configuration file parser. Uses the 'read' method to convert configuration file to the read-only 'AConf'.
-- Similarly, uses the 'show' method to write configuration to file. Generalised to allow 'IConf' to use readConf
-- and writeConf.
module Core.Conf (
        newAConf
      , fileAConf
      , readConf
      , writeConf
      , module System.IO.Error
) where

import Core.Types (AConf (..))
import Util.List (takeSnd, replaceL)

import Data.List (intercalate)
import System.IO.Error (isDoesNotExistError)
import Text.Regex.Posix ((=~))


-- | Core configuration file name.
fileAConf :: FilePath
fileAConf = "../etc/anomyrc"


-- | New and incomplete configuration.
newAConf :: AConf
newAConf = AConf
      { netHostname = "pvpgn.boredaussie.com"
      , netPort = 6112
      , netGamePort = 6112
      , netRCWait = 30

      , accUsername = []
      , accPassword = []
      , accEmail = []
      , accChannel = Nothing

      , autoLogon = True
      , autoAccept = True
      }


-- | Reads the record-syntax config file, converts using 'read' and returns the config data type.
readConf :: (Read a, Show a) => FilePath -> a -> IO a
readConf fileconf conf = do
      confstr <- readFile fileconf
      readIO $! toConf conftype confstr
      where conftype = (head . words . show) conf


toConf :: String -> String -> String
toConf conftype conf = (join . clean . sift) conf ++ "}"
      where sift  = map matchConfLine . lines
            clean = filter (not . null)
            join  = (++) (conftype ++ " {") . intercalate ", "


-- | Given a string /l/ which contains no new-line chars, return the line if it matches the configuration line spec.
matchConfLine :: String -> String
matchConfLine l = l =~ "^[[:space:]]*[[:lower:]]+[[:alnum:]].*[[:space:]]+=.*"



-- | Use 'show' and write the config data type to config file path given.
writeConf :: Show a => FilePath -> a -> IO ()
writeConf fileconf conf = do
      writeFile fileconf (showConf conf)


showConf :: Show a => a -> String
showConf = replaceL ", " "\n" . init . takeSnd '{' . show
