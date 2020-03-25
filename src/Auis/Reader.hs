-- | The 'Auis' reader module contains functions which processes string inputs given by the user via the user interface.
-- Makes use of regular expressions to match commands unique to 'Anomy' and the Parsec parser to break them down into
-- the components needed for constructing the appropriate 'Auis' message.
module Auis.Reader (
        readMsg
      , autologon
) where

import Auis.Reader.Types
import Util.List (takeSnd)

import Data.Char (isSpace)
import Text.ParserCombinators.Parsec
import Text.Regex
--import Text.Regex.Posix ((=~))


-- | Converts user input strings into 'Auis' messages.
readMsg :: String -> Auis
readMsg ('/':'!':xs) = SendChat ('/':xs)
readMsg ('/':xs) = readMsgCmd xs
readMsg [] = SendChat " "
readMsg c = SendChat c


-- | Matches input strings to internal commands. Parses the string if relevant to the command.
readMsgCmd :: String -> Auis
readMsgCmd c
      | c =~ "^(a|about) *$" = ShowAbout
      | c =~ "^(f|friends) +(l|list) *$" = ShowFriends
      | c =~ "^(g|games) +(l|list) *$" = ShowGames

      | c =~ "^(l|logon|login) *$" = ExecLogon
      | c =~ "^(rs|restart) *$" = ExecRestart

      | c =~ "^(c|clan) +.*" = readMsgCmdClan c (takeSnd ' ' c)

      | c =~ "^rj *$" = SendChat "/rejoin"
      | otherwise = SendChat ('/':c)


-- | Matches, parses and converts clan-management commands to 'Auis' messages.
readMsgCmdClan :: String -> String -> Auis
readMsgCmdClan c cr
      | cr =~ "^ *(a|accept) *$" = ReplyClanInvite Accept
      | cr =~ "^ *(r|reject) *$" = ReplyClanInvite Reject
      | cr =~ "^ *(i|invite) +[^[:space:]]+ *$" = SendClanInvite (args !! 1)

      | cr =~ "^ *(sr|setrank) +[^[:space:]]+ +(p|peon|g|grunt|s|shaman|c|chieftain) *$" =
            ChangeClanMemberRank (args !! 1) (readMsgCmdClanRank (args !! 2))
      | cr =~ "^ *(sc|setchief) +[^[:space:]]+ *$" = SetClanChieftain (args !! 1)

      | cr =~ "^ *motd +.+ *$" = SetClanMotd motd
      | cr =~ "^ *create +[^[:space:]]{1,4} +.+" = ExecClanCreation (args !! 1) desc
      | cr =~ "^ *kick +[^[:space:]]+ *$" = RemoveClanMember (args !! 1)
      | cr =~ "^ *leave *$" = LeaveClan
      | cr =~ "^ *disband *$" = DisbandClan
      
      | otherwise = SendChat ('/':c)
            where Right args = parse argsP [] cr
                  Right motd = parse motdP [] cr
                  Right desc = parse descP [] cr


-- | Parsec parser which separates command strings by their whitespace.
argsP :: Parser [String]
argsP = sepEndBy1 (many1 (satisfy (not . isSpace))) spaces


-- | Parsec parser specifically for extracting motd strings.
motdP :: Parser String
motdP = do
      spaces
      _ <- many1 letter
      spaces
      many1 anyChar


-- | Parsec parser made specifically to extract the clan name of the clan creation command string.
descP :: Parser String
descP = do
      spaces
      _ <- many1 letter
      spaces
      _ <- many1 (satisfy (not . isSpace))
      spaces
      many1 anyChar


-- | Converts rank strings into rank constructors.
readMsgCmdClanRank :: String -> Rank
readMsgCmdClanRank "p" = Peon
readMsgCmdClanRank "peon" = Peon
readMsgCmdClanRank "g" = Grunt
readMsgCmdClanRank "grunt" = Grunt
readMsgCmdClanRank "s" = Shaman
readMsgCmdClanRank "shaman" = Shaman
readMsgCmdClanRank "c" = Chieftain
readMsgCmdClanRank "chieftain" = Chieftain
readMsgCmdClanRank _ = undefined


-- | Auto-logon trigger message.
autologon :: Auis
autologon = ExecLogonAuto


-- | Case-insensitive regex.
(=~) :: String -> String -> Bool
(=~) source1 source = 
      case matchRegex (mkRegexWithOpts source True False) source1 of
            Just _ -> True
            Nothing -> False