module Gui.Icons (
        stockAnomyConnectionHot
      , stockAnomyConnectionCold
      , stockAnomyConfigurationHot
      , stockAnomyConfigurationCold
      , stockAnomyExitHot
      , stockAnomyExitCold
      , iconAddAnomyIds
      , iconPixbuf
      , iconPixbufMap
      , Icon (..)
) where

import Auis.Writer.Types (Users (..), Flag (..), Member (..), Presence (..), Rank (..))
-- import Core (showPing, showClient, showIcon, showLevel, showTag)

import Control.Monad (foldM)
import Data.List (isSuffixOf)
import Data.Map (Map, empty, insert, lookup, findWithDefault)
import Prelude hiding (lookup)
import System.Directory (getDirectoryContents)
import System.FilePath (takeBaseName)

import Graphics.UI.Gtk (StockId, Pixbuf, pixbufNewFromFile, pixbufNewFromFileAtScale)
import Graphics.UI.Gtk.General.IconFactory


-- | Path to GUI icons used for buttons and channel user list
iconDir :: FilePath
iconDir = "../share/icon/"


-- | Stock IDs for custom anomy toolbar icons added to stock icon factory
stockAnomyConnectionHot, stockAnomyConnectionCold :: StockId
stockAnomyConnectionHot = "anomy-connection-hot"
stockAnomyConnectionCold = "anomy-connection-cold"

stockAnomyConfigurationHot, stockAnomyConfigurationCold :: StockId
stockAnomyConfigurationHot = "anomy-configuration-hot"
stockAnomyConfigurationCold = "anomy-configuration-cold"

stockAnomyExitHot, stockAnomyExitCold :: StockId
stockAnomyExitHot = "anomy-exit-hot"
stockAnomyExitCold = "anomy-exit-cold"


-- | Set up toolbar icons to be in stock icons
iconAddAnomyIds :: IO ()
iconAddAnomyIds = do
      pbconnectionhot <- pixbufNewFromFile (iconDir ++ "connection-hot.png")
      iscconnectionhot <- iconSetNewFromPixbuf pbconnectionhot
      pbconnectioncold <- pixbufNewFromFile (iconDir ++ "connection-cold.png")
      iscconnectioncold <- iconSetNewFromPixbuf pbconnectioncold
      
      pbconfigurationhot <- pixbufNewFromFile (iconDir ++ "configuration-hot.png")
      icsconfigurationhot <- iconSetNewFromPixbuf pbconfigurationhot
      pbconfigurationcold <- pixbufNewFromFile (iconDir ++ "configuration-cold.png")
      icsconfigurationcold <- iconSetNewFromPixbuf pbconfigurationcold
      
      pbexithot <- pixbufNewFromFile (iconDir ++ "exit-hot.png")
      icsexithot <- iconSetNewFromPixbuf pbexithot
      pbexitcold <- pixbufNewFromFile (iconDir ++ "exit-cold.png")
      icsexitcold <- iconSetNewFromPixbuf pbexitcold
      
      icf <- iconFactoryNew
      iconFactoryAdd icf stockAnomyConnectionHot iscconnectionhot
      iconFactoryAdd icf stockAnomyConnectionCold iscconnectioncold
      iconFactoryAdd icf stockAnomyConfigurationHot icsconfigurationhot
      iconFactoryAdd icf stockAnomyConfigurationCold icsconfigurationcold
      iconFactoryAdd icf stockAnomyExitHot icsexithot
      iconFactoryAdd icf stockAnomyExitCold icsexitcold
      iconFactoryAddDefault icf


-- | Lookup function for Map if icon Pixbufs
iconPixbuf :: Map String Pixbuf -> String -> Pixbuf
iconPixbuf ipbmap icon = do
      findWithDefault unkn icon ipbmap
      where Just unkn = lookup "UNKN" ipbmap


iconPixbufMap :: IO (Map String Pixbuf)
iconPixbufMap = do
      files <- getDirectoryContents iconDir
      foldM iconPixbufAdd empty (filter (isSuffixOf ".png") files)


iconPixbufAdd :: Map String Pixbuf -> FilePath -> IO (Map String Pixbuf)
iconPixbufAdd ipbmap png = do
      pixbuf <- pixbufNewFromFileAtScale (showString iconDir png) (-1) 24 True
      return $! insert (takeBaseName png) pixbuf ipbmap


class Icon a where
      iconName :: a -> String
      iconRank :: a -> Int


instance Icon Users where
      iconName Users {uFlag = Administrator} = iconName Administrator
      iconName Users {uFlag = TempOperator} = iconName TempOperator
      iconName Users {uFlag = Voiced} = iconName Voiced
      iconName Users {uFlag = Operator} = iconName Operator
      iconName Users {uFlag = Squelched} = iconName Squelched
      iconName Users {uFlag = FlagUnknown _} = iconName (FlagUnknown undefined)
      iconName Users {uClient = client@"PX3W", uIcon = icon}
            | null icon = client
            | otherwise = icon
      iconName Users {uClient = client@"3RAW", uIcon = icon}
            | null icon = client
            | otherwise = icon
      iconName Users {uClient = client} = client

      iconRank Users {uFlag = flag, uClient = client, uLevel = level} = iconRank flag + iconRankClient client + iconRankLevel level


instance Icon Flag where
      iconName Administrator = "ADMN"
      iconName TempOperator = "TPOP"
      iconName Voiced = "VOIC"
      iconName Operator = "OPER"
      iconName Squelched = "SQCH"
      iconName (FlagUnknown _) = "UNKN"
      iconName _ = undefined

      iconRank Administrator = 10000
      iconRank Operator = 20000
      iconRank TempOperator = 30000
      iconRank Voiced = 40000
      iconRank User = 50000
      iconRank FirstJoin = 60000
      iconRank Squelched = 70000
      iconRank (FlagUnknown _) = 80000


iconRankClient :: String -> Int
iconRankClient "PX3W" = 1000
iconRankClient "3RAW" = 2000
iconRankClient "PX2D" = 3000
iconRankClient "VD2D" = 4000
iconRankClient "PXES" = 5000
iconRankClient "RATS" = 6000
iconRankClient "TAHC" = 7000
iconRankClient _ = undefined


iconRankLevel :: String -> Int
iconRankLevel [ ] = 81
iconRankLevel lvl = 80 - 10 * read lvl


instance Eq Users where
      user1 == user2 = iconRank user1 == iconRank user2


instance Ord Users where
      compare user1 user2 =
            let c1 = compare (iconRank user1) (iconRank user2)
                c2 = compare (uUsername user1) (uUsername user2)
            in  if c1 == EQ
                  then c2
                  else c1


instance Icon Member where
      iconName Member {cRank = Newb} = "NEWB"
      iconName Member {cRank = Peon} = "PEON"
      iconName Member {cRank = Grunt} = "GRNT"
      iconName Member {cRank = Shaman} = "SHMN"
      iconName Member {cRank = Chieftain} = "CHFT"

      iconRank Member {cRank = Chieftain, cPresence = presence} = 10 + iconRank presence
      iconRank Member {cRank = Shaman, cPresence = presence} = 20 + iconRank presence
      iconRank Member {cRank = Grunt, cPresence = presence} = 30 + iconRank presence
      iconRank Member {cRank = Peon, cPresence = presence} = 40 + iconRank presence
      iconRank Member {cRank = Newb, cPresence = presence} = 50 + iconRank presence


instance Icon Presence where
      iconName Offline = "OFFL"
      iconName Online = "ONLN"
      iconName OnlineInChannel = "ONCH"
      iconName OnlineInPublicGame = "ONPB"
      iconName OnlineInPrivateGame = "ONPV"

      iconRank Offline = 5
      iconRank Online = 4
      iconRank OnlineInChannel = 3
      iconRank OnlineInPublicGame = 2
      iconRank OnlineInPrivateGame = 1


instance Eq Member where
      member1 == member2 = iconRank member1 == iconRank member2


instance Ord Member where
      compare member1 member2 =
            let c1 = compare (iconRank member1) (iconRank member2)
                c2 = compare (cUsername member1) (cUsername member2)
            in  if c1 == EQ
                  then c2
                  else c1
