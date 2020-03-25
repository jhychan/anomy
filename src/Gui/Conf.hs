-- | Gui configuration file saving and loading. Inherits functions and types from 'Core.Conf'.
module Gui.Conf (
        newIConf
      , fileIConf
      , initConf
      , getAConf
      , getIConf
) where

import Core.Types (AConf (..))
import Gui.Types (IConf (..), IState (..))
import Util.String (hexToInt)

import Control.Monad (when)
import Data.Char (isHexDigit)
import Data.List (elemIndex)
import Text.ParserCombinators.Parsec

import Graphics.UI.Gtk


-- | Gui configuration file.
fileIConf :: FilePath
fileIConf = "../etc/anomyguirc"


-- | Default configuration.
newIConf :: IConf
newIConf = IConf
      { winSize = Nothing
      , winPos = Nothing

      , autoStart = True
      , showJoinLeave = True
      , showSimpleJoin = True

      , urgent = True
      , urgentJoin = False
      , urgentLeave = False
      , urgentTalk = True
      , urgentWhisper = True
      , urgentGeneral = True
      
      , logging = False
      , borderless = False

      , bgColourEntry = "#24242C"
      , fgColourEntry = "#D6D6D6"
      , bgColourList  = "#24242C"
      , fgColourList  = "#BABABA"
      , fgColourListLevel = "#FED312"
      , fgColourListTag = "#FED312"

      , bgColourChat  = "#24242C"
      , fgColourTime = "#555555"
      , fgColourUsername = "#FED312"
      , fgColourNormal = "#FFFFFF"
      , fgColourAdmin = "#45ACF6"
      , fgColourWhisper = "#00FF00"
      , fgColourEmote = "#888888"
      , fgColourError = "#FF0000"
      , fgColourGeneral = "#80D8FF"
      , fgColourInternal = "#555555"
      , fgColourSpecial = "#9A32CD"
      , fgColourURL = "#7CDD4B"
      }



-- | Cause configuration to take effect on the GUI.
initConf :: IState -> IConf -> AConf -> IO ()
initConf istate iconf aconf = do
      -- window position and size
      case winSize iconf of
            Just (w, h) -> windowResize (wMain istate) w h
            Nothing -> return ()
      case winPos iconf of
            Just (x, y) -> windowMove (wMain istate) x y
            Nothing -> return ()
      
      -- server, account and behavioural settings.
      set (etNetServer istate) [ entryText := netHostname aconf ]
      set (etNetRCWait istate) [ entryText := show (netRCWait aconf) ]
      
      set (etAccUsername istate) [ entryText := accUsername aconf ]
      set (etAccPassword istate) [ entryText := accPassword aconf ]
      
      case accChannel aconf of
            Nothing -> set (cbAccChannel istate) [ toggleButtonActive := False ]
            Just accchannel -> do
                  set (cbAccChannel istate) [ toggleButtonActive := True ]
                  set (etAccChannel istate) [ entryText := accchannel ]
      
      set (cbAutoAccept istate) [ toggleButtonActive := autoAccept aconf ]

      -- runtime behaviour defaults.
      checkMenuItemSetActive (miShowJoinLeave istate) (showJoinLeave iconf)
      checkMenuItemSetActive (miShowSimpleJoin istate) (showSimpleJoin iconf)

      -- urgency hint settings.
      checkMenuItemSetActive (miUrgent istate) (urgent iconf)
      checkMenuItemSetActive (miUrgentJoin istate) (urgentJoin iconf)
      checkMenuItemSetActive (miUrgentLeave istate) (urgentLeave iconf)
      checkMenuItemSetActive (miUrgentTalk istate) (urgentTalk iconf)
      checkMenuItemSetActive (miUrgentWhisper istate) (urgentWhisper iconf)
      checkMenuItemSetActive (miUrgentGeneral istate) (urgentGeneral iconf)
      
      -- logging settings.
      checkMenuItemSetActive (miLogging istate) (logging iconf)
      
      -- borderless window settings.
      checkMenuItemSetActive (miBorderless istate) (borderless iconf)

      -- colourise widget backgrounds and foregrounds.
      widgetModifyBase (etChat istate) StateNormal $
            toColor (bgColourEntry newIConf) (bgColourEntry iconf)
      widgetModifyText (etChat istate) StateNormal $
            toColor (fgColourEntry newIConf) (fgColourEntry iconf)

      widgetModifyBase (tvChannel istate) StateNormal $
            toColor (bgColourList newIConf) (bgColourList iconf)
      widgetModifyText (tvChannel istate) StateNormal $
            toColor (fgColourList newIConf) (fgColourList iconf)
      set (crChannelLevel istate) [ cellTextForeground := fgColourListLevel iconf ]
      set (crChannelTag istate) [ cellTextForeground := fgColourListTag iconf ]

      widgetModifyBase (tvClan istate) StateNormal $
            toColor (bgColourList newIConf) (bgColourList iconf)
      widgetModifyText (tvClan istate) StateNormal $
            toColor (fgColourList newIConf) (fgColourList iconf)

      -- colourise chat ouput base and construct text tag table.
      widgetModifyBase (tvChat istate) StateNormal $
            toColor (bgColourChat newIConf) (bgColourChat iconf)

      set (ttTime istate) [ textTagForeground := fgColourTime iconf ]
      set (ttUsername istate) [ textTagForeground := fgColourUsername iconf ]
      set (ttNormal istate) [ textTagForeground := fgColourNormal iconf ]
      set (ttAdmin istate) [ textTagForeground := fgColourAdmin iconf ]
      set (ttWhisper istate) [ textTagForeground := fgColourWhisper iconf ]
      set (ttEmote istate) [ textTagForeground := fgColourEmote iconf ]
      set (ttError istate) [ textTagForeground := fgColourError iconf ]
      set (ttGeneral istate) [ textTagForeground := fgColourGeneral iconf ]
      set (ttInternal istate) [ textTagForeground := fgColourInternal iconf ]
      set (ttSpecial istate) [ textTagForeground := fgColourSpecial iconf ]
      set (ttURL istate) [ textTagForeground := fgColourURL iconf
                         , textTagUnderline := UnderlineSingle ]

      -- Draw all the visible widgets now.
      widgetShowAll (wMain istate)
      -- Hide configure window by default.
      set (hbConfiguration istate) [ widgetVisible := False ]
      
      -- If any crucial configuration fields are empty, drop down configuration widget, grab it's
      -- focus and don't autostart.
      ettocheck <- return
            [ null (netHostname aconf)
            , null (accUsername aconf), null (accPassword aconf) ]
      if or ettocheck
            then do
                  case elemIndex True ettocheck of
                        Just 0 -> widgetGrabFocus (etNetServer istate)
                        Just 2 -> widgetGrabFocus (etAccPassword istate)
                        _ -> widgetGrabFocus (etAccUsername istate)
            else when (autoStart iconf) (postGUIAsync $ buttonClicked (btStartStop istate))



-- | (Re)create 'AConf' configuation data from old configuration and configuration widgets.
getAConf :: IState -> AConf -> IO AConf
getAConf istate aconf= do
      -- server, account and behavioural settings.
      nethostname <- get (etNetServer istate) entryText
      netrcwait <- get (etNetRCWait istate) entryText >>= return . read
      
      accusername <- get (etAccUsername istate) entryText
      accpassword <- get (etAccPassword istate) entryText
      
      checked <- get (cbAccChannel istate) toggleButtonActive
      accchannel <- if checked
            then get (etAccChannel istate) entryText >>= return . Just
            else return $! Nothing
      
      autoaccept <- get (cbAutoAccept istate) toggleButtonActive
      
      -- place into data type using record syntax.
      return $! aconf
            { netHostname = nethostname
            , netRCWait = netrcwait
            
            , accUsername = accusername
            , accPassword = accpassword
            
            , accChannel = accchannel
            , autoAccept = autoaccept
            }


-- | Parse an rgb hex string.
toColor :: String -> String -> Color
toColor def rgb =
      case (parse rgbP [] rgb) of
            Right c -> c
            Left _ -> dc
      where Right dc = parse rgbP [] def


-- | Converts RGB hex strings to 'Color'.
rgbP :: Parser Color
rgbP = do
      _ <- char '#'
      r <- cParse
      g <- cParse
      b <- cParse
      eof
      return $! Color (cWord r) (cWord g) (cWord b)
      where cParse :: Parser String
            cParse = count 2 (satisfy isHexDigit)
            cWord = round . (*) 65535 . flip (/) 255 . (realToFrac :: Int -> Double). hexToInt



-- | (Re)create 'IConf' configuration data from configuration widgets.
getIConf :: IState -> IConf -> IO IConf
getIConf istate iconf = do
      -- only saves changes to options available in the GUI. duh.
      winsize <- windowGetSize (wMain istate)
      winpos <- windowGetPosition (wMain istate)
      
      -- runtime behaviour defaults.
      showjoinleave <- checkMenuItemGetActive (miShowJoinLeave istate)
      showsimplejoin <- checkMenuItemGetActive (miShowSimpleJoin istate)

      -- urgency hint settings.
      urgent_ <- checkMenuItemGetActive (miUrgent istate)
      urgentjoin <- checkMenuItemGetActive (miUrgentJoin istate)
      urgentleave <- checkMenuItemGetActive (miUrgentLeave istate)
      urgenttalk <- checkMenuItemGetActive (miUrgentTalk istate)
      urgentwhisper <- checkMenuItemGetActive (miUrgentWhisper istate)
      urgentgeneral <- checkMenuItemGetActive (miUrgentGeneral istate)
      
      -- logging settings
      logging_ <- checkMenuItemGetActive (miLogging istate)
      
      -- borderless window settings
      borderless_ <- checkMenuItemGetActive (miBorderless istate)
      
      -- place into data type using record syntax.
      return $! iconf
            { winSize = Just winsize
            , winPos = Just winpos
            
            , showJoinLeave = showjoinleave
            , showSimpleJoin = showsimplejoin
            
            , urgent = urgent_
            , urgentJoin = urgentjoin
            , urgentLeave = urgentleave
            , urgentTalk = urgenttalk
            , urgentWhisper = urgentwhisper
            , urgentGeneral = urgentgeneral
            
            , logging = logging_
            
            , borderless = borderless_
            }