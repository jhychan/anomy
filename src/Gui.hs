-- | Interface module for realising a full Anomy GUI.
module Gui (
        initGui
      , initSignals
      , module Gui.Conf
      , module Gui.Writer
      , module Gui.Types
      , module Graphics.UI.Gtk
) where

import Gui.Conf
import Gui.Signals
import Gui.Widgets
import Gui.Writer
import Gui.Types

import Control.Concurrent (newEmptyMVar)
import Graphics.UI.Gtk


-- | Wrapper to initialise gui widgets, state and configuration.
initGui :: IO IState
initGui = do
      -- init gtk lib.
      _ <- unsafeInitGUIForThreadedRTS
      newState
            >>= initMainWidgets
            >>= initConfWidgets
            >>= initChatWidgets
            >>= initListWidgets


newState :: IO IState
newState = do
      corethread <- newEmptyMVar
      miautoscroll <- checkMenuItemNew
      miwinfocused <- checkMenuItemNew
      checkMenuItemSetActive miautoscroll True
      checkMenuItemSetActive miwinfocused True
      return $! IState
            { wMain = undefined
            , vbMain = undefined
            , hbMain = undefined

            , btStartStop = undefined
            , cbLastWhisperer = undefined

            , tbConnection = undefined
            , tbConfiguration = undefined
            , tbExit = undefined

            , miShowJoinLeave = undefined
            , miShowSimpleJoin = undefined
            , miUrgent = undefined
            , miUrgentJoin = undefined
            , miUrgentLeave = undefined
            , miUrgentTalk = undefined
            , miUrgentWhisper = undefined
            , miUrgentGeneral = undefined
            , miLogging = undefined
            , miLoggingView = undefined
            , miBorderless = undefined

            , hbConfiguration = undefined
            , etNetServer = undefined
            , etNetRCWait = undefined
            , etAccUsername = undefined
            , etAccPassword = undefined
            , cbAccChannel = undefined
            , etAccChannel = undefined
            , cbAutoAccept = undefined

            , ajChat = undefined
            , tvChat = undefined
            , tbChat = undefined
            , ttChat = undefined
            , etChat = undefined

            , nbList = undefined
            , swChannel = undefined
            , tvChannel = undefined
            , tmChannel = undefined
            , crChannelLevel = undefined
            , crChannelTag = undefined
            , swClan = undefined
            , tvClan = undefined
            , tmClan = undefined

            , ttTime = undefined
            , ttUsername = undefined
            , ttNormal = undefined
            , ttAdmin = undefined
            , ttWhisper = undefined
            , ttEmote = undefined
            , ttError = undefined
            , ttGeneral = undefined
            , ttInternal = undefined
            , ttSpecial = undefined
            , ttURL = undefined

            , coreThread = corethread
            , miAutoScroll = miautoscroll
            , miWinFocused = miwinfocused
            }
