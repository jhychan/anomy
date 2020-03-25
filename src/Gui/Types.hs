-- | State and config data types for the GUI.
module Gui.Types (
        IConf (..)
      , IState (..)
) where

import Auis.Writer.Types (Users (..), Member (..))

import Control.Concurrent (ThreadId, MVar)
import Graphics.UI.Gtk


-- | (User) Interface configuration. Mostly read-only configuration options which govern appearance, as well as provide
-- default state attributes that affect the behaviour of the GUI.
data IConf = IConf
      { -- Window properties.
        winSize               :: !(Maybe (Int, Int))
      , winPos                :: !(Maybe (Int, Int))

        -- Runtime options.
      , autoStart             :: !Bool
      , showJoinLeave         :: !Bool
      , showSimpleJoin        :: !Bool

      , urgent                :: !Bool
      , urgentJoin            :: !Bool
      , urgentLeave           :: !Bool
      , urgentTalk            :: !Bool
      , urgentWhisper         :: !Bool
      , urgentGeneral         :: !Bool
      
      , logging               :: !Bool
      
      , borderless            :: !Bool

        -- Input widget and list widget colours.
      , bgColourEntry         :: !String
      , fgColourEntry         :: !String
      , bgColourList          :: !String
      , fgColourList          :: !String
      , fgColourListLevel     :: !String
      , fgColourListTag       :: !String

        -- Chat output widget appearance.
      , bgColourChat          :: !String
      , fgColourTime          :: !String
      , fgColourUsername      :: !String
      , fgColourNormal        :: !String
      , fgColourAdmin         :: !String
      , fgColourWhisper       :: !String
      , fgColourEmote         :: !String
      , fgColourError         :: !String
      , fgColourGeneral       :: !String
      , fgColourInternal      :: !String
      , fgColourSpecial       :: !String
      , fgColourURL           :: !String
      }
      deriving (Read, Show)


-- | (User) Interface state. Actually a record of pointers to gtk's mutable GUI objects, as well as other thread-shared
-- variables which affect runtime behaviour.
data IState = IState
      { -- Main window widgets.
        wMain                 :: Window
      , vbMain                :: VBox
      , hbMain                :: HBox

        -- Specialised widgets.
      , btStartStop           :: Button
      , cbLastWhisperer       :: CheckButton

        -- Toolbar buttons.
      , tbConnection          :: ToolButton
      , tbConfiguration       :: MenuToolButton
      , tbExit                :: ToolButton

        -- Menu items
      , miShowJoinLeave       :: CheckMenuItem
      , miShowSimpleJoin      :: CheckMenuItem
      , miUrgent              :: CheckMenuItem
      , miUrgentJoin          :: CheckMenuItem
      , miUrgentLeave         :: CheckMenuItem
      , miUrgentTalk          :: CheckMenuItem
      , miUrgentWhisper       :: CheckMenuItem
      , miUrgentGeneral       :: CheckMenuItem
      , miLogging             :: CheckMenuItem
      , miLoggingView         :: MenuItem
      , miBorderless          :: CheckMenuItem

        -- Configuration area widgets.
      , hbConfiguration       :: HBox
      , etNetServer           :: Entry
      , etNetRCWait           :: Entry
      , etAccUsername         :: Entry
      , etAccPassword         :: Entry
      , cbAccChannel          :: CheckButton
      , etAccChannel          :: Entry
      , cbAutoAccept          :: CheckButton

        -- Chat area widgets.
      , ajChat                :: Adjustment
      , tvChat                :: TextView
      , tbChat                :: TextBuffer
      , ttChat                :: TextTagTable
      , etChat                :: Entry

        -- List area widgets.
      , nbList                :: Notebook
      , swChannel             :: ScrolledWindow
      , tvChannel             :: TreeView
      , tmChannel             :: ListStore Users
      , crChannelLevel        :: CellRendererText
      , crChannelTag          :: CellRendererText
      , swClan                :: ScrolledWindow
      , tvClan                :: TreeView
      , tmClan                :: ListStore Member

        -- Text tags.
      , ttTime                :: TextTag
      , ttUsername            :: TextTag
      , ttNormal              :: TextTag
      , ttAdmin               :: TextTag
      , ttWhisper             :: TextTag
      , ttEmote               :: TextTag
      , ttError               :: TextTag
      , ttGeneral             :: TextTag
      , ttInternal            :: TextTag
      , ttSpecial             :: TextTag
      , ttURL                 :: TextTag

        -- Runtime resources.
      , coreThread            :: MVar ThreadId
      , miAutoScroll          :: CheckMenuItem
      , miWinFocused          :: CheckMenuItem
      }
