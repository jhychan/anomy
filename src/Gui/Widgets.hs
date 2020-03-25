{-# LANGUAGE CPP #-}
-- | Widget drawing and redrawing.
module Gui.Widgets (
        initMainWidgets
      , initChatWidgets
      , initListWidgets
      , initConfWidgets
) where

import Auis.Writer.Types (Users (..), Member (..))
import Gui.Conf
import Gui.Icons
import Gui.Types

import Graphics.UI.Gtk


-- | Main window.
initMainWidgets :: IState -> IO IState
initMainWidgets istate = do
      -- overall window.
      wmain <- windowNew
      set wmain
            [ windowTitle := "Anomy"
            , windowResizable := True
            , windowWindowPosition := WinPosCenter
            , windowDefaultWidth := 960
            , windowDefaultHeight := 448
            , windowAllowShrink := True
            , windowGravity := GravityStatic ]
      
      -- 2-row vertical box layout separating toolbar and configuration/chat+list display.
      vbmain <- vBoxNew False 0

      -- 2-column horizontal resizable layout for separating chat and list display.
      hbmain <- hBoxNew False 0

      -- toolbar and friends.
      tbmain <- toolbarNew
      set tbmain
            [ toolbarStyle := ToolbarBothHoriz
            , widgetHeightRequest := 32 ]
      
      iconAddAnomyIds -- add anomy toolbar icons to stock icon set
      
      lblconnection <- labelNew (Just "Connection")
      widgetSetSizeRequest lblconnection 128 24
      tbconnection <- toolButtonNewFromStock stockAnomyConnectionCold
      set tbconnection
            [ toolButtonLabelWidget := Just lblconnection
            , toolItemExpand := True
            , toolItemHomogeneous := True
            , toolItemIsImportant := True ]

      tbsep0 <- separatorToolItemNew
      
      lblconfiguration <- labelNew (Just "Configuration")
      widgetSetSizeRequest lblconfiguration 128 24
      tbconfigure <- menuToolButtonNewFromStock stockAnomyConfigurationCold
      set tbconfigure
            [ toolButtonLabelWidget := Just lblconfiguration
            , toolItemExpand := True
            , toolItemHomogeneous := True
            , toolItemIsImportant := True ]

      tbsep1 <- separatorToolItemNew
      
      lblexit <- labelNew (Just "Exit")
      widgetSetSizeRequest lblexit 128 24
      tbexit <- toolButtonNewFromStock stockAnomyExitCold
      set tbexit
            [ toolButtonLabelWidget := Just lblexit
            , toolItemExpand := True
            , toolItemHomogeneous := True
            , toolItemIsImportant := True ]

      containerAdd tbmain tbconnection
      containerAdd tbmain tbsep0
      containerAdd tbmain tbconfigure
      containerAdd tbmain tbsep1
      containerAdd tbmain tbexit

      -- quick config menu
      mconfigure <- menuNew
      menuToolButtonSetMenu tbconfigure (Just mconfigure)

      mconfigurejoinleave <- menuNew
      miconfigurejoinleave <- menuItemNewWithLabel "Channel Notifications  "
      mishowjoinleave <- checkMenuItemNewWithLabel "Enable join-leave messages  "
      mishowsimplejoin <- checkMenuItemNewWithLabel "Use simple join messages  "
      menuItemSetSubmenu miconfigurejoinleave mconfigurejoinleave
      mapM_ (menuShellAppend mconfigurejoinleave)
            [ mishowjoinleave, mishowsimplejoin ]
      misep00 <- separatorMenuItemNew
      menuShellInsert mconfigurejoinleave misep00 1

      mconfigureurgent <- menuNew
      miconfigureurgent <- menuItemNewWithLabel "Taskbar Notifications  "
      miurgent <- checkMenuItemNewWithLabel "Enable taskbar flashing  "
      miurgentjoin <- checkMenuItemNewWithLabel "Flash taskbar on channel join  "
      miurgentleave <- checkMenuItemNewWithLabel "Flash taskbar on channel leave  "
      miurgenttalk <- checkMenuItemNewWithLabel "Flash taskbar on chat and emote  "
      miurgentwhisper <- checkMenuItemNewWithLabel "Flash taskbar on whisper  "
      miurgentgeneral <- checkMenuItemNewWithLabel "Flash taskbar on announce  "
      menuItemSetSubmenu miconfigureurgent mconfigureurgent
      mapM_ (menuShellAppend mconfigureurgent)
            [ miurgent, miurgentjoin, miurgentleave, miurgenttalk
            , miurgentwhisper, miurgentgeneral ]
      misep10 <- separatorMenuItemNew
      menuShellInsert mconfigureurgent misep10 1

      mconfigurelogging <- menuNew
      miconfigurelogging <- menuItemNewWithLabel "Chat Logging  "
      milogging <- checkMenuItemNewWithLabel "Enable logging  "
      miloggingview <- menuItemNewWithLabel "View logs folder  "
      menuItemSetSubmenu miconfigurelogging mconfigurelogging
      menuShellAppend mconfigurelogging milogging
      misep20 <- separatorMenuItemNew
      menuShellAppend mconfigurelogging misep20
      menuShellAppend mconfigurelogging miloggingview
      
      miborderless <- checkMenuItemNewWithLabel "Borderless Window  "
      
      mapM_ (menuShellAppend mconfigure)
            [ miconfigurejoinleave, miconfigureurgent, miconfigurelogging ]
      misep0 <- separatorMenuItemNew
      menuShellInsert mconfigure misep0 2
      misep1 <- separatorMenuItemNew
      menuShellInsert mconfigure misep1 4
#if mingw32_HOST_OS == 1
      menuShellAppend mconfigure miborderless
#endif

      -- put widgets where they belong.
      containerAdd wmain vbmain
      boxPackStart vbmain tbmain PackNatural 0

      -- draw it now because we can't see it.
      widgetShowAll mconfigure

      -- invisible button. see 'Gui.Signals'.
      btstartstop <- buttonNew
      
      -- last whisperer mutable variable (as invisible checkbutton)
      cblastwhisperer <- checkButtonNew

      -- return updated state.
      return $! istate
            { wMain = wmain
            , vbMain = vbmain
            , hbMain = hbmain

            , btStartStop = btstartstop
            , cbLastWhisperer = cblastwhisperer

            , tbConnection = tbconnection
            , tbConfiguration = tbconfigure
            , tbExit = tbexit

            , miShowJoinLeave = mishowjoinleave
            , miShowSimpleJoin = mishowsimplejoin
            , miUrgent = miurgent
            , miUrgentJoin = miurgentjoin
            , miUrgentLeave = miurgentleave
            , miUrgentTalk = miurgenttalk
            , miUrgentWhisper = miurgentwhisper
            , miUrgentGeneral = miurgentgeneral
            , miLogging = milogging
            , miLoggingView = miloggingview
            , miBorderless = miborderless
            }


-- | Stub for configuration widgets.
initConfWidgets :: IState -> IO IState
initConfWidgets istate = do
      -- columnal box dividing configuration categories and settings.
      hbconfiguration <- hBoxNew True (-6)
      
      -- Connection settings
      fmconnection <- frameNew
      set fmconnection
            [ frameLabel := "Connection"
            , frameLabelXAlign := 0.02 ]
      
      -- House option fields in a tabular box
      tbconnection <- tableNew 2 2 False
      
      lbnetserver <- labelNew (Just "PvPGN Server:")
      etnetserver <- entryNew
      lbnetrcwait <- labelNew (Just "R/C Interval (s):")
      etnetrcwait <- entryNew

      mapM_ (\lb -> set lb [ miscXalign := 1 ]) [ lbnetserver, lbnetrcwait ]
      mapM_ (\w -> set w [ widgetHeightRequest := 20, entryWidthChars := 3 ])
            [ etnetserver, etnetrcwait ]

      tableAttach tbconnection lbnetserver 0 1 0 1 [Fill] [Fill] 0 0
      tableAttachDefaults tbconnection etnetserver 1 2 0 1
      tableAttach tbconnection lbnetrcwait 0 1 1 2 [Fill] [Fill] 0 0
      tableAttachDefaults tbconnection etnetrcwait 1 2 1 2
      
      
      -- Account settings
      fmaccount <- frameNew
      set fmaccount
            [ frameLabel := "Account"
            , frameLabelXAlign := 0.02 ]
      
      -- House option fields in a tabular box
      tbaccount <- tableNew 2 2 False
      
      lbaccusername <- labelNew (Just "Username:")
      etaccusername <- entryNew
      lbaccpassword <- labelNew (Just "Password:")
      etaccpassword <- entryNew
      
      mapM_ (\w -> set w [ miscXalign := 1 ]) [ lbaccusername, lbaccpassword ]
      mapM_ (\w -> set w [ widgetHeightRequest := 20, entryWidthChars := 3 ])
            [ etaccusername, etaccpassword ]
      set etaccpassword [ entryVisibility := False ]
      
      tableAttach tbaccount lbaccusername 0 1 0 1 [Fill] [Fill] 0 0
      tableAttachDefaults tbaccount etaccusername 1 2 0 1
      tableAttach tbaccount lbaccpassword 0 1 1 2 [Fill] [Fill] 0 0
      tableAttachDefaults tbaccount etaccpassword 1 2 1 2
      
      
      -- Behavioural settings
      fmbehaviour <- frameNew
      set fmbehaviour
            [ frameLabel := "Behaviour"
            , frameLabelXAlign := 0.02 ]
      
      -- House option fields in a tabular box
      tbbehaviour <- tableNew 2 2 False
      
      cbaccchannel <- checkButtonNewWithLabel "Join custom channel at login:"
      etaccchannel <- entryNew
      cbautoaccept <- checkButtonNewWithLabel "Automatically accept or reject clan invitations"
      
      mapM_ (\w -> set w [ widgetHeightRequest := 20, entryWidthChars := 3 ]) [ etaccchannel ]
            
      tableAttach tbbehaviour cbaccchannel 0 1 0 1 [Fill] [Fill] 0 0
      tableAttachDefaults tbbehaviour etaccchannel 1 2 0 1
      tableAttach tbbehaviour cbautoaccept 0 2 1 2 [Fill] [Fill] 0 0
      
      -- Tweak tabular boxes
      mapM_ (\tb -> set tb
            [ containerBorderWidth := 4
            , tableRowSpacing := 2
            , tableColumnSpacing := 4 ])
            [ tbconnection, tbaccount, tbbehaviour ]
      
      -- put widgets where they belong.
      containerAdd fmconnection tbconnection
      containerAdd fmaccount tbaccount
      containerAdd fmbehaviour tbbehaviour
      
      boxPackStart hbconfiguration fmconnection PackGrow 5
      boxPackStart hbconfiguration fmaccount PackGrow 5
      boxPackStart hbconfiguration fmbehaviour PackGrow 5
      
      boxPackStart (vbMain istate) hbconfiguration PackNatural 4
      
      return $! istate
             { hbConfiguration = hbconfiguration
             , etNetServer = etnetserver
             , etNetRCWait = etnetrcwait
             , etAccUsername = etaccusername
             , etAccPassword = etaccpassword
             , cbAccChannel = cbaccchannel
             , etAccChannel = etaccchannel
             , cbAutoAccept = cbautoaccept
             }


-- | Draw the chat input and output display area.
initChatWidgets :: IState -> IO IState
initChatWidgets istate =
      let vbmain = vbMain istate
          hbmain = hbMain istate in do
      -- 2-row vertical layout for separating chat textview and chat entry.
      vbchat <- vBoxNew False 0

      -- chat textview and friends.
      swchat <- scrolledWindowNew Nothing Nothing
      set swchat
            [ scrolledWindowShadowType := ShadowIn
            , scrolledWindowHscrollbarPolicy := PolicyNever
            , scrolledWindowVscrollbarPolicy := PolicyAutomatic ]
      ajchat <- scrolledWindowGetVAdjustment swchat

      ttchat <- textTagTableNew
      tttime <- textTagMake ttchat (fgColourTime newIConf)
      ttusername <- textTagMake ttchat (fgColourUsername newIConf)
      ttnormal <- textTagMake ttchat (fgColourNormal newIConf)
      ttadmin <- textTagMake ttchat (fgColourAdmin newIConf)
      ttwhisper <- textTagMake ttchat (fgColourWhisper newIConf)
      ttemote <- textTagMake ttchat (fgColourEmote newIConf)
      tterror <- textTagMake ttchat (fgColourError newIConf)
      ttgeneral <- textTagMake ttchat (fgColourGeneral newIConf)
      ttinternal <- textTagMake ttchat (fgColourInternal newIConf)
      ttspecial <- textTagMake ttchat (fgColourSpecial newIConf)
      tturl <- textTagMake ttchat (fgColourURL newIConf)

      tbchat <- textBufferNew (Just ttchat)
      tvchat <- textViewNew
      set tvchat
            [ textViewPixelsAboveLines := 2
            , textViewPixelsBelowLines := 2
            , textViewEditable := False
            , textViewWrapMode := WrapWord
            , textViewLeftMargin := 4
            , textViewRightMargin := 4
            , textViewBuffer := tbchat ]

      -- chat entry and friends.
      etchat <- entryNew
--       ecChat <- entryCompletionNew
--       entrySetCompletion etChat ecChat
      set etchat
            -- [ entryEditable := False
            [ widgetSensitive := False
            , widgetHeightRequest := 26 ]

      -- put widgets where they belong.
      boxPackStart vbmain hbmain PackGrow 0
      boxPackStart hbmain vbchat PackGrow 0

      boxPackStart vbchat swchat PackGrow 0
      boxPackStart vbchat etchat PackNatural 0

      containerAdd swchat tvchat

      -- return updated state.
      return $! istate
            { ajChat = ajchat
            , tvChat = tvchat
            , tbChat = tbchat
            , ttChat = ttchat
            , etChat  = etchat

            , ttTime = tttime
            , ttUsername = ttusername
            , ttNormal = ttnormal
            , ttAdmin = ttadmin
            , ttWhisper = ttwhisper
            , ttEmote = ttemote
            , ttError = tterror
            , ttGeneral = ttgeneral
            , ttInternal = ttinternal
            , ttSpecial = ttspecial
            , ttURL = tturl
            }


-- | Draw the list area widgets.
initListWidgets :: IState -> IO IState
initListWidgets istate =
      let hbmain = hbMain istate in do
      -- notebook layout for housing the lists.
      nblist <- notebookNew
      set nblist
            [ notebookTabPos := PosBottom
            , notebookTabVborder := 1
            , notebookShowBorder := False
            , notebookHomogeneous := True
            , widgetWidthRequest := 40 + 16 + 100 + 50 + 35 ]
            -- widget Width depends on:
            --   User icon width
            --   User level text
            --   User name text
            --   User clan tag
            --   Extra space

      -- map of icon pixbufs.
      pmlist <- iconPixbufMap

      -- list views for channel user listing.
      swchannel <- scrolledWindowNew Nothing Nothing
      set swchannel
            [ scrolledWindowShadowType := ShadowNone
            , scrolledWindowHscrollbarPolicy := PolicyNever
            , scrolledWindowVscrollbarPolicy := PolicyAutomatic ]

      tmchannel <- listStoreNew []
      tmschannel <- treeModelSortNewWithModel tmchannel
      treeSortableSetDefaultSortFunc tmschannel . Just $
            \iter1 iter2 -> do
                  user1 <- treeModelGetRow tmchannel iter1
                  user2 <- treeModelGetRow tmchannel iter2
                  return $! compare user1 user2

      tvchannel <- treeViewNewWithModel tmschannel
      set tvchannel
            [ treeViewHeadersVisible := False
            , treeViewHoverSelection := True
            , treeViewShowExpanders := False ]

      tvcchannel <- treeViewColumnNew
      _ <- treeViewAppendColumn tvchannel tvcchannel

      crchannelicon <- cellRendererPixbufNew
      crchannellevel <- cellRendererTextNew
      crchannelusername <- cellRendererTextNew
      crchanneltag <- cellRendererTextNew

      treeViewColumnPackStart tvcchannel crchannelicon False
      treeViewColumnPackStart tvcchannel crchannellevel False
      treeViewColumnPackStart tvcchannel crchannelusername False
      treeViewColumnPackStart tvcchannel crchanneltag False

      cellLayoutSetAttributeFunc tvcchannel crchannelicon tmschannel $
            \tmsiter -> do
                  tmiter <- treeModelSortConvertIterToChildIter tmschannel tmsiter
                  user <- treeModelGetRow tmchannel tmiter
                  set crchannelicon
                        [ cellPixbuf := iconPixbuf pmlist (iconName user)
                        , cellXAlign := 0.50
                        , cellWidth := 40 ]
      cellLayoutSetAttributeFunc tvcchannel crchannellevel tmschannel $
            \tmsiter -> do
                  tmiter <- treeModelSortConvertIterToChildIter tmschannel tmsiter
                  user <- treeModelGetRow tmchannel tmiter
                  set crchannellevel
                        [ cellText :=
                              case uLevel user of
                                    "0" -> []
                                    lvl -> lvl
                        , cellXAlign := 0.95
                        , cellWidth := 16 ]
      cellLayoutSetAttributeFunc tvcchannel crchannelusername tmschannel $
            \tmsiter -> do
                  tmiter <- treeModelSortConvertIterToChildIter tmschannel tmsiter
                  user <- treeModelGetRow tmchannel tmiter
                  set crchannelusername
                        [ cellText := uUsername user
                        , cellXAlign := 0.00
                        , cellWidth := 100 ]
      cellLayoutSetAttributeFunc tvcchannel crchanneltag tmschannel $
            \tmsiter -> do
                  tmiter <- treeModelSortConvertIterToChildIter tmschannel tmsiter
                  user <- treeModelGetRow tmchannel tmiter
                  set crchanneltag
                        [ cellText := uTag user
                        , cellXAlign := 0.95
                        , cellWidth := 50 ]

      -- list views for clan member listing.
      swclan <- scrolledWindowNew Nothing Nothing
      set swclan
            [ scrolledWindowShadowType := ShadowEtchedIn
            , scrolledWindowHscrollbarPolicy := PolicyNever
            , scrolledWindowVscrollbarPolicy := PolicyAutomatic ]

      tmclan <- listStoreNew []
      tmsclan <- treeModelSortNewWithModel tmclan
      treeSortableSetDefaultSortFunc tmsclan . Just $
            \iter1 iter2 -> do
                  member1 <- treeModelGetRow tmclan iter1
                  member2 <- treeModelGetRow tmclan iter2
                  return $! compare member1 member2

      tvclan <- treeViewNewWithModel tmsclan
      set tvclan
            [ treeViewHeadersVisible := False
            , treeViewHoverSelection := True
            , treeViewShowExpanders := False ]

      tvcclan <- treeViewColumnNew
      _ <- treeViewAppendColumn tvclan tvcclan

      crclanicon <- cellRendererPixbufNew
      crclanpresence <- cellRendererPixbufNew
      crclanusername <- cellRendererTextNew

      treeViewColumnPackStart tvcclan crclanicon False
      treeViewColumnPackStart tvcclan crclanpresence False
      treeViewColumnPackStart tvcclan crclanusername False

      cellLayoutSetAttributeFunc tvcclan crclanicon tmsclan $
            \tmsiter -> do
                  tmiter <- treeModelSortConvertIterToChildIter tmsclan tmsiter
                  member <- treeModelGetRow tmclan tmiter
                  set crclanicon
                        [ cellPixbuf := iconPixbuf pmlist (iconName member)
                        , cellXAlign := 0.50
                        , cellWidth := 40 ]
      cellLayoutSetAttributeFunc tvcclan crclanpresence tmsclan $
            \tmsiter -> do
                  tmiter <- treeModelSortConvertIterToChildIter tmsclan tmsiter
                  member <- treeModelGetRow tmclan tmiter
                  set crclanpresence
                        [ cellPixbuf := iconPixbuf pmlist (iconName (cPresence member))
                        , cellXAlign := 0.10
                        , cellWidth := 20 ]
      cellLayoutSetAttributeFunc tvcclan crclanusername tmsclan $
            \tmsiter -> do
                  tmiter <- treeModelSortConvertIterToChildIter tmsclan tmsiter
                  member <- treeModelGetRow tmclan tmiter
                  set crclanusername
                        [ cellText := cUsername member
                        , cellXAlign := 0.00
                        , cellWidth := 146 ]

      -- put widgets where they belong.
      boxPackStart hbmain nblist PackNatural 0

      _ <- notebookAppendPage nblist swchannel "no channel"
      set nblist [ notebookChildTabPacking swchannel := PackGrow ]
      -- Just nbtablabel <- notebookGetTabLabel nblist swchannel
      -- set (castToLabel nbtablabel) [ labelSingleLineMode := True
                                   -- , labelEllipsize := EllipsizeEnd ]
      
      _ <- notebookAppendPage nblist swclan "no clan"
      set nblist [ notebookChildTabPacking swclan := PackGrow ]
      -- Just nbtablabel <- notebookGetTabLabel nblist swclan
      -- set (castToLabel nbtablabel) [ labelSingleLineMode := True
                                   -- , labelEllipsize := EllipsizeEnd ]
      
      containerAdd swchannel tvchannel
      containerAdd swclan tvclan

      -- return updated state.
      return $! istate
            { nbList = nblist
            , swChannel = swchannel
            , tvChannel = tvchannel
            , tmChannel = tmchannel
            , crChannelLevel = crchannellevel
            , crChannelTag = crchanneltag
            , swClan = swclan
            , tvClan = tvclan
            , tmClan = tmclan
            }



-- | Helper function to create text tags with multiple attributes and add them to the given 'TextTagTable' in one call.
textTagMake :: TextTagTable -> String -> IO TextTag
textTagMake ttchat fgc = do
      ttag <- textTagNew Nothing
      set ttag [ textTagForeground := fgc ]
      textTagTableAdd ttchat ttag
      return ttag
