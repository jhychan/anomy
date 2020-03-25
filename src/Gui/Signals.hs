{-# LANGUAGE CPP, ForeignFunctionInterface  #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
-- | Defines the computations to be run when the user activates various parts of the UI.
module Gui.Signals (
        initSignals
) where

import Auis.Reader (readMsg)
import Auis.Writer (exceptionMsg)
-- import Auis.Writer.Types (Auis)
import Core
      ( inc, writeTChanM
      , spawn, fileAConf, writeConf, getLogDir, writeLog, closeLog )
import Core.Types (AConf (..), AState (..))
import Gui.Conf
import Gui.Icons
-- import Gui.Widgets
import Gui.Types
import Util.List (splitByN2)
-- import Util.String ((===))

import Control.Concurrent (ThreadId, takeMVar, putMVar, isEmptyMVar)
import Control.Exception (IOException, catch)
import Control.Monad (when, void)
import Control.Monad.Trans (liftIO)
import Prelude hiding (catch)
import System.Process (createProcess, proc)

import Graphics.UI.Gtk
import qualified Graphics.UI.Gtk.Gdk.Events as DE -- deprecated events for textTags

#if mingw32_HOST_OS == 1
import Graphics.Win32.Window
import Graphics.Win32.GDI.Types
import System.Win32.Types
import Data.Bits ((.|.))

foreign import stdcall unsafe "windows.h SetWindowPos"
      c_setWindowPos :: HWND -> HWND -> INT -> INT -> INT -> INT -> SetWindowPosFlags -> IO Bool
#endif

initSignals :: IState -> IConf -> AState -> AConf -> IO ThreadId -> (ThreadId -> IO ()) -> IO ()
initSignals istate iconf astate aconf startCore stopCore =
      let
            chanauis = chanAuis astate
            chancore = chanCore astate
            wmain = wMain istate
      in do

      -- window handlers.
      after wmain deleteEvent $ tryEvent $ liftIO $ do
            saveExit istate iconf astate

      after wmain focusInEvent $ tryEvent $ liftIO $ do
            checkMenuItemSetActive (miWinFocused istate) True
            windowSetUrgencyHint wmain False
            widgetGrabFocus (etChat istate) -- automatically focus on chat entry on return

      after wmain focusOutEvent $ tryEvent $ liftIO $ do
            checkMenuItemSetActive (miWinFocused istate) False


      -- toolbar button handlers.
      afterToolButtonClicked (tbConnection istate) $ do
            -- COMMENT ME when testing NEW start/stop code
            buttonClicked (btStartStop istate)

            -- NEW start/stop code in testing starting here.

            -- NEW start/stop code in testing ending here.

      afterToolButtonClicked (tbConfiguration istate) $ do
            showconfig <- get (hbConfiguration istate) widgetVisible >>= return . not
            set (hbConfiguration istate) [ widgetVisible := showconfig ]

      afterToolButtonClicked (tbExit istate) $ do
            saveExit istate iconf astate



      -- configuration gui
      after (hbConfiguration istate) hideSignal $ do
            -- save settings to file
            aconfnew <- getAConf istate aconf
            writeConf fileAConf aconfnew
            -- refocus on chat entry
            widgetGrabFocus (etChat istate)
            editableSetPosition (etChat istate) (-1)

      after (cbAccChannel istate) toggled $ do
            checked <- get (cbAccChannel istate) toggleButtonActive
            set (etAccChannel istate) [ widgetSensitive := checked ]


      -- quick config menu
      mapM_ (\mi -> after mi checkMenuItemToggled $ getIConf istate iconf >>= writeConf fileIConf)
            [ miShowJoinLeave istate, miShowSimpleJoin istate
            , miUrgent istate, miUrgentJoin istate
            , miUrgentLeave istate, miUrgentTalk istate
            , miUrgentWhisper istate, miUrgentGeneral istate
            , miLogging istate ]

      
#if mingw32_HOST_OS == 1
      -- borderless window management
      after (miBorderless istate) checkMenuItemToggled $ do
            borderless_ <- checkMenuItemGetActive (miBorderless istate)
            makeBorderless (wMain istate) borderless_
      
      checkMenuItemEmitToggled (miBorderless istate)
#endif

      -- textview, textbuffer and texttag handlers.
      after (tbChat istate) bufferChanged (autoScroll istate)

      onAdjChanged (ajChat istate) $ do
            autoscroll <- checkMenuItemGetActive (miAutoScroll istate)
            when autoscroll $ do
                  upper <- adjustmentGetUpper (ajChat istate)
                  adjustmentClampPage (ajChat istate) upper upper

      afterValueChanged (ajChat istate) $ do
            value <- adjustmentGetValue (ajChat istate)
            upper <- adjustmentGetUpper (ajChat istate)
            psize <- adjustmentGetPageSize (ajChat istate)
            checkMenuItemSetActive (miAutoScroll istate) (value >= upper - psize)

      onTextTagEvent (ttURL istate) $ \event iter -> do
            catch (ttURLEvent event iter)
                  (writeTChanM chanauis . exceptionMsg :: IOException -> IO ())

      
      
      -- logging functionality
      loggingsignal <- afterBufferInsertText (tbChat istate) $ \_ -> \m -> do -- signaling monad api equivalent is broken for bufferInsertText
            writeLog (logHandle astate) m
      
      after (miLogging istate) checkMenuItemToggled $ do
            logging_ <- checkMenuItemGetActive (miLogging istate)
            if logging_
                  then signalUnblock loggingsignal
                  else signalBlock loggingsignal
      
      checkMenuItemEmitToggled (miLogging istate) -- trigger blocking/unblocking of logging signal as per the state of the check box
            
            
      -- open log directory
      on (miLoggingView istate) menuItemActivate $ do
            fp <- getLogDir
#if linux_HOST_OS == 1
            void $ createProcess (proc "sh" ["psexec", fp]) -- FIXME: Tie to config
#elif mingw32_HOST_OS == 1
            void $ createProcess (proc "shexec" [fp]) -- FIXME: Tie to config
#endif
      

      -- entry widget handlers.
      on (etChat istate) entryActivate $ do
            s <- entryGetText (etChat istate)
            mapM_ (writeTChanM chancore) (map (inc . readMsg) (splitByN2 244 (lines s)))
            entrySetText (etChat istate) []

      on (etChat istate) keyPressEvent $ do
            tryEvent (processKeys istate)
            return False


      -- list widget handlers.



      -- fake button (invisible). only using callback for flexible start-stop handler.
      on (btStartStop istate) buttonActivated $ do
            startStop istate startCore stopCore


      -- done. grab entry focus.
      toggleButtonToggled (cbAccChannel istate)
      widgetGrabFocus (etChat istate)



-- | Save GUI state and configuration to file and exit
saveExit :: IState -> IConf -> AState -> IO ()
saveExit istate iconf astate = do
      -- close logging
      closeLog (logHandle astate) (logFileName astate)
      -- unminimise if minimised
      windowDeiconify (wMain istate) -- ugliest UI hack ever, but will do for now...
#if mingw32_HOST_OS == 1
      -- restore titlebar to grab correct window size
      makeBorderless (wMain istate) False
#endif
      -- remember window position & size
      iconfupdated <- getIConf istate iconf
      writeConf fileIConf iconfupdated
      -- quit
      mainQuit


#if mingw32_HOST_OS == 1
-- | Set/unset the borderless window and cause window to redraw. MS Windows only.
makeBorderless :: Window -> Bool -> IO ()
makeBorderless w borderless_ = do
      hwnd <- widgetGetDrawWindow w >>= drawableGetID >>= return . fromNativeWindowId
      _ <- c_SetWindowLong hwnd (-16)
            (if borderless_
                  then 0x16870000
                  else 0x16CF0000)
      c_setWindowPos hwnd nullPtr 0 0 0 0
            (sWP_DRAWFRAME .|. sWP_NOSIZE .|. sWP_NOMOVE .|. sWP_NOZORDER .|. sWP_NOACTIVATE)
      return ()
#endif




-- | Autoscroll if appropriate. Called on particular signals. See initSignals.
autoScroll :: IState -> IO ()
autoScroll istate = do
      autoscroll <- checkMenuItemGetActive (miAutoScroll istate)
      when autoscroll $ do
            iter <- textBufferGetEndIter (tbChat istate)
            mark <- textBufferCreateMark (tbChat istate) Nothing iter True
            textViewScrollMarkOnscreen (tvChat istate) mark


startStop :: IState -> IO ThreadId -> (ThreadId -> IO ()) -> IO ()
startStop istate startCore stopCore = do
      corestopped <- isEmptyMVar (coreThread istate)
      if corestopped
            then do
                  -- thread anomy session.
                  corethread <- startCore
                  putMVar (coreThread istate) corethread

                  -- highlight toolbar icon highlights.
                  set (tbConnection istate) [ toolButtonStockId := Just stockAnomyConnectionHot ]
                  set (tbConfiguration istate) [ toolButtonStockId := Just stockAnomyConfigurationHot ]
                  set (tbExit istate) [ toolButtonStockId := Just stockAnomyExitHot ]

                  -- prep user to use ui.
                  set (etChat istate) [ widgetSensitive := True ]
                  widgetGrabFocus (etChat istate)
                  editableSetPosition (etChat istate) (-1)

            else do
                  -- stop the core thread
                  corethread <- takeMVar (coreThread istate)
                  _ <- spawn $ stopCore corethread

                  -- dim toolbar icons.
                  set (tbConnection istate) [ toolButtonStockId := Just stockAnomyConnectionCold ]
                  set (tbConfiguration istate) [ toolButtonStockId := Just stockAnomyConfigurationCold ]
                  set (tbExit istate) [ toolButtonStockId := Just stockAnomyExitCold ]

                  -- alter parts of ui.
                  set (etChat istate) [ widgetSensitive := False ]



ttURLEvent :: DE.Event -> TextIter -> IO ()
ttURLEvent (DE.Button {DE.eventClick = DE.ReleaseClick}) siter = do
      beginstag <- textIterBeginsTag siter Nothing
      when (not beginstag) (void (textIterBackwardToTagToggle siter Nothing))
      eiter <- textIterCopy siter
      _ <- textIterForwardToTagToggle eiter Nothing
      url <- textIterGetText siter eiter
#if linux_HOST_OS == 1
      void $ createProcess (proc "sh" ["psexec", url]) -- FIXME: Tie to config
#elif mingw32_HOST_OS == 1
      void $ createProcess (proc "shexec" [url]) -- FIXME: Tie to config
#endif
ttURLEvent _ _ = return ()


processKeys :: IState -> EventM EKey ()
processKeys istate = do
      -- replace /r with /whisper <last whisperer>
      Just ' ' <- eventKeyVal >>= return . keyToChar
      "/r" <- liftIO $ get (etChat istate) entryText
      True <- liftIO $ get (cbLastWhisperer istate) toggleButtonActive
      lastwhisperer <- liftIO $ get (cbLastWhisperer istate) buttonLabel
      liftIO $ set (etChat istate) [ entryText := "/w " ++ lastwhisperer ]
      liftIO $ editableSetPosition (etChat istate) (-1)