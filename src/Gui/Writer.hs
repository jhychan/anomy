-- | Gui writer thread function.
module Gui.Writer (
        writer
) where

import Auis.Writer.Types
import Core
      ( TChan, readTChanIO
      , showPing, showClient, showIcon, showLevel, showTag )
import Gui.Types
import Util.String (takeFstS, takeSndS, (===))

import Control.Monad (when)
import Data.List (findIndex)
-- import Data.Maybe (fromJust)
import System.Locale (defaultTimeLocale)
import System.Time (getClockTime, toCalendarTime, formatCalendarTime)
import Text.Regex.Posix ((=~))

import Graphics.UI.Gtk


writer :: IState -> TChan Auis -> IO ()
writer istate chanauis = do
      m <- readTChanIO chanauis
      postGUIAsync (writeMsg istate m)
      writer istate chanauis


writeMsg :: IState -> Auis -> IO ()
writeMsg istate (PrintTalk admin username message) = do
      -- time username: message
      writeTextBufferTime istate
      textBufferPut (tbChat istate) (ttUsername istate) (showString username ": ")
      writeTextBufferMsg istate (ifConst admin ttAdmin ttNormal) (filter isURL (words message)) message

      -- urgency hinting
      urgenttalk <- checkMenuItemGetActive (miUrgentTalk istate)
      when urgenttalk (windowSetUrgencyHintIf istate)

writeMsg istate (PrintWhisper sent admin username message) = do
      -- time whisperer: message
      writeTextBufferTime istate
      textBufferPut (tbChat istate) (ttUsername istate) $
            ifConst sent (showString "You whisper to " (showString username ": ")) (showString username " whispers: ")
      writeTextBufferMsg istate (ifConst admin ttAdmin ttWhisper) (filter isURL (words message)) message

      -- update last whisperer if applicable
      when (not sent) (set (do
            cbLastWhisperer istate)
                  [ toggleButtonActive := True
                  , buttonLabel := username ])

      -- urgency hinting
      urgentwhisper <- checkMenuItemGetActive (miUrgentWhisper istate)
      when urgentwhisper (windowSetUrgencyHintIf istate)

writeMsg istate (PrintEmote username message) = do
      -- time emote message
      writeTextBufferTime istate
      writeTextBufferMsg istate ttEmote (filter isURL (words message)) (showString username (showString " " message))

      -- urgency hinting
      urgenttalk <- checkMenuItemGetActive (miUrgentTalk istate)
      when urgenttalk (windowSetUrgencyHintIf istate)

writeMsg istate (PrintError message) = do
      -- time error message
      writeTextBufferTime istate
      writeTextBufferMsg istate ttError (filter isURL (words message)) message

      -- urgency hinting
      windowSetUrgencyHintIf istate

writeMsg istate (PrintGeneral message) = do
      -- time message
      writeTextBufferTime istate
      writeTextBufferMsg istate ttGeneral (filter isURL (words message)) message

      -- urgency hinting
      urgentgeneral <- checkMenuItemGetActive (miUrgentGeneral istate)
      when urgentgeneral (windowSetUrgencyHintIf istate)

writeMsg istate (PrintInternal message) = do
      -- time message
      writeTextBufferTime istate
      textBufferPut (tbChat istate) (ttInternal istate) (showString message "\n")

writeMsg istate (PrintSpecial message) = do
      -- time message
      writeTextBufferTime istate
      textBufferPut (tbChat istate) (ttSpecial istate) (showString message "\n")

writeMsg istate (AddChannelUser joined flag ping username client icon level tag) = do
      -- add to channel list
      listStorePrepend (tmChannel istate) (Users flag ping username client icon level tag)

      -- user join message
      showjoinleave <- checkMenuItemGetActive (miShowJoinLeave istate)
      when (joined && showjoinleave) $ do
            showsimplejoin <- checkMenuItemGetActive (miShowSimpleJoin istate)
            writeTextBufferTime istate
            textBufferPut (tbChat istate) (ttInternal istate) $
                  ifConst showsimplejoin
                        (shows flag " " ++ username ++ " (" ++ showTag tag ++ ") joined the channel - " ++ showClient client ++ " (" ++ showPing ping ++ ").\n")
                        (shows flag " " ++ username ++ " joined the channel using " ++ showClient client ++ " (" ++ showPing ping ++ ", " ++ showIcon icon ++ ", " ++ showLevel level ++ ", " ++ showTag tag ++ ").\n")

      -- urgency hinting
      urgentjoin <- checkMenuItemGetActive (miUrgentJoin istate)
      when urgentjoin (windowSetUrgencyHintIf istate)

writeMsg istate (RemoveChannelUser flag _ username) = do
      -- remove from channel list
      Just midx <- listStoreFind (tmChannel istate) uUsername username
      listStoreRemove (tmChannel istate) midx

      -- leave message
      showjoinleave <- checkMenuItemGetActive (miShowJoinLeave istate)
      when showjoinleave $ do
            writeTextBufferTime istate
            textBufferPut (tbChat istate) (ttInternal istate) (shows flag " " ++ username ++ " left the channel.\n")

      -- urgency hinting
      urgentleave <- checkMenuItemGetActive (miUrgentLeave istate)
      when urgentleave (windowSetUrgencyHintIf istate)

writeMsg istate (UpdateChannelUser flag ping username client icon level tag) = do
      -- update stored element
      midx <- listStoreFind (tmChannel istate) uUsername username
      case midx of
            Nothing -> return ()
            Just idx -> listStoreSetValue (tmChannel istate) idx (Users flag ping username client icon level tag)

writeMsg istate (ClearChannelUsers channel) = do
      -- empty list store.
      listStoreClear (tmChannel istate)

      -- time stamp join message: channel
      writeTextBufferTime istate
      textBufferPut (tbChat istate) (ttNormal istate) "Joining Channel: "
      textBufferPut (tbChat istate) (ttUsername istate) (showString channel "\n")

writeMsg istate (ShowChannelInfo channel count) = do
      -- update notebook tab label
      case count of
            0 -> notebookSetTabLabelText (nbList istate) (swChannel istate) channel
            _ -> notebookSetTabLabelText (nbList istate) (swChannel istate) (channel ++ " \183 " ++ show count)

writeMsg istate (ListClanMembers members) = do
      -- empty list widget.
      listStoreClear (tmClan istate)

      -- add to clan list.
      mapM_ (listStorePrepend (tmClan istate)) members

writeMsg istate (RemoveClanMember username) = do
      -- remove from clan list.
      Just midx <- listStoreFind (tmClan istate) cUsername username
      listStoreRemove (tmClan istate) midx

writeMsg istate (UpdateClanMember member) = do
      -- update stored element.
      midx <- listStoreFind (tmClan istate) cUsername (cUsername member)
      case midx of
            Nothing -> listStorePrepend (tmClan istate) member
            Just idx -> listStoreSetValue (tmClan istate) idx member

writeMsg istate (ClearClanMembers) = do
      -- empty list widget.
      listStoreClear (tmClan istate)
      
      -- update notebook tab label
      notebookSetTabLabelText (nbList istate) (swClan istate) (showTag [])

writeMsg istate (ShowClanInfo tag count) = do
      -- update notebook tab label
      case count of
            0 -> notebookSetTabLabelText (nbList istate) (swClan istate) (showTag tag)
            _ -> notebookSetTabLabelText (nbList istate) (swClan istate) (showTag tag ++ " \183 " ++ show count)


-- | Time stamp writing to text buffer.
writeTextBufferTime :: IState -> IO ()
writeTextBufferTime istate = do
      time <- getClockTime >>= toCalendarTime
      textBufferPut (tbChat istate) (ttTime istate) (showString (formatCalendarTime defaultTimeLocale "%r" time) "  ")


-- | Recursive textBufferPut for handling messages which may need hyperlinking of URIs.
writeTextBufferMsg :: IState -> (IState -> TextTag) -> [String] -> String -> IO ()
writeTextBufferMsg istate _ [] [] = do
      pos <- textBufferGetEndIter (tbChat istate)
      textBufferInsert (tbChat istate) pos "\n"
writeTextBufferMsg istate ttagf [] text = do
      textBufferPut (tbChat istate) (ttagf istate) (showString text "\n")
writeTextBufferMsg istate ttagf (url:urls) text = do
      textBufferPut (tbChat istate) (ttagf istate) (takeFstS url text)
      textBufferPut (tbChat istate) (ttURL istate) url
      writeTextBufferMsg istate ttagf urls (takeSndS url text)


-- | Wrapper for writing text with text tag applied to a text buffer.
textBufferPut :: TextBuffer -> TextTag -> String -> IO ()
textBufferPut tbchat ttag text = do
      pos <- textBufferGetEndIter tbchat
      lMark <- textBufferCreateMark tbchat Nothing pos True
      textBufferInsert tbchat pos text
      lower <- textBufferGetIterAtMark tbchat lMark
      upper <- textBufferGetEndIter tbchat
      textBufferApplyTag tbchat ttag lower upper


-- | Function to find the index of an element in a list store.
-- listStoreFind :: Eq b => ListStore a -> (a -> b) -> b -> IO (Maybe Int)
-- listStoreFind ls acc val = do
      -- lslist <- listStoreToList ls
      -- return $! findIndex ((==) val . acc) lslist
-- | Function to find the index of an string element in a list store. *CASE INSENSITIVY REQUIED*
listStoreFind :: ListStore a -> (a -> String) -> String -> IO (Maybe Int)
listStoreFind ls acc val = do
      lslist <- listStoreToList ls
      return $! findIndex ((===) val . acc) lslist


-- | Only set urgency hints if urgency enabled and window inactive.
windowSetUrgencyHintIf :: IState -> IO ()
windowSetUrgencyHintIf istate = do
      enabled <- checkMenuItemGetActive (miUrgent istate)
      focused <- checkMenuItemGetActive (miWinFocused istate)
      when (enabled && (not focused)) (windowSetUrgencyHint (wMain istate) True)


-- | Convenient wrapper for the simplest of if statements.
ifConst :: Bool -> a -> a -> a
ifConst True  a _ = a
ifConst False _ a = a


-- | Regex that matches for valid URLs.
isURL :: String -> Bool
isURL s = s =~ "(((http|ftp|https|ftps|sftp)://)|(www\\.))+(([a-zA-Z0-9\\._-]+\\.[a-zA-Z]{2,6})|([0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}\\.[0-9]{1,3}))(/[a-zA-Z0-9&amp;%_\\./-~-]*)?"
