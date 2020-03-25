-- | Anomy relies on 'Core' to correctly respond to incoming messages, translate them into outgoing messages, modify
-- state, and so on. Ensuring the correct messages are produced is vital to attaining desired behaviour of the system.
module Core (
      -- * Anomy core loop
        handler
      , initCore
      -- * Anomy monad
      , Anomy
      , execAnomy
      , evalAnomy
      , AConf (..)
      , AState (..)
      -- * Exceptions
      , aeAStop
      , isAStop
      , isAException
      -- * Type concealing
      , Inc, inc
      -- * Information
      , module Core.About
      -- * Concurrency
      , module Core.Conc
      -- * File config
      , module Core.Conf
      -- * Logging
      , module Core.Log
      -- * Networking
      , module Core.Socket
      -- * Show methods
      , module Core.Show
      -- * STM Channels
      , module Core.TChan
) where

import Auis.Reader.Types as AR
import Auis.Writer.Types as AW
import Bncs.Reader.Types as BR
import Bncs.Writer.Types as BW
-- import Core.Common.Types
import Core.About
import Core.Conf
import Core.Conc
import Core.Log
import Core.Show
import Core.Socket
import Core.TChan
import Core.Types
import Util.List (split, padGenericL)
import Util.String (intToBig, (===), (/==))

import Control.Monad (when)
import Data.Char (toLower)
import Data.List (intersperse)
import Data.Maybe (isJust, isNothing, fromJust)

import qualified Data.Map as M (size, empty, insert, delete, fromList, filter)


-- | This function is the main loop, run in a single thread, which grabs 'Core.Types.Incoming' messages from record
-- /chanCore/ of 'Core.Types.AState' and pushes the appropriate sequence of 'Core.Types.Outgoing' messages into records
-- /chanAuis/ and /chanBncs/. Also modifies state, the incoming message itself and/or does nothing.
handler :: Anomy ()
handler = do
      chancore <- gets chanCore
      im <- readTChanM chancore
      respMsg im >>= mapM_ execMsg
      handler


-- | Fresh state with resources instantiated.
initCore :: IO AState
initCore = newState


-- | The freshest state you can get.
newState :: IO AState
newState = do
      -- thread-independent variables.
      netsock <- newSocket
      chanauis <- newTChanIO
      chanbncs <- newTChanIO
      chancore <- newTChanIO
      
      -- create logfile. notify if failure occurs
      (loghandle, logfilename) <- Prelude.catch 
            ( do
                  newlog <- newLog
                  writeTChanIO chanauis (AW.PrintSpecial "Logging functionality sanity check: okay.")
                  return newlog
            )
            ( -- only runs when fail to create log file
              \excep -> do
                  writeTChanIO chanauis (AW.PrintError ("Logging functionality sanity check: broken. (" ++ show excep ++ ")"))
                  return (stdout, [])
            )
      
      -- welcome and version info only when 'newState' is called.
      writeTChanIO chanauis (AW.PrintSpecial "Welcome to Anomy!")
      writeTChanIO chanauis (AW.PrintSpecial ("Running version " ++ version ++ "."))
      return $! AState
            { netSock  = netsock

            , chanAuis = chanauis
            , chanBncs = chanbncs
            , chanCore = chancore
            
            , logHandle = loghandle
            , logFileName = logfilename

            , selfFlag = User
            , selfTag = Nothing
            , selfRank = Nothing
            , selfChannel = Nothing

            , sizeChannel = 0
            , sizeFriends = undefined
            , sizeClan = 0
            , mapClan = M.empty

            , createTag = Nothing
            , createClan = Nothing

            , inviteCookie = Nothing
            , inviteTag = Nothing
            , inviteClan = Nothing
            , inviteSender = Nothing

            , inviteUsername = Nothing
            , removeUsername = Nothing
            , transChieftain = Nothing

            , changeUsername = Nothing
            , changeRank = Nothing
            }


-- | Stop exception.
aeAStop :: AException
aeAStop = AStop


-- | Check if exception is 'Anomy's internal one.
isAException :: SomeException -> Bool
isAException sexcep =
      case (fromException sexcep) of
            Just AStop -> True
            Just ARestart -> True
            _ -> False


-- | Check if exception is a stop message for 'Anomy'
isAStop :: SomeException -> Bool
isAStop sexcep =
      case (fromException sexcep) of
            Just AStop -> True
            _ -> False


-- | Predicate to determine if owner of message has blue text.
isBlueText :: Flag -> Bool
isBlueText Administrator = True
isBlueText Operator = True
isBlueText _ = False


-- | Predicate to determine if user is online or not.
isOnline :: Presence -> Bool
isOnline Offline = False
isOnline _ = True


-- | Wrapper function for correctly showing username fields (clan tag & level) based on client.
addUpdChannelUser :: (Flag -> Dword -> String -> Dword -> Dword -> String -> Dword -> AW.Auis)
      -> Flag -> Dword -> String -> Dword -> Dword -> String -> Dword -> AW.Auis
addUpdChannelUser auis flag ping username client icon level tag
      | client == "PXES" || client == "RATS" = auis flag ping username client icon tag []
      | client /= "PX3W" && client /= "3RAW" = auis flag ping username client icon level []
      | otherwise = auis flag ping username client icon level tag


-- | Helper function to construct a numbered list in a nice format.
formatList :: Show a => [a] -> [String]
formatList [] = []
formatList xs = zipWith (++) numbering (showing xs)
      where numbering = map (padGenericL padlen ' ') (map show [(1::Int)..])
            showing = map (showString ": " . show)
            padlen = length (show (length xs))


-- | Tailored equivalent of function 'Control.Monad.when' for outputing lists.
whenL :: (Monad m, Outgoing o) => Bool -> m [o] -> m [o]
whenL True mf = mf
whenL False _ = return $! []


-- | Conditional outgoing message list construction.
(?) :: Outgoing o => Bool -> o -> o -> [o] -> [o]
(?) True  m _ = (:) m
(?) False _ m = (:) m


-- | Conditional outgoing message list contruction which defaults to the do-nothing message
(??) :: Bool -> Out -> [Out] -> [Out]
(??) p m = (?) p m (out (ignore))


-- | Make 'Auis.Reader.Types.Auis' an instance of 'Core.Types.Incoming'.
instance Incoming AR.Auis where
      respMsg (AR.SendChat message) = do
            selfflag <- gets selfFlag
            accusername <- asks accUsername
            return $! (:) (out (BW.ChatCommand message))
                   $ (??) (head message /= '/')
                          (out (AW.PrintTalk (isBlueText selfflag) accusername message)) []

      respMsg (AR.ShowAbout) = do
            return $! (:) (out (BW.ChatCommand ("/me is running anomy " ++ version))) []

      respMsg (AR.ShowGames) = do
            return $! (:) (out (BW.GetAdvListEx)) []

      respMsg (AR.ShowFriends) = do
            return $! (:) (out (BW.FriendsList)) []

      respMsg (AR.ShowClanMembers) = do
            return $! (:) (out (BW.ClanMemberList)) []

      respMsg (AR.ExecLogonAuto) = do
            autologon <- asks autoLogon
            whenL autologon $ do
                  return $! (:) (out (BW.AuthInfo))
                         $  (:) (out (AW.PrintInternal "Client information sent.")) []

      respMsg (AR.ExecLogon) = do
            return $! (:) (out (BW.AuthInfo))
                   $  (:) (out (AW.PrintInternal "Client information sent.")) []

      respMsg (AR.ExecRestart) = do
            throwA ARestart

      respMsg (AR.ExecClanCreation tag clan) = do
            mselftag <- gets selfTag
            whenL (and [isNothing mselftag, length tag <= 4]) $ do
                  modify (\astate ->
                        astate { createTag = Just tag
                               , createClan = Just clan })
                  return $! (:) (out (AW.PrintGeneral ("Attemping to create " ++ showTag tag ++ " (" ++ clan ++ ").")))
                         $  (:) (out (BW.ClanFindCandidates tag)) []

      respMsg (AR.SendClanInvite username) = do
            mselftag <- gets selfTag
            whenL (isJust mselftag) $ do
                  modify (\astate -> astate { inviteUsername = Just username })
                  return $! (:) (out (BW.ClanInvitation username))
                         $  (:) (out (AW.PrintGeneral ("Inviting " ++ username ++ " to join " ++ showTag (fromJust mselftag) ++ "."))) []

      respMsg (AR.ReplyClanInvite response) = do
            mcreatetag <- gets createTag
            mselftag <- gets selfTag
            minvitecookie <- gets inviteCookie
            minvitetag <- gets inviteTag
            minviteclan <- gets inviteClan
            minvitesender <- gets inviteSender
            modify (\astate ->
                  astate { createTag = Nothing
                         , inviteCookie = Nothing
                         , inviteTag = Nothing
                         , inviteClan = Nothing
                         , inviteSender = Nothing })
            whenL (and [isNothing mselftag, isJust minvitecookie, isJust minvitetag, isJust minviteclan, isJust minvitesender]) $ do
                  return $! (?) (isJust mcreatetag)
                            (out (BW.ClanCreationInvitation (fromJust minvitecookie) (reverse (fromJust minvitetag)) (fromJust minvitesender) response))
                            (out (BW.ClanInvitationResponse (fromJust minvitecookie) (reverse (fromJust minvitetag)) (fromJust minvitesender) response))
                         $  (:) (out (AW.PrintGeneral (shows response (fromJust minvitesender ++ "'s invitation to join " ++ showTag (fromJust minvitetag) ++ " (" ++ fromJust minviteclan ++ ").")))) []

      respMsg (AR.DisbandClan) = do
            mselfrank <- gets selfRank
            whenL (Just Chieftain == mselfrank) $ do
                  Just selftag <- gets selfTag
                  return $! (:) (out (BW.ClanDisband))
                         $  (:) (out (AW.PrintGeneral ("Disbanding " ++ showTag selftag ++ "."))) []

      respMsg (AR.SetClanChieftain username) = do
            mselfrank <- gets selfRank
            whenL (Just Chieftain == mselfrank) $ do
                  Just selftag <- gets selfTag
                  modify (\astate -> astate { transChieftain = Just username })
                  return $! (:) (out (BW.ClanMakeChieftain username))
                         $  (:) (out (AW.PrintGeneral ("Transferring ownership of " ++ showTag selftag ++ " to " ++ username ++ "."))) []

      respMsg (AR.RemoveClanMember username) = do
            mselfrank <- gets selfRank
            whenL (or [Just Chieftain == mselfrank, Just Shaman == mselfrank]) $ do
                  Just selftag <- gets selfTag
                  modify (\astate -> astate { removeUsername = Just username })
                  return $! (:) (out (BW.ClanRemoveMember username))
                         $  (:) (out (AW.PrintGeneral ("Removing member " ++ username ++ " from " ++ showTag selftag ++ "."))) []

      respMsg (AR.LeaveClan) = do
            mselftag <- gets selfTag
            whenL (isJust mselftag) $ do
                  accusername <- asks accUsername
                  modify (\astate ->  astate { removeUsername = Just accusername })
                  return $! (:) (out (BW.ClanRemoveMember accusername))
                         $  (:) (out (AW.PrintGeneral ("Leaving " ++ showTag (fromJust mselftag) ++ "."))) []

      respMsg (AR.ChangeClanMemberRank username newrank) = do
            mselfrank <- gets selfRank
            whenL (or [Just Chieftain == mselfrank, Just Shaman == mselfrank]) $ do
                  modify (\astate ->
                        astate { changeUsername = Just username
                               , changeRank = Just newrank })
                  return $! (:) (out (BW.ClanRankChange username newrank))
                         $  (:) (out (AW.PrintGeneral ("Changing " ++ username ++ "'s rank to " ++ shows newrank "."))) []

      respMsg (AR.SetClanMotd motd) = do
            mselfrank <- gets selfRank
            whenL (or [Just Chieftain == mselfrank, Just Shaman == mselfrank]) $ do
                  return $! (:) (out (BW.ClanSetMotd motd)) []


-- | Make 'Auis.Writer.Types.Auis' an instance of 'Core.Types.Outgoing'.
instance Outgoing AW.Auis where
      execMsg m = do
            chanauis <- gets chanAuis
            writeTChanM chanauis m


-- | Make 'Bncs.Reader.Types.Bncs' an instance of 'Core.Types.Incoming'.
instance Incoming BR.Bncs where
      respMsg BR.Null = do
            return $! (:) (out BW.Null) []

      respMsg BR.GetAdvListEx {bGames = games} = do
            return $! (:) (out (AW.PrintGeneral "Your PvPGN - Games List"))
                   $  (:) (out (AW.PrintGeneral "=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-="))
                   $ (++) (map (out . AW.PrintGeneral) (formatList games))
                   $  (:) (out (AW.PrintGeneral "=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-="))
                   $  (:) (out (AW.PrintGeneral "End of Games List")) []

      respMsg BR.EnterChat {} = do
            maccchannel <- asks accChannel
            return $! (:) (out (joinChannel maccchannel)) []
            where
                  joinChannel Nothing = BW.JoinChannel BW.Default "W3"
                  joinChannel (Just accchannel) = BW.JoinChannel BW.Forced accchannel

      respMsg (BR.ChatEvent flag ping username (BR.ShowUser client icon level tag)) = do
            accusername <- asks accUsername
            when (username === accusername) $ do
                  modify (\astate -> astate { selfFlag = flag })
            Just selfchannel <- gets selfChannel
            sizechannel <- gets sizeChannel
            modify (\astate -> astate { sizeChannel = sizechannel+1 })
            return $! (:) (out (AW.ShowChannelInfo selfchannel (sizechannel+1)))
                   $  (:) (out (addUpdChannelUser (AW.AddChannelUser False) flag ping username client icon level tag)) []

      respMsg (BR.ChatEvent flag ping username (BR.Join client icon level tag)) = do
            Just selfchannel <- gets selfChannel
            sizechannel <- gets sizeChannel
            modify (\astate -> astate { sizeChannel = sizechannel+1 })
            return $! (:) (out (AW.ShowChannelInfo selfchannel (sizechannel+1)))
                   $  (:) (out (addUpdChannelUser (AW.AddChannelUser True) flag ping username client icon level tag)) []

      respMsg (BR.ChatEvent flag ping username (BR.Leave)) = do
            Just selfchannel <- gets selfChannel
            sizechannel <- gets sizeChannel
            modify (\astate -> astate { sizeChannel = sizechannel-1 })
            return $! (:) (out (AW.ShowChannelInfo selfchannel (sizechannel-1)))
                   $  (:) (out (AW.RemoveChannelUser flag ping username)) []

      respMsg BR.ChatEvent {bFlag = Squelched, bEvent = BR.Whisper {}} = do
            return $! []
      respMsg BR.ChatEvent {bFlag = flag, BR.bUsername = username, BR.bEvent = BR.Whisper message} = do
            return $! (:) (out (AW.PrintWhisper False (isBlueText flag) username message)) []

      respMsg BR.ChatEvent {bFlag = Squelched, bEvent = BR.Talk {}} = do
            return $! []
      respMsg BR.ChatEvent {bFlag = flag, BR.bUsername = username, bEvent = BR.Talk message} = do
            return $! (:) (out (AW.PrintTalk (isBlueText flag) username message)) []

      respMsg BR.ChatEvent {bEvent = BR.Broadcast message} = do
            return $! (:) (out (AW.PrintGeneral message)) []

      respMsg BR.ChatEvent {bEvent = BR.Channel info} = do
            modify (\astate ->
                  astate { selfChannel = Just info
                         , sizeChannel = 0 })
            return $! (:) (out (AW.ClearChannelUsers info))
                   $  (:) (out (AW.ShowChannelInfo info 0)) []

      respMsg (BR.ChatEvent flag ping username (BR.UserFlags client icon level tag)) = do
            accusername <- asks accUsername
            when (username === accusername) $ do
                  modify (\astate -> astate { selfFlag = flag })
            return $! (:) (out (addUpdChannelUser AW.UpdateChannelUser flag ping username client icon level tag)) []

      respMsg BR.ChatEvent {BR.bUsername = username, bEvent = BR.WhisperSent message} = do
            selfflag <- gets selfFlag
            return $! (:) (out (AW.PrintWhisper True (isBlueText selfflag) username message)) []

      respMsg BR.ChatEvent {bEvent = BR.ChannelFull {}} = do
            return $! (:) (out (AW.PrintError "Channel is full.")) []

      respMsg BR.ChatEvent {bEvent = BR.ChannelDoesNotExist {}} = do
            return $! (:) (out (AW.PrintError "Channel could not be created.")) []

      respMsg BR.ChatEvent {bEvent = BR.ChannelRestricted {}} = do
            return $! (:) (out (AW.PrintError "Channel is restricted.")) []

      respMsg BR.ChatEvent {bEvent = BR.Info message} = do
            return $! (:) (out (AW.PrintGeneral message)) []

      respMsg BR.ChatEvent {bEvent = BR.Error message} = do
            return $! (:) (out (AW.PrintError message)) []

      respMsg BR.ChatEvent {BR.bUsername = username, bEvent = BR.Emote message} = do
            return $! (:) (out (AW.PrintEmote username message)) []

      respMsg BR.FloodDetected = do
            _ <- fail "flooding the server"
            return $! []

      respMsg (BR.MessageBox text caption) = do
            return $! (:) (out (AW.PrintInternal (caption ++ ": " ++ text))) []

      respMsg BR.StartAdvEx3 {} = do
            return $! [] -- TODO

      respMsg (BR.Ping ping) = do
            return $! (:) (out (BW.Ping ping)) []

      respMsg (BR.AuthInfo) = do
            return $! (:) (out (AW.PrintInternal "Client information accepted"))
                   $  (:) (out (BW.AuthCheck))
                   $  (:) (out (AW.PrintInternal "Client authentication sent.")) []

      respMsg BR.AuthCheck {bExeResult = BR.ExeSuccess {}} = do
            accusername <- asks accUsername
            return $! (:) (out (AW.PrintInternal ("Client authentication accepted.")))
                   $  (:) (out (BW.AuthAccountLogon accusername))
                   $  (:) (out (AW.PrintInternal ("Account username sent."))) []
      respMsg (BR.AuthCheck bexeresult message) = do
            return $! (:) (out (AW.PrintError ("Client authentication rejected: " ++ shows bexeresult message))) []

      respMsg (BR.AuthAccountCreate BR.AuthSuccess) = do
            accusername <- asks accUsername
            return $! (:) (out (AW.PrintInternal "Account creation successful."))
                   $  (:) (out (BW.AuthAccountLogon accusername))
                   $  (:) (out (AW.PrintInternal ("Account username sent."))) []
      respMsg (BR.AuthAccountCreate authresult) = do
            return $! (:) (out (AW.PrintError ("Account creation failed: " ++ show authresult))) []

      respMsg (BR.AuthAccountLogon BR.AuthSuccess) = do
            accpassword <- asks accPassword
            return $! (:) (out (AW.PrintInternal "Account username accepted."))
                   $  (:) (out (BW.AuthAccountLogonProof accpassword))
                   $  (:) (out (AW.PrintInternal "Account password sent.")) []
      respMsg (BR.AuthAccountLogon authresult) = do
            return $! (:) (out (AW.PrintError ("Account username rejected: " ++ show authresult))) []

      respMsg BR.AuthAccountLogonProof {bAuthResult = BR.AuthSuccess {}} = do
            netgameport <- asks netGamePort
            return $! (:) (out (AW.PrintInternal "Account password accepted."))
                   $  (:) (out (BW.NetGamePort (intToBig netgameport)))
                   $  (:) (out (BW.EnterChat)) []
      respMsg BR.AuthAccountLogonProof {bAuthResult = BR.AuthNeedEmail {}} = do
            netgameport <- asks netGamePort
            accemail <- asks accEmail
            return $! (:) (out (AW.PrintInternal "Account password accepted."))
                   $  (:) (out (BW.SetEmail accemail))
                   $  (:) (out (AW.PrintInternal "Account email registered."))
                   $  (:) (out (BW.NetGamePort (intToBig netgameport)))
                   $  (:) (out (BW.EnterChat)) []
      respMsg BR.AuthAccountLogonProof {bAuthResult = authresult} = do
            return $! (:) (out (AW.PrintError ("Account password rejected: " ++ show authresult))) []

      respMsg (BR.SetEmail) = do
            return $! (:) (out (AW.PrintInternal "Account email registration required.")) []

      respMsg BR.FriendsList {bFriends = friends} = do
            return $! (:) (out (AW.PrintGeneral "Your PvPGN - Friends List"))
                   $  (:) (out (AW.PrintGeneral "=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-="))
                   $ (++) (map (out . AW.PrintGeneral) (formatList (filter (isOnline . fPresence) friends)))
                   $  (:) (out (AW.PrintGeneral "=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-="))
                   $  (:) (out (AW.PrintGeneral "End of Friends List")) []

      respMsg BR.FriendsUpdate {} = do -- PvPGN never sends this message
            return $! (:) (out (AW.PrintSpecial "Unexpected FriendsUpdate message received. (respMsg BR.FriendsUpdate {})")) []

      respMsg BR.FriendsAdd {} = do -- TODO: update internal friends list - not used because no GUI friends list (blame BR.FriendsUpdate)
            return $! []

      respMsg BR.FriendsRemove {} = do -- TODO: update internal friends list - not used because no GUI friends list (blame BR.FriendsUpdate)
            return $! [] 

      respMsg BR.FriendsPosition {} = do -- TODO: update internal friends list - not used because no GUI friends list (blame BR.FriendsUpdate)
            return $! []

      respMsg (BR.ClanFindCandidates BR.ClanSuccess count usernames) = do
            mcreateclan <- gets createClan
            mcreatetag <- gets createTag
            return $! (:) (out (BW.ClanInviteMultiple (fromJust mcreateclan) (fromJust mcreatetag) count usernames))
                   $  (:) (out (AW.PrintGeneral ("Inviting to create " ++ showTag (fromJust mcreatetag) ++ " (" ++ fromJust mcreateclan ++ "): " ++ unwords usernames))) []
      respMsg BR.ClanFindCandidates {bClanResult = clanresult} = do
            mcreatetag <- gets createTag
            mcreateclan <- gets createClan
            return $! (:) (out (AW.PrintGeneral ("Failed to create " ++ showTag (fromJust mcreatetag) ++ " (" ++ fromJust mcreateclan ++ "): " ++ show clanresult))) []

      respMsg BR.ClanInviteMultiple {bClanResult = BR.ClanSuccess {}} = do
            mcreatetag <- gets createTag
            mcreateclan <- gets createClan
            return $! (:) (out (AW.PrintGeneral ("Successfully created " ++ showTag (fromJust mcreatetag) ++ " (" ++ fromJust mcreateclan ++ ")."))) []
      respMsg (BR.ClanInviteMultiple BR.ClanInvitationRejected usernames) = do
            mcreatetag <- gets createTag
            mcreateclan <- gets createClan
            return $! (:) (out (AW.PrintGeneral ("Failed to create " ++ showTag (fromJust mcreatetag) ++ " (" ++ fromJust mcreateclan ++ "): Invitations rejected (" ++ unwords usernames ++ ")."))) []
      respMsg BR.ClanInviteMultiple {bClanResult = clanresult} = do
            mcreatetag <- gets createTag
            mcreateclan <- gets createClan
            return $! (:) (out (AW.PrintGeneral ("Failed to create " ++ showTag (fromJust mcreatetag) ++ " (" ++ fromJust mcreateclan ++ "): " ++ show clanresult))) []

      respMsg BR.ClanCreationInvitation {BR.bCookie = cookie, BR.bTag = tag, BR.bClan = clan, BR.bInviter = inviter, BR.bUsernames = usernames} = do
            mselftag <- gets selfTag
            autoaccept <- asks autoAccept
            when (and [isNothing mselftag, not autoaccept])
                 (modify (\astate ->
                        astate { createTag = Just tag -- flag to differentiate from normal invite
                               , inviteCookie = Just cookie
                               , inviteTag = Just tag
                               , inviteClan = Just clan
                               , inviteSender = Just inviter }))
            return $! (?) (isJust mselftag)
                          (out (BW.ClanCreationInvitation cookie tag inviter Reject))
                          (out (AW.PrintGeneral ("Received an invitation from " ++ inviter ++ " to join " ++ showTag tag ++ " (" ++ clan ++ ") for creation (" ++ unwords usernames ++ ").")))
                   $ (??) (and [isNothing mselftag, autoaccept])
                          (out (BW.ClanCreationInvitation cookie tag inviter Accept))
                   $ (??) (and [isNothing mselftag, autoaccept])
                          (out (AW.PrintGeneral (shows Accept ("(auto) " ++ inviter ++ "'s invitation to join " ++ showTag tag ++ " (" ++ clan ++ ").")))) []

      respMsg (BR.ClanDisband BR.ClanSuccess) = do
            return $! (:) (out (AW.ClearClanMembers))
                   $  (:) (out (AW.PrintGeneral ("Clan successfully disbanded."))) []
      respMsg (BR.ClanDisband clanresult) = do
            Just selftag <- gets selfTag
            return $! (:) (out (AW.PrintGeneral ("Failed to disband " ++ showTag selftag ++ ": " ++ show clanresult))) []

      respMsg (BR.ClanMakeChieftain BR.ClanSuccess) = do
            Just selftag <- gets selfTag
            mtranschieftain <- gets transChieftain
            modify (\astate -> astate { transChieftain = Nothing })
            return $! (?) (isJust mtranschieftain)
                          (out (AW.PrintGeneral ("Successfully transferred ownership of " ++ showTag selftag ++ " to " ++ fromJust mtranschieftain ++ ".")))
                          (out (AW.PrintGeneral ("Ownership of " ++ showTag selftag ++ " has been transferred."))) []
      respMsg (BR.ClanMakeChieftain clanresult) = do
            Just selftag <- gets selfTag
            Just transchieftain <- gets transChieftain
            modify (\astate -> astate { transChieftain = Nothing })
            return $! (:) (out (AW.PrintGeneral ("Failed to transfer ownership of " ++ showTag selftag ++ " to " ++ transchieftain ++ ": " ++ show clanresult))) []

      respMsg (BR.ClanInfo tag rank) = do
            modify (\astate ->
                  astate { selfTag = Just tag
                         , selfRank = Just rank })
            return $! (:) (out (BW.ClanMemberList)) []

      respMsg BR.ClanQuitNotify {} = do
            Just selftag <- gets selfTag
            modify (\astate ->
                  astate { selfTag = Nothing
                         , selfRank = Nothing})
            return $! (:) (out (AW.ClearClanMembers))
                   $  (:) (out (AW.PrintGeneral ("You have been removed from " ++ showTag selftag ++ "."))) []

      respMsg (BR.ClanInvitation BR.ClanSuccess) = do
            minviteusername <- gets inviteUsername
            whenL (isJust minviteusername) $ do
                  Just selftag <- gets selfTag
                  modify (\astate -> astate { inviteUsername = Nothing })
                  return $! (:) (out (AW.PrintGeneral (fromJust minviteusername ++ " accepted the invitation to join " ++ showTag selftag ++ "."))) []
      respMsg (BR.ClanInvitation BR.ClanInvitationRejected) = do
            minviteusername <- gets inviteUsername
            whenL (isJust minviteusername) $ do
                  Just selftag <- gets selfTag
                  modify (\astate -> astate { inviteUsername = Nothing })
                  return $! (:) (out (AW.PrintGeneral (fromJust minviteusername ++ " rejected the invitation to join " ++ showTag selftag ++ "."))) []
      respMsg (BR.ClanInvitation clanresult) = do
            minviteusername <- gets inviteUsername
            whenL (isJust minviteusername) $ do
                  Just selftag <- gets selfTag
                  modify (\astate -> astate { inviteUsername = Nothing })
                  return $! (:) (out (AW.PrintGeneral ("Failed to invite " ++ fromJust minviteusername ++ " to join " ++ showTag selftag ++ ": " ++ show clanresult))) []

      respMsg (BR.ClanRemoveMember BR.ClanSuccess) = do
            accusername <- asks accUsername
            Just removeusername <- gets removeUsername
            modify (\astate -> astate { removeUsername = Nothing })
            whenL (accusername /== removeusername) $ do
                  Just selftag <- gets selfTag
                  return $! (:) (out (AW.PrintGeneral ("Successfully removed member " ++ removeusername ++ " from " ++ showTag selftag ++ "."))) []
      respMsg (BR.ClanRemoveMember clanresult) = do
            Just selftag <- gets selfTag
            Just removeusername <- gets removeUsername
            modify (\astate -> astate { removeUsername = Nothing })
            return $! (:) (out (AW.PrintGeneral ("Failed to remove member " ++ removeusername ++ " from " ++ showTag selftag ++ ": " ++ show clanresult))) []

      respMsg (BR.ClanInvitationResponse cookie tag clan inviter) = do
            mselftag <- gets selfTag
            autoaccept <- asks autoAccept
            when (and [isNothing mselftag, not autoaccept])
                 (modify (\astate ->
                        astate { inviteCookie = Just cookie
                               , inviteTag = Just tag
                               , inviteClan = Just clan
                               , inviteSender = Just inviter }))
            return $! (?) (isJust mselftag)
                          (out (BW.ClanInvitationResponse cookie tag inviter Reject))
                          (out (AW.PrintGeneral ("Received an invitation from " ++ inviter ++ " to join " ++ showTag tag ++ " (" ++ clan ++ ").")))
                   $ (??) (and [isNothing mselftag, autoaccept])
                          (out (BW.ClanInvitationResponse cookie tag inviter Accept))
                   $ (??) (and [isNothing mselftag, autoaccept])
                          (out (AW.PrintGeneral (shows Accept ("(auto) " ++ inviter ++ "'s invitation to join " ++ showTag tag ++ " (" ++ clan ++ ").")))) []

      respMsg (BR.ClanRankChange BR.ClanSuccess) = do
            Just changeusername <- gets changeUsername
            Just changerank <- gets changeRank
            modify (\astate ->
                  astate { changeUsername = Nothing
                         , changeRank = Nothing })
            return $! (:) (out (AW.PrintGeneral ("Successfully changed " ++ changeusername ++ "'s rank to " ++ shows changerank "."))) []
      respMsg (BR.ClanRankChange clanresult) = do
            Just changeusername <- gets changeUsername
            Just changerank <- gets changeRank
            modify (\astate ->
                  astate { changeUsername = Nothing
                         , changeRank = Nothing })
            return $! (:) (out (AW.PrintGeneral ("Failed to change " ++ changeusername ++ "'s rank to " ++ shows changerank (": " ++ show clanresult)))) []

      respMsg BR.ClanMemberList {bMembers = members} = do
            Just selftag <- gets selfTag
            modify (\astate ->
                  astate { sizeClan = sizeclan
                         , mapClan = mapclan })
            return $! (:) (out (AW.ClearClanMembers))
                   $  (:) (out (AW.ShowClanInfo selftag sizeclan))
                   $  (:) (out (AW.ListClanMembers members)) []
            where
                  mapclan = M.fromList (map (\m -> (map toLower (cUsername m), m)) members)
                  sizeclan = M.size (M.filter (isOnline . cPresence) mapclan)

      respMsg (BR.ClanMemberRemoved username) = do
            Just selftag <- gets selfTag
            sizeclan <- gets sizeClan
            mapclan <- gets mapClan
            modify (\astate ->
                  astate { sizeClan = sizeclan-1
                         , mapClan = M.delete (map toLower username) mapclan })
            return $! (:) (out (AW.RemoveClanMember username))
                   $  (:) (out (AW.ShowClanInfo selftag (sizeclan-1)))
                   $  (:) (out (AW.PrintGeneral (username ++ " has been removed from " ++ showTag selftag ++ "."))) []

      respMsg (BR.ClanMemberStatusChange username rank presence location) = do
            mselftag <- gets selfTag
            mapclan <- gets mapClan
            modify (\astate ->
                  astate { sizeClan = sizeOnline (updateMapClan mapclan)
                         , mapClan = updateMapClan mapclan })
            return $! (:) (out (AW.UpdateClanMember member))
                   $ (??) (isJust mselftag)
                          (out (AW.ShowClanInfo (fromJust mselftag) (sizeOnline (updateMapClan mapclan)))) []
            where
                  member = Member username rank presence location
                  updateMapClan tmapclan = M.insert (map toLower username) member tmapclan
                  sizeOnline tmapclan = M.size (M.filter (isOnline . cPresence) tmapclan)

      respMsg (BR.ClanMemberRankChange rankold ranknew changer) = do
            Just selftag <- gets selfTag
            modify (\astate -> astate { selfRank = Just ranknew })
            return $! (:) (out (AW.PrintGeneral ("Rank in " ++ showTag selftag ++ " has been changed from " ++ shows rankold (" to " ++ shows ranknew " by " ++ shows changer ".")))) []

      respMsg (BR.MessageUnknown bid bdata) = do
            return $! (:) (out (AW.PrintError ("Unknown BNCS message received: " ++ show (fromEnum bid) ++ "\"" ++ bdata ++ "\"."))) []

      respMsg (BR.MessageBadForm bhead bbody) = do
            return $! (:) (out (AW.PrintError ("Invalid BNCS message received: " ++ bhead ++ "    " ++ bbody ++ "."))) []


-- | Make 'Bncs.Writer.Types.Bncs' an instance of 'Core.Types.Outgoing'.
instance Outgoing BW.Bncs where
      execMsg m = do
            chanbncs <- gets chanBncs
            writeTChanM chanbncs m


-- | Construct a game list item string using 'Bncs.Reader.Types.Game'.
instance Show Game where
      show (Game port ip name mapfile creator) = "\"" ++ name ++ "\" - " ++ showMap mapfile ++ " - " ++ creator ++ " (" ++ showIP ip ++ ":" ++ showPort port ")"
            where showPort = shows . sum . zipWith (*) [256,1] . map fromEnum
                  showIP = concat . intersperse "." . map (show . fromEnum)
                  showMap = last . split '\\'


-- | Map server's checkrevision result constructors 'Bncs.Reader.Types.ExeResult' to error messages.
instance Show ExeResult where
      show ExeSuccess = undefined -- this will (should) never happen
      show ExeTooOld = "Version is too old. "
      show (ExeBadVersion ver) = "Version " ++ show (fromEnum ver) ++ " is invalid. "
      show ExeTooNew = "Version is too new. "
      show ExeBadKey = "CD key (classic) is invalid. "
      show ExeBadExpKey = "CD key (expansion) is invalid. "
      show ExeKeyInUse = "CD key (classic) is in use by "
      show ExeExpKeyInUse = "CD key (expansion) is in use by "
      show ExeBannedKey = "CD key (classic) is banned. "
      show ExeBannedExpKey = "CD key (expansion) is banned. "
      show ExeWrongProductKey = "CD key (classic) is not for WAR3. "
      show ExeWrongProductExpKey = "CD key (expansion) is not for W3XP. "


-- | Map account authentication result constructors 'Bncs.Reader.Types.AuthResult' to user-friendly feedback messages.
instance Show AuthResult where
      show AuthSuccess = undefined -- this will (should) never happen
      show AuthAccountDoesNotExist = "Account does not exist."
      show AuthIncorrectPassword = "Password is incorrect."
      show AuthNameExists = "Account under this username already exists."
      show AuthNameTooShort = "Username is too short."
      show AuthNameIllegalChar = "Username contains an illegal character."
      show AuthNameIllegalWord = "Username contains an illegal word."
      show AuthNameTooFewAlphaNum = "Username contains too few alphanumeric characters."
      show AuthNameAdjPunc = "Username contains adjacent punctuation characters."
      show AuthNameTooManyPunc = "Username contains too many punctuation characters."
      show AuthNeedEmail = "Account requires an email to be registered."
      show AuthError = "An unknown error occured."


-- | Map clan mangement command result constructors 'Bncs.Reader.Types.ClanResult' to user-friendly feedback messages.
instance Show ClanResult where
      show ClanSuccess = undefined -- this will (should) never happen
      show ClanInUse = "clan tag is already in use."
      show ClanTooSoon = "command cannot be executed at this time."
      show ClanNotEnoughMembers = "insufficient candidates for clan creation."
      show ClanInvitationRejected = "invitation rejected."
      show ClanInAlready = "already in a clan."
      show ClanNotAuthorised = "not authorised to execute this command."
      show ClanUserNotFound = "user unavailable or does not exist."
      show ClanIsFull = "clan is full."
      show ClanInvalidTag = "clan tag is invalid."
      show ClanInvalidName = "clan name is invalid."
      show (ClanResultUnknown result) = "unknown error (" ++ show (fromEnum result) ++ ")."


-- | Construct a friend list item string using 'Bnce.Reader.Types.Friend'.
instance Show Friend where
      show (Friend username status Offline _ _) = shows status $ username ++ show Offline
      show (Friend username status Online client _) = shows status (username ++ shows Online (" - " ++ showClient client))
      show (Friend username NotMutual presence client _) = shows NotMutual (username ++ " - in a " ++ shows presence (" - " ++ showClient client))
      show (Friend username status presence client location) = shows status (username ++ " - in " ++ shows presence (" \"" ++ location ++ "\" - " ++ showClient client))


-- | Map friend status constructors 'Bncs.Reader.Types.Status' to user-friendly strings.
instance Show Status where
      show NotMutual = "[_] "
      show Mutual = "[*] "
      show DoNotDisturb = "[!] "
      show Away = "[-] "
      show (StatusUnknown status) = "[" ++ show (fromEnum status) ++ "] "


-- | Map friend presence constructors 'Bncs.Reader.Types.Presence' to user-friendly strings.
instance Show Presence where
      show Offline = " is offline"
      show Online = " is online"
      show OnlineInChannel = "channel"
      show OnlineInPublicGame = "public game"
      show OnlineInPrivateGame = "private game"


-- | Map invitation response constructors 'Bncs.Writer.Types.Response' to user-friendly strings.
instance Show Response where
      show Accept = "Accepted "
      show Reject = "Rejected "


-- | Map user flag constructors 'Core.Types.Flag' to user-friendly strings.
instance Show Flag where
      show User = "User"
      show Administrator = "Administrator"
      show TempOperator = "Temporary Operator"
      show Voiced = "Voiced user"
      show Operator = "Operator"
      show FirstJoin = "First-join user"
      show Squelched = "Squelched"
      show (FlagUnknown flag) = "Unknown user (" ++ show (fromEnum flag) ++ ")"
