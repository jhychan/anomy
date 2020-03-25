-- | Incoming BNCS message string processing.
module Bncs.Reader (
        readMsg
      , Bncs
) where

import Bncs.Reader.Types
import Util.Crypto (decode)
import Util.List (takeFst, takeSnd, split, splitInf)

-- | Takes an incoming BNCS packet and produces the corresponding 'Bncs.Reader.Types.Bncs' constructor.
readMsg :: String -> Bncs
readMsg ('\xff':mid:_:_:m) = readMsgMain mid m
readMsg bad = MessageBadForm mhead mbody
      where mhead = take 4 bad
            mbody = drop 4 bad


-- | Main function for processing incoming BNCS messages.
readMsgMain :: Char -> String -> Bncs

readMsgMain '\x00' _ = Null

readMsgMain '\x09' m = GetAdvListEx count games
      where count = head m
            games = (readMsgGetAdvListEx (fromEnum count) . drop 4) m

readMsgMain '\x0a' m = EnterChat username client account
      where username = (split '\x00' m) !! 0
            client   = (split '\x00' m) !! 1
            account  = (split '\x00' m) !! 2

readMsgMain '\x0f' m = ChatEvent flag ping username event
      where flag     = (readMsgChatEventFlag . head . drop 4) m
            ping     = (take 4 . drop 8) m
            username = (split '\x00' . drop 24) m !! 0
            message  = (split '\x00' . drop 24) m !! 1
            event    = readMsgChatEvent (head m) message

readMsgMain '\x13' _ = FloodDetected

readMsgMain '\x19' m = MessageBox text caption
      where text    = (takeFst '\x00' . drop 4) m
            caption = (takeSnd '\x00' . drop 4) m

readMsgMain '\x1c' m = StartAdvEx3 result
      where result = (readMsgStartAdvEx3Result . head) m

readMsgMain '\x25' m = Ping ping
      where ping = m

readMsgMain '\x50' _ = AuthInfo

readMsgMain '\x51' m = AuthCheck exeresult message
      where exeresult = (readMsgAuthCheckResult . take 2) m
            message   = (init . drop 4) m

readMsgMain '\x52' m = AuthAccountCreate result
      where result = (readMsgAuthResult . head) m

readMsgMain '\x53' m = AuthAccountLogon result
      where result = (readMsgAuthResult . head) m

readMsgMain '\x54' m = AuthAccountLogonProof result message
      where result  = (readMsgAuthResult . head) m
            message = (drop 24) m

readMsgMain '\x59' _ = SetEmail

readMsgMain '\x65' m = FriendsList count friends
      where count   = head m
            friends = (readMsgFriendsList . tail) m

readMsgMain '\x66' m = FriendsUpdate index presence status client location
      where index    = m !! 0
            presence = readMsgUserPresence (m !! 1)
            status   = readMsgUserStatus (m !! 2)
            client   = (take 4 . drop 3) m
            location = (init . drop 7) m

readMsgMain '\x67' m = FriendsAdd username status presence client location
      where username = takeFst '\x00' m
            status   = readMsgUserStatus (takeSnd '\x00' m !! 0)
            presence = readMsgUserPresence (takeSnd '\x00' m !! 1)
            client   = (take 4 . drop 2 . takeSnd '\x00') m
            location = (init . drop 6 . takeSnd '\x00') m

readMsgMain '\x68' m = FriendsRemove index
      where index = head m

readMsgMain '\x69' m = FriendsPosition indexold indexnew
      where indexold = head m
            indexnew = last m

readMsgMain '\x70' m = ClanFindCandidates result count usernames
      where result    = readMsgClanResult (drop 4 m !! 0)
            count     = drop 4 m !! 1
            usernames = (split '\x00' . drop 6) m

readMsgMain '\x71' m = ClanInviteMultiple result usernames
      where result    = (readMsgClanResult . head . drop 4) m
            usernames = (split '\x00' . drop 5) m

readMsgMain '\x72' m = ClanCreationInvitation cookie tag clan inviter count usernames
      where cookie    = take 4 m
            tag       = (takeFst '\x00' . reverse . take 4 . drop 4) m
            clan      = (split '\x00' . drop 8) m !! 0
            inviter   = (split '\x00' . drop 8) m !! 1
            count     = (split '\x00' . drop 8) m !! 2 !! 0
            usernames = (split '\x00' . tail . takeSnd '\x00' . takeSnd '\x00' . drop 8) m

readMsgMain '\x73' m = ClanDisband result
      where result = (readMsgClanResult . head . drop 4) m

readMsgMain '\x74' m = ClanMakeChieftain result
      where result = (readMsgClanResult . head . drop 4) m

readMsgMain '\x75' m = ClanInfo tag rank
      where tag  = (takeFst '\x00' . reverse . take 4 . tail) m
            rank = (readMsgClanRank . last) m

readMsgMain '\x76' m = ClanQuitNotify result
      where result = (readMsgClanResult . head) m

readMsgMain '\x77' m = ClanInvitation result
      where result = (readMsgClanResult . head . drop 4) m

readMsgMain '\x78' m = ClanRemoveMember result
      where result = (readMsgClanResult . head . drop 4) m

readMsgMain '\x79' m = ClanInvitationResponse cookie tag clan inviter
      where cookie  = take 4 m
            tag     = (takeFst '\x00' . reverse . take 4 . drop 4) m
            clan    = (takeFst '\x00' . drop 8) m
            inviter = (init . takeSnd '\x00' . drop 8) m

readMsgMain '\x7a' m = ClanRankChange result
      where result = (readMsgClanResult . head . drop 4) m

readMsgMain '\x7d' m = ClanMemberList count members
      where count   = m !! 4
            members = (readMsgClanMemberList . drop 5) m

readMsgMain '\x7e' m = ClanMemberRemoved username
      where username = init m

readMsgMain '\x7f' m = ClanMemberStatusChange username rank presence location
      where username = takeFst '\x00' m
            rank     = readMsgClanRank (takeSnd '\x00' m !! 0)
            presence = readMsgUserPresence (takeSnd '\x00' m !! 1)
            location = (init . drop 2 . takeSnd '\x00') m

readMsgMain '\x81' m = ClanMemberRankChange rankold ranknew changer
      where rankold = readMsgClanRank (m !! 0)
            ranknew = readMsgClanRank (m !! 1)
            changer = (init . drop 2) m

readMsgMain unknwn m = MessageUnknown unknwn m


-- | Breaks down the GetAdvListEx message (game list) into the listed type.
readMsgGetAdvListEx :: Int -> String -> [Game]
readMsgGetAdvListEx 0 _ = []
readMsgGetAdvListEx count gm = Game port ip name mapfile creator : readMsgGetAdvListEx (count-1) rgm
      where port    = (take 2 . drop 10) gm
            ip      = (take 4 . drop 12) gm
            name    = (takeFst '\x00' . drop 32) gm
            mapfile = (takeFst '\x00' . drop 13 . decode . takeFst '\x00' . drop 10 . takeSnd '\x00' . drop 32) gm
            creator = (takeFst '\x00' . takeSnd '\x00' . drop 13 . decode . takeFst '\x00' . drop 10 . takeSnd '\x00' . drop 32) gm
            rgm     = (takeSnd '\x00' . drop 10 . takeSnd '\x00' . drop 32) gm


-- | Converts the byte representation of user flags to the internal type.
readMsgChatEventFlag :: Char -> Flag
readMsgChatEventFlag '\x00' = User
readMsgChatEventFlag '\x01' = Administrator
readMsgChatEventFlag '\x02' = TempOperator
readMsgChatEventFlag '\x04' = Voiced
readMsgChatEventFlag '\x08' = Operator
readMsgChatEventFlag '\x10' = FirstJoin
readMsgChatEventFlag '\x20' = Squelched
readMsgChatEventFlag  unkn  = FlagUnknown unkn


-- | Turn chat event data into our internal Event type representation.
readMsgChatEvent :: Char -> String -> Event
readMsgChatEvent '\x01' m = ShowUser client icon level tag
      where client = words m !! 0
            icon   = splitInf ' ' m !! 1
            level  = splitInf ' ' m !! 2
            tag    = reverse $ splitInf ' ' m !! 3
readMsgChatEvent '\x02' m = Join client icon level tag
      where client = words m !! 0
            icon   = splitInf ' ' m !! 1
            level  = splitInf ' ' m !! 2
            tag    = reverse $ splitInf ' ' m !! 3
readMsgChatEvent '\x03' _ = Leave
readMsgChatEvent '\x04' m = Whisper m
readMsgChatEvent '\x05' m = Talk m
readMsgChatEvent '\x06' m = Broadcast m
readMsgChatEvent '\x07' m = Channel m
readMsgChatEvent '\x09' m = UserFlags client icon level tag
      where client = words m !! 0
            icon   = splitInf ' ' m !! 1
            level  = splitInf ' ' m !! 2
            tag    = reverse $ splitInf ' ' m !! 3
readMsgChatEvent '\x0a' m = WhisperSent m
readMsgChatEvent '\x0d' m = ChannelFull m
readMsgChatEvent '\x0e' m = ChannelDoesNotExist m
readMsgChatEvent '\x0f' m = ChannelRestricted m
readMsgChatEvent '\x12' m = Info m
readMsgChatEvent '\x13' m = Error m
readMsgChatEvent '\x17' m = Emote m
readMsgChatEvent _ _ = undefined


-- | Convert game hosting result byte to the internal type.
readMsgStartAdvEx3Result :: Char -> Result
readMsgStartAdvEx3Result '\x00' = Success
readMsgStartAdvEx3Result '\x01' = Failure
readMsgStartAdvEx3Result  unkn  = ResultUnknown unkn


-- | Convert AuthCheck result string to the internal type.
readMsgAuthCheckResult :: String -> ExeResult
readMsgAuthCheckResult "\x00\x00" = ExeSuccess
readMsgAuthCheckResult "\x00\x01" = ExeTooOld
readMsgAuthCheckResult "\x01\x01" = ExeTooOld -- Meant to be: ExeBadVersion
readMsgAuthCheckResult "\x02\x01" = ExeTooNew
readMsgAuthCheckResult "\x00\x02" = ExeBadKey
readMsgAuthCheckResult "\x10\x02" = ExeBadExpKey
readMsgAuthCheckResult "\x01\x02" = ExeKeyInUse
readMsgAuthCheckResult "\x11\x02" = ExeExpKeyInUse
readMsgAuthCheckResult "\x02\x02" = ExeBannedKey
readMsgAuthCheckResult "\x12\x02" = ExeBannedExpKey
readMsgAuthCheckResult "\x03\x02" = ExeWrongProductKey
readMsgAuthCheckResult "\x13\x03" = ExeWrongProductExpKey
readMsgAuthCheckResult ('\x00':v) = ExeBadVersion (head v)
readMsgAuthCheckResult _ = undefined


-- | Convert Logon-related result bytes to the internal type.
readMsgAuthResult :: Char -> AuthResult
readMsgAuthResult '\x00' = AuthSuccess
readMsgAuthResult '\x01' = AuthAccountDoesNotExist
readMsgAuthResult '\x02' = AuthIncorrectPassword
readMsgAuthResult '\x04' = AuthNameExists
readMsgAuthResult '\x07' = AuthNameTooShort
readMsgAuthResult '\x08' = AuthNameIllegalChar
readMsgAuthResult '\x09' = AuthNameIllegalWord
readMsgAuthResult '\x0a' = AuthNameTooFewAlphaNum
readMsgAuthResult '\x0b' = AuthNameAdjPunc
readMsgAuthResult '\x0c' = AuthNameTooManyPunc
readMsgAuthResult '\x0e' = AuthNeedEmail
readMsgAuthResult '\x0f' = AuthError
readMsgAuthResult _ = undefined


-- | Breaks down the FriendsList message into the listed type.
readMsgFriendsList :: String -> [Friend]
readMsgFriendsList [] = []
readMsgFriendsList fm = Friend username status presence client location : readMsgFriendsList rfm
      where username = takeFst '\x00' fm
            status   = readMsgUserStatus (takeSnd '\x00' fm !! 0)
            presence = readMsgUserPresence (takeSnd '\x00' fm !! 1)
            client   = (take 4 . drop 2 . takeSnd '\x00') fm
            location = (takeFst '\x00' . drop 6 . takeSnd '\x00') fm
            rfm      = (takeSnd '\x00' . drop 6 . takeSnd '\x00') fm


-- | Converts the byte representation of user status flags to the internal type.
readMsgUserStatus :: Char -> Status
readMsgUserStatus '\x00' = NotMutual
readMsgUserStatus '\x01' = Mutual
readMsgUserStatus '\x02' = Mutual
readMsgUserStatus '\x03' = DoNotDisturb
readMsgUserStatus '\x05' = Away
readMsgUserStatus  unkn  = StatusUnknown unkn


-- | Converts the byte representation of user presence flags to internal type.
readMsgUserPresence :: Char -> Presence
readMsgUserPresence '\x00' = Offline
readMsgUserPresence '\x01' = Online
readMsgUserPresence '\x02' = OnlineInChannel
readMsgUserPresence '\x03' = OnlineInPublicGame
readMsgUserPresence '\x04' = OnlineInPrivateGame
readMsgUserPresence '\x05' = OnlineInPrivateGame
readMsgUserPresence _ = undefined


-- | Converts the byte representation of clan-related command results.
readMsgClanResult :: Char -> ClanResult
readMsgClanResult '\x00' = ClanSuccess
readMsgClanResult '\x01' = ClanInUse
readMsgClanResult '\x02' = ClanTooSoon
readMsgClanResult '\x03' = ClanNotEnoughMembers
readMsgClanResult '\x04' = ClanInvitationRejected
readMsgClanResult '\x05' = ClanInAlready
readMsgClanResult '\x07' = ClanNotAuthorised
readMsgClanResult '\x08' = ClanUserNotFound
readMsgClanResult '\x09' = ClanIsFull
readMsgClanResult '\x0a' = ClanInvalidTag
readMsgClanResult '\x0b' = ClanInvalidName
readMsgClanResult  unkn  = ClanResultUnknown unkn


-- | Converts the byte representation of clan ranks to the internal type.
readMsgClanRank :: Char -> Rank
readMsgClanRank '\x00' = Newb
readMsgClanRank '\x01' = Peon
readMsgClanRank '\x02' = Grunt
readMsgClanRank '\x03' = Shaman
readMsgClanRank '\x04' = Chieftain
readMsgClanRank _ = undefined


-- | Breaks down the ClanMemberList message into the listed type.
readMsgClanMemberList :: String -> [Member]
readMsgClanMemberList [] = []
readMsgClanMemberList cm = Member username rank presence location : readMsgClanMemberList rcm
      where username = takeFst '\x00' cm
            rank     = readMsgClanRank (takeSnd '\x00' cm !! 0)
            presence = readMsgUserPresence (takeSnd '\x00' cm !! 1)
            location = (takeFst '\x00' . drop 2 . takeSnd '\x00') cm
            rcm      = (takeSnd '\x00' . drop 2 . takeSnd '\x00') cm
