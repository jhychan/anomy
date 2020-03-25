-- | Outgoing BNCS message constructor processing.
module Bncs.Writer (
        writeMsg
      , Bncs
) where

import Bncs.Writer.Types
import Util.Crypto (hash)
import Util.String (pad)

import Data.List (intercalate)


-- | Wrapper for writeMsg. Prepends outgoing BNCS messages with the correct BNCS header information.
writeMsgWrap :: Char -> String -> String
writeMsgWrap mid m = '\xff' : mid : mlen : '\x00' : m
      where mlen = toEnum (length m + 4)


-- | Takes a complete 'Bncs.Writer.Types.Bncs' constructor and produces the corresponding outgoing BNCS packet.
writeMsg :: Bncs -> String

writeMsg (Null) = writeMsgWrap
        '\x00' $!
      [                                   ] -- (Blank)

writeMsg (StopAdv) = writeMsgWrap
        '\x02' $!
      [                                   ] -- (Blank)

writeMsg (GetAdvListEx) = writeMsgWrap
        '\x09' $!
      [ '\x00' , '\xe0' , '\x7f' , '\x00' , -- Product-spec. cond. 1&2 (Unkn.)
        '\x00' , '\x00' , '\x00' , '\x00' , -- Product-spec. cond. 3 (Unknown)
        '\x00' , '\x00' , '\x00' , '\x00' , -- Product-spec. cond. 4 (Unknown)
        '\x14' , '\x00' , '\x00' , '\x00' , -- Maximum number of games to list (Copied)
        '\x00' , '\x00' , '\x00' , '\x00' , -- Game name (Var)
        '\x00' , '\x00' , '\x00' , '\x00' , -- Game password (Var)
        '\x00' , '\x00' , '\x00' , '\x00' ] -- Game stats (Var)

writeMsg (EnterChat) = writeMsgWrap
        '\x0a' $!
      [ '\x00' , '\x00'                   ] -- Null

writeMsg (JoinChannel tmethod channel) = writeMsgWrap
        '\x0c' $!
      [ method , '\x00' , '\x00' , '\x00' ] -- Flags (Var)
        ++ channel ++                       -- Channel (Var)
      [ '\x00'                            ] -- -
      where method = writeMsgJoinChannelMethod tmethod

writeMsg (ChatCommand message) = writeMsgWrap
        '\x0e' $!
        message ++                          -- Text (Var)
      [ '\x00'                            ] -- -

writeMsg (LeaveChat) = writeMsgWrap
        '\x10' $!
      [                                   ] -- (Blank)

writeMsg (StartAdvEx3 tstate tobserver name slots mapfile) = writeMsgWrap
        '\x1c' $!
      [ state  , '\x00' , '\x00' , '\x00' , -- State (Var)
        '\x00' , '\x00' , '\x00' , '\x00' , -- Time since creation (Var)
        '\x01' , '\x20'                   , -- Game type (Custom)
        observer        , '\x00'          , -- Parameter (Var)
        '\xff' , '\x03' , '\x00' , '\x00' , -- Unknown (Copied)
        '\x00' , '\x00' , '\x00' , '\x00' ] -- Unkown (Null)
        ++ name ++                          -- Game name (Var UTF-8)
      [ '\x00'                            , -- -
        '\x00'                            , -- Game password (Var)
        slots                             , -- Free slots (Var ASCII Byte)
        '\x39' , '\x39' , '\x39' , '\x39' ] -- Hosting count (Var ASCII)
        ++ mapfile ++ undefined             -- Game statstring (Var)
      where state    = writeMsgStartAdvEx3State tstate
            observer = writeMsgStartAdvEx3Observer tobserver

writeMsg (LeaveGame) = writeMsgWrap
        '\x1f' $!
      [                                   ] -- (Blank)

writeMsg (NotifyJoin name) = writeMsgWrap
        '\x22' $!
      [ '\x00' , '\x00' , '\x00' , '\x00' , -- Product ID (Null)
        '\x18' , '\x00' , '\x00' , '\x00' ] -- Version Byte (18)
        ++ name ++                          -- Game name (Var)
      [ '\x00'                            , -- -
        '\x00'                            ] -- Game password (Var)

writeMsg (Ping ping) = writeMsgWrap
        '\x25' $!
        ping                                -- Timestamp (Var)

writeMsg (NetGamePort port) = writeMsgWrap
        '\x45' $!
        port                                -- Port (Var)

writeMsg (AuthInfo) = writeMsgWrap
        '\x50' $!
      [ '\x00' , '\x00' , '\x00' , '\x00' , -- Protocol ID (0)
        '\x36' , '\x38' , '\x58' , '\x49' , -- Platform ID IX86 (68XI)
        '\x50' , '\x58' , '\x33' , '\x57' , -- Product ID W3XP (PX3W)
        '\x1A' , '\x00' , '\x00' , '\x00' , -- Version Byte (1A)
        '\x53' , '\x55' , '\x6e' , '\x65' , -- Product language enUS (SUne)
        '\xc0' , '\xa8' , '\x01' , '\x01' , -- Local IP for NAT (Not used)*
        '\xa8' , '\xfd' , '\xff' , '\xff' , -- Time zone bias (Not used)*
        '\x09' , '\x0c' , '\x00' , '\x00' , -- Locale ID (Not used)*
        '\x09' , '\x04' , '\x00' , '\x00' , -- Language ID (Not used)*
        '\x41' , '\x55' , '\x53' , '\x00' , -- Country abreviation (AUS)
        '\x41' , '\x75' , '\x73' , '\x74' , -- Country (Australia)
        '\x72' , '\x61' , '\x6c' , '\x69' , -- -
        '\x61' , '\x00'                   ] -- -

writeMsg (AuthCheck) = writeMsgWrap
        '\x51' $!
      [ '\x00' , '\x00' , '\x00' , '\x00' , -- Client Token (Not used)
        '\x01' , '\x00' , '\x1A' , '\x01' , -- EXE Version (1.26a)
        '\xc2' , '\xce' , '\xe7' , '\xf2' , -- EXE Hash (From CheckRevision)
        '\x02' , '\x00' , '\x00' , '\x00' , -- Number of keys in this packet (2)
        '\x00' , '\x00' , '\x00' , '\x00' , -- Unknown (Null)
        '\x1a' , '\x00' , '\x00' , '\x00' , -- Key 1 Length (26)
        '\x0f' , '\x00' , '\x00' , '\x00' , -- Key 1 Product Value (Copied)
        '\x63' , '\xd4' , '\x30' , '\x00' , -- Key 1 Public Value (Copied)
        '\x00' , '\x00' , '\x00' , '\x00' , -- Unknown (Null)
        '\x00' , '\x00' , '\x00' , '\x00' , -- Hashed Key 1 Data (Not used)*
        '\x00' , '\x00' , '\x00' , '\x00' , -- -
        '\x00' , '\x00' , '\x00' , '\x00' , -- -
        '\x00' , '\x00' , '\x00' , '\x00' , -- -
        '\x00' , '\x00' , '\x00' , '\x00' , -- -
        '\x1a' , '\x00' , '\x00' , '\x00' , -- Key 2 Length (26)
        '\x12' , '\x00' , '\x00' , '\x00' , -- Key 2 Product Value (Copied)
        '\x58' , '\x54' , '\x02' , '\x01' , -- Key 2 Public Value (Copied)
        '\x00' , '\x00' , '\x00' , '\x00' , -- Unknown (Null)
        '\x00' , '\x00' , '\x00' , '\x00' , -- Hashed Key 2 Data (Not used)*
        '\x00' , '\x00' , '\x00' , '\x00' , -- -
        '\x00' , '\x00' , '\x00' , '\x00' , -- -
        '\x00' , '\x00' , '\x00' , '\x00' , -- -
        '\x00' , '\x00' , '\x00' , '\x00' , -- -
        '\x77' , '\x61' , '\x72' , '\x33' , -- EXE Information (war3)
        '\x2e' , '\x65' , '\x78' , '\x65' , -- (.exe)
        '\x20' , '\x30' , '\x30' , '\x2f' , -- ( 00/)
        '\x30' , '\x30' , '\x2f' , '\x30' , -- (00/0)
        '\x30' , '\x20' , '\x30' , '\x30' , -- (0 00)
        '\x3a' , '\x30' , '\x30' , '\x3a' , -- (:00:)
        '\x30' , '\x30' , '\x20' , '\x34' , -- (00 4)
        '\x37' , '\x31' , '\x30' , '\x34' , -- (7104)
        -- '\x20' , '\x30' , '\x38' , '\x2f' , -- ( 08/)
        -- '\x30' , '\x37' , '\x2f' , '\x30' , -- (07/0)
        -- '\x39' , '\x20' , '\x31' , '\x39' , -- (9 19)
        -- '\x3a' , '\x32' , '\x30' , '\x3a' , -- (:20:)
        -- '\x35' , '\x33' , '\x20' , '\x34' , -- (53 4)
        -- '\x37' , '\x31' , '\x30' , '\x34' , -- (7104)
        '\x30' , '\x00'                   , -- (0)
        '\x55' , '\x6e' , '\x6b' , '\x6e' , -- CD Key owner name ("Unknown")
        '\x70' , '\x77' , '\x6e' , '\x00' ] -- -

writeMsg (AuthAccountCreate password username) = writeMsgWrap
        '\x52' $!
      [ '\x00' , '\x00' , '\x00' , '\x00' , -- Salt (Ignored)
        '\x00' , '\x00' , '\x00' , '\x00' , -- -
        '\x00' , '\x00' , '\x00' , '\x00' , -- -
        '\x00' , '\x00' , '\x00' , '\x00' , -- -
        '\x00' , '\x00' , '\x00' , '\x00' , -- -
        '\x00' , '\x00' , '\x00' , '\x00' , -- -
        '\x00' , '\x00' , '\x00' , '\x00' , -- -
        '\x00' , '\x00' , '\x00' , '\x00' ] -- -
        ++ (pad 32 password) ++             -- Plain text password (Var)
        username ++                         -- Account username (Var)
      [ '\x00'                            ] -- -

writeMsg (AuthAccountLogon username) = writeMsgWrap
        '\x53' $!
      [ '\x00' , '\x00' , '\x00' , '\x00' , -- Client Key A (Not used)*
        '\x00' , '\x00' , '\x00' , '\x00' , -- -
        '\x00' , '\x00' , '\x00' , '\x00' , -- -
        '\x00' , '\x00' , '\x00' , '\x00' , -- -
        '\x00' , '\x00' , '\x00' , '\x00' , -- -
        '\x00' , '\x00' , '\x00' , '\x00' , -- -
        '\x00' , '\x00' , '\x00' , '\x00' , -- -
        '\x00' , '\x00' , '\x00' , '\x00' ] -- -
        ++ username ++                      -- Username (Var)
      [ '\x00'                            ] -- -

writeMsg (AuthAccountLogonProof password) = writeMsgWrap
        '\x54' $!
        passhash ++                         -- Client Password Proof M1 (Var)
      [ '\x00'                            ] -- -
      where passhash = hash password

writeMsg (SetEmail email) = writeMsgWrap
        '\x59' $!
        email ++                            -- Email address (Var)
      [ '\x00'                            ] -- -

writeMsg (ResetPassword username email) = writeMsgWrap
        '\x5a' $!
        username ++                         -- Account to reset (Var)
      [ '\x00'                            ] -- -
        ++ email ++                         -- Email address (Var)
      [ '\x00'                            ] -- -

writeMsg (ChangeEmail username emailold emailnew) = writeMsgWrap
        '\x5b' $!
        username ++                         -- Account to change (Var)
      [ '\x00'                            ] -- -
        ++ emailold ++                      -- Old email to verify (Var)
      [ '\x00'                            ] -- -
        ++ emailnew ++                      -- Email to change to (Var)
      [ '\x00'                            ] -- -

writeMsg (FriendsList) = writeMsgWrap
        '\x65' $!
      [                                   ] -- (Blank)

writeMsg (FriendsUpdate index) = writeMsgWrap
        '\x66' $!
      [ index                             ] -- Friend list index (Var)

writeMsg (ClanFindCandidates tag) = writeMsgWrap
        '\x70' $!
      [ '\x01' , '\x00' , '\x00' , '\x00' ] -- Cookie (Var 1)
        ++ (reverse (pad 4 tag))            -- Clan tag

writeMsg (ClanInviteMultiple clan tag count usernames) = writeMsgWrap
        '\x71' $!
      [ '\x01' , '\x00' , '\x00' , '\x00' ] -- Cookie (Var 1)
        ++ clan ++                          -- Clan name (Var)
      [ '\x00'                            ] -- -
        ++ (reverse (pad 4 tag)) ++         -- Clan tag (Var)
      [ count                             ] -- Username count (Var Byte)
        ++ (intercalate "\x00" usernames) ++      -- Usernames list (Var)
      [ '\x00'                            ] -- -

writeMsg (ClanCreationInvitation cookie tag sender tresponse) = writeMsgWrap
        '\x72' $!
        cookie ++                           -- Cookie (Var)
        (reverse (pad 4 tag)) ++            -- Clan tag (Var)
        sender ++                           -- Invitation sender (Var)
      [ '\x00'                            , -- -
        response                          ] -- Response code (Var Byte)
      where response = writeMsgClanInvitationResponse tresponse

writeMsg (ClanDisband) = writeMsgWrap
        '\x73' $!
      [ '\x01' , '\x00' , '\x00' , '\x00' ] -- Cookie (Var 1)

writeMsg (ClanMakeChieftain username) = writeMsgWrap
        '\x74' $!
      [ '\x01' , '\x00' , '\x00' , '\x00' ] -- Cookie (Var 1)
        ++ username ++                      -- New chieftain username (Var)
      [ '\x00'                            ] -- -

writeMsg (ClanInvitation username) = writeMsgWrap
        '\x77' $!
      [ '\x01' , '\x00' , '\x00' , '\x00' ] -- Cookie (Var 1)
        ++ username ++                      -- Invitee username (Var)
      [ '\x00'                            ] -- -

writeMsg (ClanRemoveMember username) = writeMsgWrap
        '\x78' $!
      [ '\x01' , '\x00' , '\x00' , '\x00' ] -- Cookie (Var 1)
        ++ username ++                      -- Removee username (Var)
      [ '\x00'                            ] -- -

writeMsg (ClanInvitationResponse cookie tag inviter tresponse) = writeMsgWrap
        '\x79' $!
        cookie ++                           -- Cookie (Var)
        (reverse (pad 4 tag)) ++            -- Clan tag (Var)
        inviter ++                          -- Inviter (Var)
      [ '\x00'                            , -- -
        response                          ] -- Response code (Var Byte)
      where response = writeMsgClanInvitationResponse tresponse

writeMsg (ClanRankChange username tnewrank) = writeMsgWrap
        '\x7a' $!
      [ '\x01' , '\x00' , '\x00' , '\x00' ] -- Cookie (Var 1)
        ++ username ++                      -- Changee username (Var)
      [ '\x00'                            , -- -
        newrank                           ] -- New rank (Var Byte)
      where newrank = writeMsgClanRank tnewrank

writeMsg (ClanSetMotd motd) = writeMsgWrap
        '\x7b' $!
      [ '\x01' , '\x00' , '\x00' , '\x00' ] -- Cookie (Var 1)
        ++ motd ++                          -- Motd (Var)
      [ '\x00'                            ] -- -

writeMsg (ClanMemberList) = writeMsgWrap
        '\x7d' $!
      [ '\x01' , '\x00' , '\x00' , '\x00' ] -- Cookie (Var 1)


-- | Turn channel join types into corresponding BNCS bytes.
writeMsgJoinChannelMethod :: Method -> Char
writeMsgJoinChannelMethod Custom = '\x00'
writeMsgJoinChannelMethod Default = '\x01'
writeMsgJoinChannelMethod Forced = '\x02'


-- | Turn game state types into corresponding BNCS bytes.
writeMsgStartAdvEx3State :: State -> Char
writeMsgStartAdvEx3State New = '\x00'
writeMsgStartAdvEx3State Private = '\x01'
writeMsgStartAdvEx3State Full = '\x02'
writeMsgStartAdvEx3State ContainsPlayers = '\x04'
writeMsgStartAdvEx3State InProgress = '\x08'


-- | Turn observer types into corresponding BNCS bytes.
writeMsgStartAdvEx3Observer :: Observer -> Char
writeMsgStartAdvEx3Observer None = '\x49'
writeMsgStartAdvEx3Observer All = '\x19'


-- | Turn clan invitation responses into their BNCS bytes.
writeMsgClanInvitationResponse :: Response -> Char
writeMsgClanInvitationResponse Reject = '\x04'
writeMsgClanInvitationResponse Accept = '\x06'


-- | Turn clan rank types into corresponding BNCS bytes.
writeMsgClanRank :: Rank -> Char
writeMsgClanRank Newb = '\x00'
writeMsgClanRank Peon = '\x01'
writeMsgClanRank Grunt = '\x02'
writeMsgClanRank Shaman = '\x03'
writeMsgClanRank Chieftain = '\x04'
