-- | Internal representation of the incoming BNCS message. Constructors form the myriad of existing BNCS messages
-- applicable to PvPGN servers.
module Bncs.Reader.Types (
      -- * Bncs message representation
        Bncs (..)
      -- * Chat event represenation
      , Event (..)
      -- * Data representations
      , Result (..)
      , ExeResult (..)
      , AuthResult (..)
      , ClanResult (..)
      -- * Re-exported types
      , module Core.Common.Types
) where

import Core.Common.Types
      ( Word , Dword
      , Game (..), Friend (..), Member (..)
      , Flag (..), Status (..), Presence (..), Rank (..))



-- | Bncs message constructors with relevant records. Reference BNetDocs for more details.
data Bncs
      = Null                        {                                   }
      | GetAdvListEx                {     bCount      :: !Char
                                    ,     bGames      :: ![Game]        }
      | EnterChat                   {     bUsername   :: !String
                                    ,     bClient     :: !Dword
                                    ,     bAccount    :: !String        }
      | ChatEvent                   {     bFlag       :: !Flag
                                    ,     bPing       :: !Dword
                                    ,     bUsername   :: !String
                                    ,     bEvent      :: !Event         }
      | FloodDetected               {                                   }
      | MessageBox                  {     bText       :: !String
                                    ,     bCaption    :: !String        }
      | StartAdvEx3                 {     bResult     :: !Result        }
      | Ping                        {     bPing       :: !Dword          }
      | AuthInfo                    {                                   }
      | AuthCheck                   {     bExeResult  :: !ExeResult
                                    ,     bMessage    :: !String        }
      | AuthAccountCreate           {     bAuthResult :: !AuthResult    }
      | AuthAccountLogon            {     bAuthResult :: !AuthResult    }
      | AuthAccountLogonProof       {     bAuthResult :: !AuthResult
                                    ,     bMessage    :: !String        }
      | SetEmail                    {                                   }
      | FriendsList                 {     bCount      :: !Char
                                    ,     bFriends    :: ![Friend]      }
      | FriendsUpdate               {     bIndex      :: !Char
                                    ,     bPresence   :: !Presence
                                    ,     bStatus     :: !Status
                                    ,     bClient     :: !Dword
                                    ,     bLocation   :: !String        }
      | FriendsAdd                  {     bUsername   :: !String
                                    ,     bStatus     :: !Status
                                    ,     bPresence   :: !Presence
                                    ,     bClient     :: !Dword
                                    ,     bLocation   :: !String        }
      | FriendsRemove               {     bIndex      :: !Char          }
      | FriendsPosition             {     bIndexOld   :: !Char
                                    ,     bIndexNew   :: !Char          }
      | ClanFindCandidates          {     bClanResult :: !ClanResult
                                    ,     bCount      :: !Char
                                    ,     bUsernames  :: ![String]      }
      | ClanInviteMultiple          {     bClanResult :: !ClanResult
                                    ,     bUsernames  :: ![String]      }
      | ClanCreationInvitation      {     bCookie     :: !Dword
                                    ,     bTag        :: !Dword
                                    ,     bClan       :: !String
                                    ,     bInviter    :: !String
                                    ,     bCount      :: !Char
                                    ,     bUsernames  :: ![String]      }
      | ClanDisband                 {     bClanResult :: !ClanResult    }
      | ClanMakeChieftain           {     bClanResult :: !ClanResult    }
      | ClanInfo                    {     bTag        :: !Dword
                                    ,     bRank       :: !Rank          }
      | ClanQuitNotify              {     bClanResult :: !ClanResult    }
      | ClanInvitation              {     bClanResult :: !ClanResult    }
      | ClanRemoveMember            {     bClanResult :: !ClanResult    }
      | ClanInvitationResponse      {     bCookie     :: !Dword
                                    ,     bTag        :: !Dword
                                    ,     bClan       :: !String
                                    ,     bInviter    :: !String        }
      | ClanRankChange              {     bClanResult :: !ClanResult    }
      | ClanMemberList              {     bCount      :: !Char
                                    ,     bMembers    :: ![Member]      }
      | ClanMemberRemoved           {     bUsername   :: !String        }
      | ClanMemberStatusChange      {     bUsername   :: !String
                                    ,     bRank       :: !Rank
                                    ,     bPresence   :: !Presence
                                    ,     bLocation   :: !String        }
      | ClanMemberRankChange        {     bRankOld    :: !Rank
                                    ,     bRankNew    :: !Rank
                                    ,     bChanger    :: !String        }
      | MessageUnknown              {     bId         :: !Char
                                    ,     bData       :: !String        }
      | MessageBadForm              {     bHead       :: !Dword
                                    ,     bBody       :: !String        }


-- | Chat events.
data Event
      = ShowUser                    {     eClient     :: !Dword
                                    ,     eIcon       :: !Dword
                                    ,     eLevel      :: !String
                                    ,     eTag        :: !Dword         }
      | Join                        {     eClient     :: !Dword
                                    ,     eIcon       :: !Dword
                                    ,     eLevel      :: !String
                                    ,     eTag        :: !Dword         }
      | Leave                       {                                   }
      | Whisper                     {     eMessage    :: !String        }
      | Talk                        {     eMessage    :: !String        }
      | Broadcast                   {     eMessage    :: !String        }
      | Channel                     {     eInfo       :: !String        }
      | UserFlags                   {     eClient     :: !Dword
                                    ,     eIcon       :: !Dword
                                    ,     eLevel      :: !String
                                    ,     eTag        :: !Dword         }
      | WhisperSent                 {     eMessage    :: !String        }
      | ChannelFull                 {     eMessage    :: !String        }
      | ChannelDoesNotExist         {     eMessage    :: !String        }
      | ChannelRestricted           {     eMessage    :: !String        }
      | Info                        {     eMessage    :: !String        }
      | Error                       {     eMessage    :: !String        }
      | Emote                       {     eMessage    :: !String        }


-- | Generic result flags.
data Result
      = Success                     {                                   }
      | Failure                     {                                   }
      | ResultUnknown               {     rResult     :: !Char          }


-- | Version result flags.
data ExeResult
      = ExeSuccess                  {                                   }
      | ExeTooOld                   {                                   }
      | ExeBadVersion               {     rVersion    :: !Char          }
      | ExeTooNew                   {                                   }
      | ExeBadKey                   {                                   }
      | ExeBadExpKey                {                                   }
      | ExeKeyInUse                 {                                   }
      | ExeExpKeyInUse              {                                   }
      | ExeBannedKey                {                                   }
      | ExeBannedExpKey             {                                   }
      | ExeWrongProductKey          {                                   }
      | ExeWrongProductExpKey       {                                   }


-- | Authentication result flags.
data AuthResult
      = AuthSuccess                 {                                   }
      | AuthAccountDoesNotExist     {                                   }
      | AuthIncorrectPassword       {                                   }
      | AuthNameExists              {                                   }
      | AuthNameTooShort            {                                   }
      | AuthNameIllegalChar         {                                   }
      | AuthNameIllegalWord         {                                   }
      | AuthNameTooFewAlphaNum      {                                   }
      | AuthNameAdjPunc             {                                   }
      | AuthNameTooManyPunc         {                                   }
      | AuthNeedEmail               {                                   }
      | AuthError                   {                                   }


-- | Clan result flags.
data ClanResult
      = ClanSuccess                 {                                   }
      | ClanInUse                   {                                   }
      | ClanTooSoon                 {                                   }
      | ClanNotEnoughMembers        {                                   }
      | ClanInvitationRejected      {                                   }
      | ClanInAlready               {                                   }
      | ClanNotAuthorised           {                                   }
      | ClanUserNotFound            {                                   }
      | ClanIsFull                  {                                   }
      | ClanInvalidTag              {                                   }
      | ClanInvalidName             {                                   }
      | ClanResultUnknown           {     cResult     :: !Char          }
