-- | Internal representation of the outgoing BNCS message. Constructors form the myriad of existing BNCS messages
-- applicable to PvPGN servers.
module Bncs.Writer.Types (
      -- * Bncs message represenation
        Bncs (..)
      -- * Data represenations
      , Method (..)
      , State (..)
      , Observer (..)
      -- * Re-exported types
      , module Core.Common.Types
) where

import Core.Common.Types
      ( Word, Dword
      , Flag (..), Rank (..)
      , Response (..))


-- | Bncs message constructors with relevant records. Reference BNetDocs for more details.
data Bncs
      = Null                        {                                   }
      | StopAdv                     {                                   }
      | GetAdvListEx                {                                   }
      | EnterChat                   {                                   }
      | JoinChannel                 {     bMethod     :: !Method
                                    ,     bChannel    :: !String        }
      | ChatCommand                 {     bMessage    :: !String        }
      | LeaveChat                   {                                   }
      | StartAdvEx3 {-- * --}       {     bState      :: !State
                                    ,     bObserver   :: !Observer
                                    ,     bName       :: !String
                                    ,     bSlots      :: !Char
                                    ,     bMapFile    :: !String        }
      | LeaveGame                   {                                   }
      | NotifyJoin                  {     bName       :: !String        }
      | Ping                        {     bPing       :: !Dword         }
      | NetGamePort                 {     bPort       :: !Word          }
      | AuthInfo                    {                                   }
      | AuthCheck                   {                                   }
      | AuthAccountCreate           {     bPassword   :: !String
                                    ,     bUsername   :: !String        }
      | AuthAccountLogon            {     bUsername   :: !String        }
      | AuthAccountLogonProof       {     bPasshash   :: !String        }
      | SetEmail                    {     bEmail      :: !String        }
      | ResetPassword               {     bUsername   :: !String
                                    ,     bEmail      :: !String        }
      | ChangeEmail                 {     bUsername   :: !String
                                    ,     bEmailOld   :: !String
                                    ,     bEmailNew   :: !String        }
      | FriendsList                 {                                   }
      | FriendsUpdate               {     bIndex      :: !Char          }
      | ClanFindCandidates          {     bTag        :: !Dword         }
      | ClanInviteMultiple          {     bClan       :: !String
                                    ,     bTag        :: !Dword
                                    ,     bCount      :: !Char
                                    ,     bUsernames  :: ![String]      }
      | ClanCreationInvitation      {     bCookie     :: !Dword
                                    ,     bTag        :: !Dword
                                    ,     bInviter    :: !String
                                    ,     bResponse   :: !Response      }
      | ClanDisband                 {                                   }
      | ClanMakeChieftain           {     bUsername   :: !String        }
      | ClanInvitation              {     bUsername   :: !String        }
      | ClanRemoveMember            {     bUsername   :: !String        }
      | ClanInvitationResponse      {     bCookie     :: !Dword
                                    ,     bTag        :: !Dword
                                    ,     bSender     :: !String
                                    ,     bResponse   :: !Response      }
      | ClanRankChange              {     bUsername   :: !String
                                    ,     bNewrank    :: !Rank          }
      | ClanSetMotd                 {     bMotd       :: !String        }
      | ClanMemberList              {                                   }


-- | Channel to join upon logon.
data Method
      = Custom                      {                                   }
      | Default                     {                                   }
      | Forced                      {                                   }


-- | StartAdvEx3 state.
data State
      = New                         {                                   }
      | Private                     {                                   }
      | Full                        {                                   }
      | ContainsPlayers             {                                   }
      | InProgress                  {                                   }


-- | StartAdvEx3 observer flags.
data Observer
      = None                        {                                   }
      | All                         {                                   }