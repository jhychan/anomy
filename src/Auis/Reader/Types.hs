module Auis.Reader.Types (
        Auis (..)
      , module Core.Common.Types
) where

import Core.Common.Types (Rank (..), Response (..))


-- | Incoming Auis message contructors with the relevant records corresponding to user interface input actions. Naming
-- convention for constructors is /VerbNoun/.
data Auis
      = SendChat                    {     aMessage    :: !String        }
      | ShowAbout                   {                                   }
      | ShowGames                   {                                   }
      | ShowFriends                 {                                   }
      | ShowClanMembers             {                                   }
      | ExecLogonAuto               {                                   }
      | ExecLogon                   {                                   }
      | ExecRestart                 {                                   }
--       | SetEmail                    {     aEmail      :: !String        }
--       | ResetPassword               {     aEmail      :: !String        }
--       | ResetEmail                  {     aOldEmail   :: !String
--                                     ,     aNewEmail   :: !String        }
      | ExecClanCreation            {     aTag        :: !String
                                    ,     aClan       :: !String        }
      | SendClanInvite              {     aUsername   :: !String        }
      | ReplyClanInvite             {     aResponse   :: !Response      }
      | DisbandClan                 {                                   }
      | SetClanChieftain            {     aUsername   :: !String        }
      | RemoveClanMember            {     aUsername   :: !String        }
      | LeaveClan                   {                                   }
      | ChangeClanMemberRank        {     aUsername   :: !String
                                    ,     aNewRank    :: !Rank          }
      | SetClanMotd                 {     aMotd       :: !String        }


-- Basic inputs:
      -- Send chat message

      -- Show games list
      -- Show friends list
      -- Show clan members

      -- Connect/reconnect (!!)
      -- Execute logon sequence
      -- Set email *
      -- Reset password *
      -- Reset email *

      -- Execute clan creation sequence
      -- Invite user to clan
      -- Reply clan invitation
      -- Disband clan
      -- Set clan chieftain
      -- Remove clan member from clan
      -- Leave clan
      -- Change clan member rank