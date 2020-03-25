module Auis.Writer.Types (
        Auis (..)
      , Users (..)
      , module Core.Common.Types
) where

import Core.Common.Types (Word, Dword, Flag (..), Member (..), Presence (..), Rank (..))


-- | Outgoing Auis message contructors with the relevant records corresponding to user interface output actions. Naming
-- convention for constructors is /VerbNoun/.
data Auis
      = PrintTalk                   {     aAdmin      :: !Bool
                                    ,     aUsername   :: !String
                                    ,     aMessage    :: !String        }
      | PrintWhisper                {     aSent       :: !Bool
                                    ,     aAdmin      :: !Bool
                                    ,     aUsername   :: !String
                                    ,     aMessage    :: !String        }
      | PrintEmote                  {     aUsername   :: !String
                                    ,     aMessage    :: !String        }
      | PrintError                  {     aMessage    :: !String        }
      | PrintGeneral                {     aMessage    :: !String        }
      | PrintInternal               {     aMessage    :: !String        }
      | PrintSpecial                {     aMessage    :: !String        }
      | AddChannelUser              {     aJoined     :: !Bool
                                    ,     aFlag       :: !Flag
                                    ,     aPing       :: !Dword
                                    ,     aUsername   :: !String
                                    ,     aClient     :: !Dword
                                    ,     aIcon       :: !Dword
                                    ,     aLevel      :: !String
                                    ,     aTag        :: !Dword         }
      | RemoveChannelUser           {     aFlag       :: !Flag
                                    ,     aPing       :: !Dword
                                    ,     aUsername   :: !String        }
      | UpdateChannelUser           {     aFlag       :: !Flag
                                    ,     aPing       :: !Dword
                                    ,     aUsername   :: !String
                                    ,     aClient     :: !Dword
                                    ,     aIcon       :: !Dword
                                    ,     aLevel      :: !String
                                    ,     aTag        :: !Dword         }
      | ClearChannelUsers           {     aChannel    :: !String        }
      | ShowChannelInfo             {     aChannel    :: !String
                                    ,     aCount      :: !Int           }      
--       | AddFriend                   {     aUsername   :: !String
--                                     ,     aStatus     :: !Status
--                                     ,     aPresence   :: !Presence
--                                     ,     aClient     :: !Info
--                                     ,     aLocation   :: !String        }
--       | MoveFriend                  {     aOldIndex   :: !Char
--                                     ,     aNewIndex   :: !Char          }
--       | RemoveFriend                {     aIndex      :: !Char          }
--       | UpdateFriend                {     aIndex      :: !Char
--                                     ,     aPresence   :: !Presence
--                                     ,     aStatus     :: !Status
--                                     ,     aClient     :: !Info
--                                     ,     aLocation   :: !String        }
--       | ClearFriends                {                                   }
      | ListClanMembers             {     aMembers    :: ![Member]      }
      | RemoveClanMember            {     aUsername   :: !String        }
      | UpdateClanMember            {     aMember     :: !Member        }
      | ClearClanMembers            {                                   }
      | ShowClanInfo                {     aTag        :: !Dword
                                    ,     aCount      :: !Int           }


-- | User data type.
data Users = Users
      { uFlag                 :: !Flag
      , uPing                 :: !Dword
      , uUsername             :: !String
      , uClient               :: !Dword
      , uIcon                 :: !Dword
      , uLevel                :: !String
      , uTag                  :: !Dword
      }


-- Basic outputs:
      -- Print normal chat messages (white text)
      -- Print whispers (green text)
      -- Print admin messages (blue text)
      -- Print emotes (grey text)
      -- Print error messages (red text)
      -- Print general messages (light blue text)

      -- Print internally generated messages

      -- Update channel user list (add/remove/refresh users)
      -- Update friends list (add/remove/move/refresh friends) *
      -- Update clan member list (add/remove/refresh members)