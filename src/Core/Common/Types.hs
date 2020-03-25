-- | Common data types necessary for 'Core' and 'Bncs' modules, and possibly 'Auis' modules and others.
module Core.Common.Types (
      -- * Convenience types
        Word
      , Dword
      -- * Data-carrying types
      , Game (..)
      , Friend (..)
      , Member (..)
      -- * Data representations
      , Flag (..)
      , Status (..)
      , Presence (..)
      , Rank (..)
      , Response (..)
) where


-- | To differentiate message strings from the commonly seen 2-byte data strings.
type Word = String
-- | To differentiate message strings from the commonly seen 4-byte data strings.
type Dword = String


-- | Games list item.
data Game
      = Game                        {     gPort       :: !Word
                                    ,     gIP         :: !Dword
                                    ,     gName       :: !String
                                    ,     gMapFile    :: !String
                                    ,     gCreator    :: !String        }


-- | Channel user flags. Shared with BNCS modules.
data Flag
      = User                        {                                   }
      | Administrator               {                                   }
      | TempOperator                {                                   }
      | Voiced                      {                                   }
      | Operator                    {                                   }
      | FirstJoin                   {                                   }
      | Squelched                   {                                   }
      | FlagUnknown                 {     fFlag       :: !Char          }


-- | Friends entity.
data Friend
      = Friend                      {     fUsername   :: !String
                                    ,     fStatus     :: !Status
                                    ,     fPresence   :: !Presence
                                    ,     fClient     :: !Dword
                                    ,     fLocation   :: !String        }


-- | Friend status.
data Status
      = NotMutual                   {                                   }
      | Mutual                      {                                   }
      | DoNotDisturb                {                                   }
      | Away                        {                                   }
      | StatusUnknown               {     sStatus     :: !Char          }


-- | Friend presence.
data Presence
      = Offline                     {                                   }
      | Online                      {                                   }
      | OnlineInChannel             {                                   }
      | OnlineInPublicGame          {                                   }
      | OnlineInPrivateGame         {                                   }
      deriving (Eq)


-- | Clan member entity.
data Member
      = Member                      {     cUsername   :: !String
                                    ,     cRank       :: !Rank
                                    ,     cPresence   :: !Presence
                                    ,     cLocation   :: !String        }


-- | Clan rankings. Shared with BNCS modules.
data Rank
      = Newb                        {                                   }
      | Peon                        {                                   }
      | Grunt                       {                                   }
      | Shaman                      {                                   }
      | Chieftain                   {                                   }
      deriving (Show, Eq)


-- | Clan invitation reponse flags.
data Response
      = Reject                      {                                   }
      | Accept                      {                                   }