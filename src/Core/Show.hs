-- | Non-class instantiated show methods for String-synonymous types only.
module Core.Show (
        showPing
      , showClient
      , showIcon
      , showLevel
      , showTag
) where

import Core.Common.Types (Dword)
import Util.String (bigToInt)


-- | Convert raw ping result into text.
showPing :: Dword -> String
showPing p = shows (bigToInt p) "ms"


-- | Decipher client dwords found in 'Bncs.Reader.Types'.
showClient :: Dword -> String
showClient "PX3W" = "Warcraft III Frozen Throne"
showClient "3RAW" = "Warcraft III Reign of Chaos"
showClient "TAHC" = "Chat"
showClient "PX2D" = "Diablo II Lord of Destruction"
showClient "VD2D" = "Diablo II"
showClient "PXES" = "Starcraft Brood War"
showClient "RATS" = "Starcraft"
showClient  unkn  = "Unkown client (" ++ unkn ++ ")"


-- | Map icon dwords in 'Bncs.Reader.Types' to Warcraft III unit/character names.
showIcon :: Dword -> String
showIcon  [  ] = "No icon"
showIcon "PX3W" = "Frozen Throne"
showIcon "1R3W" = "Slave"
showIcon "2R3W" = "Myrmidon"
showIcon "3R3W" = "Siren"
showIcon "4R3W" = "Dragon Turtle"
showIcon "5R3W" = "Sea Witch"
showIcon "6R3W" = "Illidan"
showIcon "1H3W" = "Peasant"
showIcon "2H3W" = "Rifleman"
showIcon "3H3W" = "Sorceress"
showIcon "4H3W" = "Spell Breaker"
showIcon "5H3W" = "Blood Mage"
showIcon "6H3W" = "Jaina"
showIcon "1O3W" = "Peon"
showIcon "2O3W" = "Troll Headhunter"
showIcon "3O3W" = "Shaman"
showIcon "4O3W" = "Spirit Walker"
showIcon "5O3W" = "Shadow Hunter"
showIcon "6O3W" = "Rexxar"
showIcon "1U3W" = "Necrolyte"
showIcon "2U3W" = "Crypt Fiend"
showIcon "3U3W" = "Banshee"
showIcon "4U3W" = "Destroyer"
showIcon "5U3W" = "Crypt Lord"
showIcon "6U3W" = "Sylvanas"
showIcon "1N3W" = "Wisp"
showIcon "2N3W" = "Huntress"
showIcon "3N3W" = "Druid of the Talon"
showIcon "4N3W" = "Dryad"
showIcon "5N3W" = "Keeper of the Grove"
showIcon "6N3W" = "Maiev"
showIcon "1T3W" = "Fel Orc Peon"
showIcon "2T3W" = "Felguard"
showIcon "3T3W" = "Infernal"
showIcon "4T3W" = "Doomguard"
showIcon "5T3W" = "Pit Lord"
showIcon "6T3W" = "Archimonde"
showIcon  unkn  = "Unknown icon (" ++ unkn ++ ")"


-- | Provides string description of level attribute found in 'Bncs.Reader.Types'.
showLevel :: String -> String
showLevel [ ] = "No level"
showLevel "0" = "No level"
showLevel lvl = "Level " ++ lvl


-- | Provides string description of clan tag attribute found in 'Bncs.Reader.Types'.
showTag :: Dword -> String
showTag [ ] = "No clan"
showTag tag = "Clan " ++ tag