-- | Helper functions for the manipulation of 'Strings'.
module Util.String (
      -- * Extraction
        takeFstS
      , takeSndS
      -- * Padding
      , pad
      -- * Conversion
      , bigToInt
      , intToBig
      , hexToInt
      -- * Comparison
      , (===)
      , (/==)
) where

import Data.Char (toLower, digitToInt)
import Data.List (isPrefixOf)
import Util.List (padGeneric)


-- | @ takeFstS ss s = ... @
--
-- Returns the contigous string from the first character of /s/ up to the first occurence of substring /ss/ exclusive.
takeFstS :: String -> String -> String
takeFstS ss s@(sh:st) =
      if ss `isPrefixOf` s
            then []
            else sh : takeFstS ss st
takeFstS _ [] = []


-- | @ takeSndS ss s = ... @
--
-- Returns the contigous string from the /end/ of the first occurence of /ss/ up to the last character of /s/.
takeSndS :: String -> String -> String
takeSndS ss s@(_:st) =
      if ss `isPrefixOf` s
            then drop (length ss) s
            else takeSndS ss st
takeSndS _ [] = []


-- | @ pad len s = ... @
--
-- Pads input string /s/ with sufficient number of NULL bytes to make new length of /s/ an integer multiple of /len/. A
-- convenience function which uses 'padGeneric'.
pad :: Int -> String -> String
pad len s = padGeneric len '\x00' s


-- | @ bigToInt s = ... @
--
-- Returns the integer representation of a big-endian string /s/.
bigToInt :: String -> Int
bigToInt s = (sum . zipWith (*) t . map fromEnum) s
      where t = zipWith (^) (repeat 256) [0..u-1]
            u = length s


-- | @ intToBig i = ... @
--
-- Returns the big endian string representation of a an integer /i/.
intToBig :: Int -> String
intToBig i =
      if i < 256
            then toEnum i : []
            else toEnum k : intToBig j
      where (j,k) = divMod i 256


-- | @ hexToInt x = ... @
--
-- Converts hex string /x/ to its integer representation.
hexToInt :: String -> Int
hexToInt [] = 0
hexToInt x = i + is
      where (h,hs) = splitAt 2 x
            i = sum $ zipWith (*) (map digitToInt h) s
            s = zipWith (^) (repeat 16) [(u-1),(u-2)]
            u = length x
            is = hexToInt hs


-- | @ a === b = ... @
--
-- Does a case-insensitive comparison of two strings /a/ and /b/.
(===) :: String -> String -> Bool
(===) a b = (map toLower a) == (map toLower b)

-- | @ a /== b = not (a === b) @
--
-- Inversion of (===).
(/==) :: String -> String -> Bool
(/==) a b = not (a === b)