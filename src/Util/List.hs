-- | Helper functions for the manipulation of Lists.
module Util.List (
      -- * Extraction
        takeFst
      , takeSnd
      -- * Splitting
      , split
      , splitInf
      , splitByN
      , splitByN2
      -- * Padding
      , padGeneric
      , padGenericL
      -- * Replacing
      , replace
      , replaceL
) where

import Data.List (isPrefixOf)


-- | @ takeFst e x = ... @
--
-- Returns the contigous list from the first element of /x/ up to the first occurence of /e/ exclusive.
takeFst :: Eq a => a -> [a] -> [a]
takeFst e x = takeWhile (/=e) x


-- | @ takeSnd e x = ... @
--
-- Returns the contigous list from the first occurence of /e/ exclusive to the last element of /x/.
takeSnd :: Eq a => a -> [a] -> [a]
takeSnd e x = drop 1 (dropWhile (/=e) x)


-- | @ split x xs = ... @
--
-- Splits the list /xs/ into elements of /xs/ separated by the delimiter /x/. Consumes the delimiter in the process.
split :: Eq a => a -> [a] -> [[a]]
split _ [] = []
split x xs = takeFst x xs : split x (takeSnd x xs)


-- | @ splitInf x xs = ... @
--
-- Same as split, but appends the end of the list with more empty lists infinitely.
splitInf :: Eq a => a -> [a] -> [[a]]
splitInf x xs = takeFst x xs : splitInf x (takeSnd x xs)


-- | @ splitByN n xs = ... @
--
-- Separates list /xs/ into elements of length /n/.
splitByN :: Int -> [a] -> [[a]]
splitByN _ [] = []
splitByN n xs = take n xs : splitByN n (drop n xs)


-- | @ splitByN2 n xs = ... @
--
-- Splitting of nested lists.
splitByN2 :: Int -> [[a]] -> [[a]]
splitByN2 _ [] = []
splitByN2 n (x:xs) = splitByN n x ++ splitByN2 n xs


-- | @ padGeneric len x xs = ... @
--
-- Pads input list /xs/ with sufficient number of /x/ elements to make new length of /xs/ an integer multiple of /len/.
padGeneric :: Int  -> a -> [a] -> [a]
padGeneric len x xs = xs ++ replicate padsize x
      where padsize = len * (xslength `div` len + 1) - xslength
            xslength = length xs


-- | @ padGenericL len x xs = ... @
--
-- Exactly like 'padGeneric', except prepends instead of appending the padding element /x/.
padGenericL :: Int  -> a -> [a] -> [a]
padGenericL len x xs = replicate padsize x ++ xs
      where padsize = len * (xslength `div` len + 1) - xslength
            xslength = length xs


-- | @ replace x y xs = ... @
--
-- Replaces all instances of /x/ with /y/ in the list /xs/.
replace :: Eq a => a -> a -> [a] -> [a]
replace x y (z:zs)
      | z == x    = y : replace x y zs
      | otherwise = z : replace x y zs
replace _ _ [] = []


-- | @ replaceL xs ys zs = ... @
--
-- Like replace, except replaces all instances of /xs/ with /ys/ in the list /zs/.
replaceL :: Eq a => [a] -> [a] -> [a] -> [a]
replaceL xs ys zs@(z:zt)
      | xs `isPrefixOf` zs = ys ++ replaceL xs ys (drop (length xs) zs)
      | otherwise = z : replaceL xs ys zt
replaceL _ _ [] = []