-- | Cryptographic and hash functions that are required for extracting and  producing encoded information needed when
-- working with specific BNCS messages.
module Util.Crypto (
      -- * Encoding and Decoding
        decode
      -- * Hashing
      , hash
) where

import Data.Bits ((.&.), (.|.), shiftL, shiftR, rotateL, xor, complement)
import Data.Char (toLower)
import Data.Word (Word32)
import Util.String (pad)


-- | Decode the game statstring found in GetAdvListEx messages into ASCII readable form.
decode :: String -> String
decode gdata = map toEnum $ decode' (map fromEnum gdata) 0 0 0 0 []


-- | Main decoding loop for 'decode'.
decode' :: [Int] -> Int -> Int -> Int -> Int -> [Int] -> [Int]
decode' [] _ _ _ _ result = reverse result
decode' (c:cs) idx i j d result
      | mod i 8 > 0 = decode' cs (idx+1) (i+1) (j+1) d (nc:result)
      | otherwise = decode' cs idx (i+1) 0 c result
      where nc = c .&. (shiftR d (j+1) .|. (complement 1))


-- | Broken (X)-SHA1 algorithm for password hashing. Starcraft and Diablo2 compatibility required for PvPGN logon
-- under Warcraft III.
hash :: String -> String
hash pass = (endianBSHA1 . binafyBSHA1 . hashBSHA1 initBSHA1 . padBSHA1 . map toLower) pass


-- | Type synonym for simplification.
type ABCDE = (Word32, Word32, Word32, Word32, Word32)


-- | Princess Padme.
padBSHA1 :: String -> String
padBSHA1 s = pad 64 s


-- | Hashing constants.
initBSHA1 :: ABCDE
initBSHA1 = (0x67452301, 0xEFCDAB89, 0x98BADCFE, 0x10325476, 0xC3D2E1F0)


-- | Entry to hashing function.
hashBSHA1 :: ABCDE -> String -> [Word32]
hashBSHA1 (a, b, c, d, e) [] = [a, b, c, d, e]
hashBSHA1 (a, b, c, d, e) om = hashBSHA1 (a+na, b+nb, c+nc, d+nd, e+ne) ms
      where (m,ms) = splitAt 64 om
            em = (extBSHA1 16 . toWord32) m
            (na, nb, nc, nd, ne) = mainBSHA1 0 (a, b, c, d, e) em


-- | Split ASCII password into 32-bit words.
toWord32 :: String -> [Word32]
toWord32 "" = []
toWord32 s = word32 : toWord32 ws
      where (w,ws) = splitAt 4 s
            word32 = sum (zipWith shiftL (map (fromIntegral . fromEnum) w) [0, 8, 16, 24])


-- | Something, I forgot. Check wikipedia about SHA1.
extBSHA1 :: Int -> [Word32] -> [Word32]
extBSHA1 80 m = m
extBSHA1 i m = extBSHA1 (i+1) nm
      where nm = m ++ [mm]
            mm = rotateL 1 $ fromIntegral $ (m!!(i-3)) `xor` (m!!(i-8)) `xor` (m!!(i-14)) `xor` (m!!(i-16))


-- | Main loop of hashing algorithm.
mainBSHA1 :: Int -> ABCDE -> [Word32] -> ABCDE
mainBSHA1 i (a, b, c, d, e) m
      | i < 20 = mainBSHA1 (i+1) (mash f1) m
      | i < 40 = mainBSHA1 (i+1) (mash f2) m
      | i < 60 = mainBSHA1 (i+1) (mash f3) m
      | i < 80 = mainBSHA1 (i+1) (mash f4) m
      | otherwise = (a, b, c, d, e)
      where mash f = (temp f, a, rotateL b 30, c, d)
            temp f = (rotateL a 5) + f + e + (m!!i)
            f1 = ((b .&. c) .|. ((complement b) .&. d)) + 0x5A827999
            f2 = (b `xor` c `xor` d) + 0x6ED9EBA1
            f3 = ((b .&. c) .|. (b .&. d) .|. (c .&. d)) - 0x70E44324
            f4 = (b `xor` c `xor` d) - 0x359D3E2A


-- | Convert hash to ASCII Binary.
binafyBSHA1 :: [Word32] -> String
binafyBSHA1 d = foldr (\x y -> toBin x ++ y) "" d


-- | Convert 32-bit word to ASCII Binary.
toBin :: Word32 -> String
toBin x0 = zipWith getc [y8, y6, y4, y2] [y7, y5, y3, y1]
      where (x1, y1) = divMod x0 16
            (x2, y2) = divMod x1 16
            (x3, y3) = divMod x2 16
            (x4, y4) = divMod x3 16
            (x5, y5) = divMod x4 16
            (x6, y6) = divMod x5 16
            (y8, y7) = divMod x6 16
            getc n1 n0 = toEnum (16*(fromIntegral n1) + (fromIntegral n0))


-- \| Convert hash to ASCII Hex.
-- hexifyBSHA1 :: [Word32] -> String
-- hexifyBSHA1 d = foldr (\x y -> toHex x ++ y) "" d


-- \| Convert 32-bit word to ASCII Hex.
-- toHex :: Word32 -> String
-- toHex x0 = map getc [y8,y7,y6,y5,y4,y3,y2,y1]
--       where (x1, y1) = divMod x0 16
--             (x2, y2) = divMod x1 16
--             (x3, y3) = divMod x2 16
--             (x4, y4) = divMod x3 16
--             (x5, y5) = divMod x4 16
--             (x6, y6) = divMod x5 16
--             (y8, y7) = divMod x6 16
--             getc n = intToDigit (fromIntegral n)


-- | Big endian over network socket.
endianBSHA1 :: String -> String
endianBSHA1 [] = []
endianBSHA1 hsh = reverse h ++ endianBSHA1 hs
      where (h,hs) = splitAt 4 hsh