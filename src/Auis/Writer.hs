-- | ?
module Auis.Writer (
        errorMsg
      , internalMsg
      , specialMsg
      , exceptionMsg
) where

import Auis.Writer.Types


errorMsg :: String -> Auis
errorMsg = PrintError


internalMsg :: String -> Auis
internalMsg = PrintInternal


specialMsg :: String -> Auis
specialMsg = PrintSpecial


exceptionMsg :: Show e => e -> Auis
exceptionMsg = PrintError . (:) '\t' . show