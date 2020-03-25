{-# LANGUAGE CPP #-}
#include "About.h"
-- | 'Anomy' information, including version number. Statically modified during compile time.
module Core.About (
        version
) where


version :: String
version = VERSION
