{- /bin/sh handling
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Utility.Shell where

shellPath :: FilePath
#ifndef __ANDROID__
shellPath = "/bin/sh"
#else
shellPath = "/system/bin/sh"
#endif

shebang :: String
shebang = "#!" ++ shellPath
