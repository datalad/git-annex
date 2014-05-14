{- /bin/sh handling
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE CPP #-}

module Utility.Shell where

shellPath_portable :: FilePath
shellPath_portable = "/bin/sh"

shellPath_local :: FilePath
#ifndef __ANDROID__
shellPath_local = shellPath_portable
#else
shellPath_local = "/system/bin/sh"
#endif

shebang_portable :: String
shebang_portable = "#!" ++ shellPath_portable

shebang_local :: String
shebang_local = "#!" ++ shellPath_local
