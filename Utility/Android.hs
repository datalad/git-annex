{-# LANGUAGE CPP #-}

{- Android stuff
 -
 - Copyright 2018 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

module Utility.Android (
	osAndroid
) where

#ifdef linux_HOST_OS
import Common
#endif

-- Detect when the Linux build is running on Android, eg in termux.
--
-- Note that this relies on termux's uname having been built with "Android"
-- as the os name. Often on Android, uname will report "Linux".
osAndroid :: IO Bool
#ifdef linux_HOST_OS
osAndroid = catchDefaultIO False $
	("Android" `isPrefixOf` ) <$> readProcess "uname" ["-o"]
#else
osAndroid = return False
#endif
