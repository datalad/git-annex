{- Android stuff
 -
 - Copyright 2018 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

module Utility.Android where

import Common

-- Detect when the Linux build is running on Android, eg in termux.
--
-- Note that this relies on termux's uname having been built with "Android"
-- as the os name. Often on Android, uname will report "Linux".
osAndroid :: IO Bool
osAndroid = ("Android" `isPrefixOf` ) <$> readProcess "uname" ["-o"]
