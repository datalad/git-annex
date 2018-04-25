{- Android stuff
 -
 - Copyright 2018 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

module Utility.Android where

import Common

-- Detect when the Linux build is running on Android, eg in termux.
osAndroid :: IO Bool
osAndroid = ("Android" == ) <$> readProcess "uname" ["-o"]
