{- git-annex user config files
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Locations.UserConfig where

import Utility.FreeDesktop

import System.FilePath

{- ~/.config/git-annex/file -}
userConfigFile :: FilePath -> IO FilePath
userConfigFile file = do
	dir <- userConfigDir
	return $ dir </> "git-annex" </> file

autoStartFile :: IO FilePath
autoStartFile = userConfigFile "autostart"

{- The path to git-annex is written here; which is useful when cabal
 - has installed it to some aweful non-PATH location. -}
programFile :: IO FilePath
programFile = userConfigFile "program"
