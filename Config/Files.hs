{- git-annex extra config files
 -
 - Copyright 2012-2019 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# OPTIONS_GHC -fno-warn-tabs #-}

module Config.Files where

import Common
import Utility.FreeDesktop
import Utility.Exception

{- ~/.config/git-annex/file -}
userConfigFile :: OsPath -> IO OsPath
userConfigFile file = do
	dir <- toOsPath <$> userConfigDir
	return $ dir </> literalOsPath "git-annex" </> file

autoStartFile :: IO OsPath
autoStartFile = userConfigFile (literalOsPath "autostart")

{- The path to git-annex is written here; which is useful when something
 - has installed it to some awful non-PATH location. -}
programFile :: IO OsPath
programFile = userConfigFile (literalOsPath "program")

{- A .noannex file in a git repository prevents git-annex from
 - initializing that repository. The content of the file is returned. -}
noAnnexFileContent :: Maybe OsPath -> IO (Maybe String)
noAnnexFileContent repoworktree = case repoworktree of
	Nothing -> return Nothing
	Just wt -> catchMaybeIO (readFile (fromOsPath (wt </> literalOsPath ".noannex")))
