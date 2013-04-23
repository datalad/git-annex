{- git-annex extra config files
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Config.Files where

import Common
import Utility.TempFile
import Utility.FreeDesktop

{- ~/.config/git-annex/file -}
userConfigFile :: FilePath -> IO FilePath
userConfigFile file = do
	dir <- userConfigDir
	return $ dir </> "git-annex" </> file

autoStartFile :: IO FilePath
autoStartFile = userConfigFile "autostart"

{- Returns anything listed in the autostart file (which may not exist). -}
readAutoStartFile :: IO [FilePath]
readAutoStartFile = do
	f <- autoStartFile
	nub . lines <$> catchDefaultIO "" (readFile f)

{- Adds a directory to the autostart file. -}
addAutoStartFile :: FilePath -> IO ()
addAutoStartFile path = do
	dirs <- readAutoStartFile
	when (path `notElem` dirs) $ do
		f <- autoStartFile
		createDirectoryIfMissing True (parentDir f)
		viaTmp writeFile f $ unlines $ dirs ++ [path]

{- Removes a directory from the autostart file. -}
removeAutoStartFile :: FilePath -> IO ()
removeAutoStartFile path = do
	dirs <- readAutoStartFile
	when (path `elem` dirs) $ do
		f <- autoStartFile
		createDirectoryIfMissing True (parentDir f)
		viaTmp writeFile f $ unlines $
			filter (not . equalFilePath path) dirs

{- The path to git-annex is written here; which is useful when cabal
 - has installed it to some aweful non-PATH location. -}
programFile :: IO FilePath
programFile = userConfigFile "program"

{- Returns a command to run for git-annex. -}
readProgramFile :: IO FilePath
readProgramFile = do
	programfile <- programFile
	catchDefaultIO "git-annex" $ readFile programfile
