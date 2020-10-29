{- git-annex autostart file
 -
 - Copyright 2012-2019 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# OPTIONS_GHC -fno-warn-tabs #-}

module Config.Files.AutoStart where

import Common
import Config.Files
import Utility.Tmp

{- Returns anything listed in the autostart file (which may not exist). -}
readAutoStartFile :: IO [FilePath]
readAutoStartFile = do
	f <- autoStartFile
	filter valid . nub . map dropTrailingPathSeparator . lines
		<$> catchDefaultIO "" (readFile f)
  where
	-- Ignore any relative paths; some old buggy versions added eg "."
	valid = isAbsolute

modifyAutoStartFile :: ([FilePath] -> [FilePath]) -> IO ()
modifyAutoStartFile func = do
	dirs <- readAutoStartFile
	let dirs' = nubBy equalFilePath $ func dirs
	when (dirs' /= dirs) $ do
		f <- autoStartFile
		createDirectoryIfMissing True $
			fromRawFilePath (parentDir (toRawFilePath f))
		viaTmp writeFile f $ unlines dirs'

{- Adds a directory to the autostart file. If the directory is already
 - present, it's moved to the top, so it will be used as the default
 - when opening the webapp. -}
addAutoStartFile :: FilePath -> IO ()
addAutoStartFile path = do
	path' <- fromRawFilePath <$> absPath (toRawFilePath path)
	modifyAutoStartFile $ (:) path'

{- Removes a directory from the autostart file. -}
removeAutoStartFile :: FilePath -> IO ()
removeAutoStartFile path = do
	path' <- fromRawFilePath <$> absPath (toRawFilePath path)
	modifyAutoStartFile $
		filter (not . equalFilePath path')
