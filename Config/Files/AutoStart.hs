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
readAutoStartFile :: IO [OsPath]
readAutoStartFile = do
	f <- autoStartFile
	filter valid . nub . map (dropTrailingPathSeparator . toOsPath) . lines
		<$> catchDefaultIO "" (readFile (fromOsPath f))
  where
	-- Ignore any relative paths; some old buggy versions added eg "."
	valid = isAbsolute

modifyAutoStartFile :: ([OsPath] -> [OsPath]) -> IO ()
modifyAutoStartFile func = do
	dirs <- readAutoStartFile
	let dirs' = nubBy equalFilePath $ func dirs
	when (dirs' /= dirs) $ do
		f <- autoStartFile
		createDirectoryIfMissing True (parentDir f)
		viaTmp (writeFile . fromRawFilePath . fromOsPath) f
			(unlines (map fromOsPath dirs'))

{- Adds a directory to the autostart file. If the directory is already
 - present, it's moved to the top, so it will be used as the default
 - when opening the webapp. -}
addAutoStartFile :: OsPath -> IO ()
addAutoStartFile path = do
	path' <- absPath path
	modifyAutoStartFile $ (:) path'

{- Removes a directory from the autostart file. -}
removeAutoStartFile :: OsPath -> IO ()
removeAutoStartFile path = do
	path' <- absPath path
	modifyAutoStartFile $
		filter (not . equalFilePath path')
