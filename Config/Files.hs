{- git-annex extra config files
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Config.Files where

import Common
import Utility.Tmp
import Utility.FreeDesktop
import System.Environment

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
	nub . map dropTrailingPathSeparator . lines
		<$> catchDefaultIO "" (readFile f)

modifyAutoStartFile :: ([FilePath] -> [FilePath]) -> IO ()
modifyAutoStartFile func = do
	dirs <- readAutoStartFile
	let dirs' = nubBy equalFilePath $ func dirs
	when (dirs' /= dirs) $ do
		f <- autoStartFile
		createDirectoryIfMissing True (parentDir f)
		viaTmp writeFile f $ unlines dirs'

{- Adds a directory to the autostart file. If the directory is already
 - present, it's moved to the top, so it will be used as the default
 - when opening the webapp. -}
addAutoStartFile :: FilePath -> IO ()
addAutoStartFile path = modifyAutoStartFile $ (:) path

{- Removes a directory from the autostart file. -}
removeAutoStartFile :: FilePath -> IO ()
removeAutoStartFile path = modifyAutoStartFile $
	filter (not . equalFilePath path)

{- The path to git-annex is written here; which is useful when cabal
 - has installed it to some awful non-PATH location. -}
programFile :: IO FilePath
programFile = userConfigFile "program"

{- Returns a command to run for git-annex. -}
readProgramFile :: IO FilePath
readProgramFile = do
	programfile <- programFile
	p <- catchDefaultIO cmd $ 
		fromMaybe cmd . headMaybe . lines <$> readFile programfile
	ifM (inPath p)
		( return p
		, ifM (inPath cmd)
			( return cmd
			, error $ "cannot find git-annex program in PATH or in the location listed in " ++ programfile
			)
		)
  where
  	cmd = "git-annex"

{- A fully qualified path to the currently running git-annex program.
 - 
 - getExecutablePath is available since ghc 7.4.2. On OSs it supports
 - well, it returns the complete path to the program. But, on other OSs,
 - it might return just the basename.
 -}
programPath :: IO (Maybe FilePath)
programPath = do
#if MIN_VERSION_base(4,6,0)
	exe <- getExecutablePath
	p <- if isAbsolute exe
		then return exe
		else readProgramFile
#else
	p <- readProgramFile
#endif
	-- In case readProgramFile returned just the command name,
	-- fall back to finding it in PATH.
	searchPath p
