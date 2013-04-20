{- Generating and installing a desktop menu entry file
 - and a desktop autostart file. (And OSX equivilants.)
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Build.DesktopFile where

import Utility.Exception
import Utility.FreeDesktop
import Utility.Path
import Utility.Monad
import Locations.UserConfig
import Utility.OSX
import Assistant.Install.AutoStart
import Assistant.Install.Menu

import Control.Applicative
import System.Directory
import System.Environment
import System.Posix.User
import System.Posix.Files
import System.FilePath
import Data.Maybe

systemwideInstall :: IO Bool
systemwideInstall = isroot <||> destdirset
  where
	isroot = do
		uid <- fromIntegral <$> getRealUserID
		return $ uid == (0 :: Int)
	destdirset = isJust <$> catchMaybeIO (getEnv "DESTDIR")

inDestDir :: FilePath -> IO FilePath
inDestDir f = do
	destdir <- catchDefaultIO "" (getEnv "DESTDIR")
	return $ destdir ++ "/" ++ f

writeFDODesktop :: FilePath -> IO ()
writeFDODesktop command = do
	datadir <- ifM systemwideInstall ( return systemDataDir, userDataDir )
	installMenu command
		=<< inDestDir (desktopMenuFilePath "git-annex" datadir)

	configdir <- ifM systemwideInstall ( return systemConfigDir, userConfigDir )
	installAutoStart command 
		=<< inDestDir (autoStartPath "git-annex" configdir)

writeOSXDesktop :: FilePath -> IO ()
writeOSXDesktop command = do
	installAutoStart command =<< inDestDir =<< ifM systemwideInstall
		( return $ systemAutoStart osxAutoStartLabel
		, userAutoStart osxAutoStartLabel
		)

install :: FilePath -> IO ()
install command = do
#ifdef darwin_HOST_OS
	writeOSXDesktop command
#else
	writeFDODesktop command
#endif
	ifM systemwideInstall
		( return ()
		, do
			programfile <- inDestDir =<< programFile
			createDirectoryIfMissing True (parentDir programfile)
			writeFile programfile command
		)
