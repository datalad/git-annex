{- Generating and installing a desktop menu entry file and icon,
 - and a desktop autostart file. (And OSX equivalents.)
 -
 - Copyright 2012 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module Build.DesktopFile where

import Common
import Utility.FreeDesktop
import Config.Files
import Utility.OSX
import Assistant.Install.AutoStart
import Assistant.Install.Menu

import System.Environment
#ifndef mingw32_HOST_OS 
import System.Posix.User
#endif

systemwideInstall :: IO Bool
#ifndef mingw32_HOST_OS 
systemwideInstall = isroot <||> (not <$> userdirset)
  where
	isroot = do
		uid <- fromIntegral <$> getRealUserID
		return $ uid == (0 :: Int)
	userdirset = isJust <$> catchMaybeIO (getEnv "USERDIR")
#else
systemwideInstall = return False
#endif

inDestDir :: OsPath -> IO OsPath
inDestDir f = do
	destdir <- catchDefaultIO "" (getEnv "DESTDIR")
	return $ toOsPath destdir <> literalOsPath "/" <> f

writeFDODesktop :: FilePath -> IO ()
writeFDODesktop command = do
	systemwide <- systemwideInstall

	datadir <- if systemwide then return systemDataDir else userDataDir
	menufile <- inDestDir (desktopMenuFilePath "git-annex" datadir)
	icondir <- inDestDir (iconDir datadir)
	installMenu command menufile (literalOsPath "doc") icondir

	configdir <- if systemwide then return systemConfigDir else userConfigDir
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
			writeFileString programfile command
		)

installUser :: FilePath -> IO ()
installUser command = ifM systemwideInstall
	( return ()
	, install command
	)
