{- Generating and installing a desktop menu entry file and icon,
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
import Config.Files
import Utility.OSX
import Assistant.Install.AutoStart
import Assistant.Install.Menu

import Control.Applicative
import System.Directory
import System.Environment
#ifndef mingw32_HOST_OS
import System.Posix.User
import System.Posix.Files
#endif
import System.FilePath
import Data.Maybe
import System.IO

systemwideInstall :: IO Bool
#ifndef mingw32_HOST_OS 
systemwideInstall = isroot <||> destdirset
  where
	isroot = do
		uid <- fromIntegral <$> getRealUserID
		return $ uid == (0 :: Int)
	destdirset = isJust <$> catchMaybeIO (getEnv "DESTDIR")
#else
systemwideInstall = return False
#endif

inDestDir :: FilePath -> IO FilePath
inDestDir f = do
	destdir <- catchDefaultIO "" (getEnv "DESTDIR")
	return $ destdir ++ "/" ++ f

writeFDODesktop :: FilePath -> IO ()
writeFDODesktop command = do
	systemwide <- systemwideInstall

	datadir <- if systemwide then return systemDataDir else userDataDir
	installMenu command
		=<< inDestDir (desktopMenuFilePath "git-annex" datadir)

	installIcon "doc/logo.svg"
		=<< inDestDir (iconFilePath "git-annex.svg" "scalable" datadir)
	installIcon "doc/favicon.png"
		=<< inDestDir (iconFilePath "git-annex.png" "16x16" datadir)

	configdir <- if systemwide then return systemConfigDir else userConfigDir
	installAutoStart command 
		=<< inDestDir (autoStartPath "git-annex" configdir)

installIcon :: FilePath -> FilePath -> IO ()
installIcon src dest = do
	createDirectoryIfMissing True (parentDir dest)
	withBinaryFile src ReadMode $ \hin ->
		withBinaryFile dest WriteMode $ \hout ->
			hGetContents hin >>= hPutStr hout

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
