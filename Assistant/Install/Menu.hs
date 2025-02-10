{- Assistant menu installation.
 -
 - Copyright 2013 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module Assistant.Install.Menu where

import Common
import Utility.FreeDesktop

installMenu :: String -> OsPath -> OsPath -> OsPath -> IO ()
#ifdef darwin_HOST_OS
installMenu _command _menufile _iconsrcdir _icondir = return ()
#else
installMenu command menufile iconsrcdir icondir = do
	writeDesktopMenuFile (fdoDesktopMenu command) menufile
	installIcon (iconsrcdir </> literalOsPath "logo.svg") $
		iconFilePath (toOsPath (iconBaseName ++ ".svg")) "scalable" icondir
	installIcon (iconsrcdir </> literalOsPath "logo_16x16.png") $
		iconFilePath (toOsPath (iconBaseName ++ ".png")) "16x16" icondir
#endif

{- The command can be either just "git-annex", or the full path to use
 - to run it. -}
fdoDesktopMenu :: FilePath -> DesktopEntry
fdoDesktopMenu command = genDesktopEntry
	"Git Annex"
	"Track and sync the files in your Git Annex"
	False
	(command ++ " webapp")
	(Just iconBaseName)
	["Network", "FileTransfer"]

installIcon :: OsPath -> OsPath -> IO ()
installIcon src dest = do
	createDirectoryIfMissing True (parentDir dest)
	withBinaryFile (fromOsPath src) ReadMode $ \hin ->
		withBinaryFile (fromOsPath dest) WriteMode $ \hout ->
			hGetContents hin >>= hPutStr hout

iconBaseName :: String
iconBaseName = "git-annex"
