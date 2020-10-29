{- Assistant menu installation.
 -
 - Copyright 2013 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module Assistant.Install.Menu where

import Utility.FreeDesktop
import Utility.FileSystemEncoding
import Utility.Path

import System.IO
import Utility.SystemDirectory
import System.FilePath

installMenu :: FilePath -> FilePath -> FilePath -> FilePath -> IO ()
#ifdef darwin_HOST_OS
installMenu _command _menufile _iconsrcdir _icondir = return ()
#else
installMenu command menufile iconsrcdir icondir = do
	writeDesktopMenuFile (fdoDesktopMenu command) menufile
	installIcon (iconsrcdir </> "logo.svg") $
		iconFilePath (iconBaseName ++ ".svg") "scalable" icondir
	installIcon (iconsrcdir </> "logo_16x16.png") $
		iconFilePath (iconBaseName ++ ".png") "16x16" icondir
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

installIcon :: FilePath -> FilePath -> IO ()
installIcon src dest = do
	createDirectoryIfMissing True (fromRawFilePath (parentDir (toRawFilePath dest)))
	withBinaryFile src ReadMode $ \hin ->
		withBinaryFile dest WriteMode $ \hout ->
			hGetContents hin >>= hPutStr hout

iconBaseName :: String
iconBaseName = "git-annex"
