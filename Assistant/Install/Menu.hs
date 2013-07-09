{- Assistant menu installation.
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Assistant.Install.Menu where

import Utility.FreeDesktop

installMenu :: FilePath -> FilePath -> IO ()
installMenu command file =
#ifdef darwin_HOST_OS
	return ()
#else
	writeDesktopMenuFile (fdoDesktopMenu command) file
#endif

{- The command can be either just "git-annex", or the full path to use
 - to run it. -}
fdoDesktopMenu :: FilePath -> DesktopEntry
fdoDesktopMenu command = genDesktopEntry
	"Git Annex"
	"Track and sync the files in your Git Annex"
	False
	(command ++ " webapp")
	(Just "git-annex") -- icon base name
	["Network", "FileTransfer"]
