{- Assistant autostart file installation
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Assistant.Install.AutoStart where

import Utility.FreeDesktop
#ifdef darwin_HOST_OS
import Utility.OSX
import Utility.Path
import System.Directory
#endif

installAutoStart :: FilePath -> FilePath -> IO ()
installAutoStart command file = do
#ifdef darwin_HOST_OS
	createDirectoryIfMissing True (parentDir file)
	writeFile file $ genOSXAutoStartFile osxAutoStartLabel command
		["assistant", "--autostart"]
#else
	writeDesktopMenuFile (fdoAutostart command) file
#endif

osxAutoStartLabel :: String
osxAutoStartLabel = "com.branchable.git-annex.assistant"

fdoAutostart :: FilePath -> DesktopEntry
fdoAutostart command = genDesktopEntry
	"Git Annex Assistant"
	"Autostart"
	False
	(command ++ " assistant --autostart")
	Nothing
	[]
