{- Assistant autostart file installation
 -
 - Copyright 2012 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module Assistant.Install.AutoStart where

import Common
import Utility.FreeDesktop
#ifdef darwin_HOST_OS
import Utility.OSX
import Utility.Path
import Utility.SystemDirectory
import Utility.FileSystemEncoding
#endif

installAutoStart :: String -> OsPath -> IO ()
installAutoStart command file = do
#ifdef darwin_HOST_OS
	createDirectoryIfMissing True (parentDir file)
	writeFile (fromOsPath file) $ genOSXAutoStartFile osxAutoStartLabel command
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
