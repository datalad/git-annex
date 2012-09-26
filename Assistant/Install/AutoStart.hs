{- Assistant OSX autostart file installation
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.Install.AutoStart where

import Utility.OSX
import Utility.Path

import System.Directory

{- Installs an autostart plist file for OSX. -}
installAutoStart :: FilePath -> FilePath -> IO ()
installAutoStart command file = do
	createDirectoryIfMissing True (parentDir file)
	writeFile file $ genOSXAutoStartFile autoStartLabel command
		["assistant", "--autostart"]

autoStartLabel :: String
autoStartLabel = "com.branchable.git-annex.assistant"
