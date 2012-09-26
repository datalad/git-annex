{- Assistant installation
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.Install where

import Locations.UserConfig
import Utility.OSX
import Utility.Path

import System.Posix.Env
import System.Directory

{- The OSX git-annex.app does not have an installation process.
 - So when it's run, it needs to set up autostarting of the assistant
 - daemon, as well as writing the programFile.
 -
 - Note that this is done every time it's started, so if the user moves
 - it around, the paths this sets up won't break.
 -}
ensureInstalled :: IO ()
ensureInstalled = do
	e <- getEnv "OSX_GIT_ANNEX_APP_PROGRAM"
	case e of
		Nothing -> return ()
		Just program -> do
			programfile <- programFile
			createDirectoryIfMissing True (parentDir programfile)
			writeFile programfile program

			autostartfile <- userAutoStart autoStartLabel
			installAutoStart program autostartfile

{- Installs an autostart plist file for OSX. -}
installAutoStart :: FilePath -> FilePath -> IO ()
installAutoStart command file = do
	createDirectoryIfMissing True (parentDir file)
	writeFile file $ genOSXAutoStartFile autoStartLabel command
		["assistant", "--autostart"]

autoStartLabel :: String
autoStartLabel = "com.branchable.git-annex.assistant"
