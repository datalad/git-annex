{- Generating and installing a desktop menu entry file
 - and a desktop autostart file.
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Build.InstallDesktopFile where

import Utility.Exception
import Utility.FreeDesktop

import Control.Applicative
import System.Environment
import System.Posix.User

{- The command can be either just "git-annex", or the full path to use
 - to run it. -}
desktop :: FilePath -> DesktopEntry
desktop command = genDesktopEntry
	"Git Annex"
	"Track and sync the files in your Git Annex"
	False
	(command ++ " webapp")
	["Network", "FileTransfer"]

autostart :: FilePath -> DesktopEntry
autostart command = genDesktopEntry
	"Git Annex Assistant"
	"Autostart"
	False
	(command ++ " assistant --autostart")
	[]

writeDesktop :: String -> IO ()
writeDesktop command = do
	destdir <- catchDefaultIO (getEnv "DESTDIR") ""
	uid <- fromIntegral <$> getRealUserID

	datadir <- if uid /= 0 then userDataDir else return systemDataDir
	writeDesktopMenuFile (desktop command) $
		desktopMenuFilePath "git-annex" datadir

	configdir <- if uid /= 0 then userConfigDir else return systemConfigDir
	writeDesktopMenuFile (autostart command) $
		autoStartPath "git-annex" configdir

	when (uid /= 0) $ do
		programfile <- programFile
		createDirectoryIfMissing True (parentDir programFile)
		writeFile programfile command

main = getArgs >>= go
	where
		go [] = error "specify git-annex command"
		go (command:_) = writeDesktop command
