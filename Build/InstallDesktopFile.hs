{- Generating and installing a desktop menu entry file
 - and a desktop autostart file. (And OSX equivilants.)
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Build.InstallDesktopFile where

import Utility.Exception
import Utility.FreeDesktop
import Utility.Path
import Utility.Monad
import Locations.UserConfig

import Control.Applicative
import Control.Monad
import System.Directory
import System.Environment
import System.Posix.User
import System.FilePath

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

isRoot :: IO Bool
isRoot = do
	uid <- fromIntegral <$> getRealUserID
	return $ uid == 0

inDestDir :: FilePath -> IO FilePath
inDestDir f = do
	destdir <- catchDefaultIO (getEnv "DESTDIR") ""
	return $ destdir </> f

writeDesktop :: FilePath -> IO ()
writeDesktop command = do
	datadir <- ifM isRoot ( return systemDataDir, userDataDir )
	writeDesktopMenuFile (desktop command) 
		=<< inDestDir (desktopMenuFilePath "git-annex" datadir)

	configdir <- ifM isRoot ( return systemConfigDir, userConfigDir )
	writeDesktopMenuFile (autostart command) 
		=<< inDestDir (autoStartPath "git-annex" configdir)

	ifM isRoot
		( return ()
		, do
			programfile <- inDestDir =<< programFile
			createDirectoryIfMissing True (parentDir programfile)
			writeFile programfile command
		)

main = getArgs >>= go
	where
		go [] = error "specify git-annex command"
		go (command:_) = writeDesktop command
