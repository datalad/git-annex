{- Generating and installing a desktop menu entry file.
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Build.Desktop where

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

writeDesktop :: DesktopEntry -> IO ()
writeDesktop d = do
	destdir <- catchDefaultIO (getEnv "DESTDIR") ""
	uid <- fromIntegral <$> getRealUserID
	dest <- if uid /= 0
		then userDesktopMenuFilePath "git-annex"
		else return $ systemDesktopMenuFilePath "git-annex"
	writeDesktopMenuFile d dest
