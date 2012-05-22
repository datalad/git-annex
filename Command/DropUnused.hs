{- git-annex command
 -
 - Copyright 2010,2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.DropUnused where

import Logs.Unused
import Common.Annex
import Command
import qualified Annex
import qualified Command.Drop
import qualified Remote
import qualified Git
import qualified Option

def :: [Command]
def = [withOptions [Command.Drop.fromOption] $
	command "dropunused" (paramRepeating paramNumRange)
		seek "drop unused file content"]

seek :: [CommandSeek]
seek = [withUnusedMaps start]

start :: UnusedMaps -> Int -> CommandStart
start = startUnused "dropunused" perform (performOther gitAnnexBadLocation) (performOther gitAnnexTmpLocation)

perform :: Key -> CommandPerform
perform key = maybe droplocal dropremote =<< Remote.byName =<< from
	where
		dropremote r = do
			showAction $ "from " ++ Remote.name r
			ok <- Remote.removeKey r key
			next $ Command.Drop.cleanupRemote key r ok
		droplocal = Command.Drop.performLocal key (Just 0) -- force drop
		from = Annex.getField $ Option.name Command.Drop.fromOption

performOther :: (Key -> Git.Repo -> FilePath) -> Key -> CommandPerform
performOther filespec key = do
	f <- fromRepo $ filespec key
	liftIO $ whenM (doesFileExist f) $ removeFile f
	next $ return True
