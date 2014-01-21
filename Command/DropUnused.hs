{- git-annex command
 -
 - Copyright 2010,2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.DropUnused where

import Common.Annex
import Command
import qualified Annex
import qualified Command.Drop
import qualified Remote
import qualified Git
import qualified Option
import Command.Unused (withUnusedMaps, UnusedMaps(..), startUnused)
import Config.NumCopies

def :: [Command]
def = [withOptions [Command.Drop.fromOption] $
	command "dropunused" (paramRepeating paramNumRange)
		seek SectionMaintenance "drop unused file content"]

seek :: CommandSeek
seek ps = do
	numcopies <- getNumCopies
	withUnusedMaps (start numcopies) ps

start :: NumCopies -> UnusedMaps -> Int -> CommandStart
start numcopies = startUnused "dropunused" (perform numcopies) (performOther gitAnnexBadLocation) (performOther gitAnnexTmpLocation)

perform :: NumCopies -> Key -> CommandPerform
perform numcopies key = maybe droplocal dropremote =<< Remote.byNameWithUUID =<< from
  where
	dropremote r = do
		showAction $ "from " ++ Remote.name r
		Command.Drop.performRemote key numcopies r
	droplocal = Command.Drop.performLocal key numcopies Nothing
	from = Annex.getField $ Option.name Command.Drop.fromOption

performOther :: (Key -> Git.Repo -> FilePath) -> Key -> CommandPerform
performOther filespec key = do
	f <- fromRepo $ filespec key
	liftIO $ nukeFile f
	next $ return True
