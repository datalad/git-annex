{- git-annex command
 -
 - Copyright 2010,2012,2018 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Command.DropUnused where

import Command
import qualified Annex
import qualified Command.Drop
import qualified Remote
import qualified Git
import Command.Unused (withUnusedMaps, UnusedMaps(..), startUnused)
import Annex.NumCopies
import Annex.Content
import qualified Utility.RawFilePath as R

cmd :: Command
cmd = command "dropunused" SectionMaintenance
	"drop unused file content"
	(paramRepeating paramNumRange) (seek <$$> optParser)

data DropUnusedOptions = DropUnusedOptions
	{ rangesToDrop :: CmdParams
	, dropFrom :: Maybe (DeferredParse Remote)
	}

optParser :: CmdParamsDesc -> Parser DropUnusedOptions
optParser desc = DropUnusedOptions
	<$> cmdParams desc
	<*> optional (Command.Drop.parseDropFromOption)

seek :: DropUnusedOptions -> CommandSeek
seek o = do
	numcopies <- getNumCopies
	mincopies <- getMinCopies
	from <- maybe (pure Nothing) (Just <$$> getParsed) (dropFrom o)
	withUnusedMaps (start from numcopies mincopies) (rangesToDrop o)

start :: Maybe Remote -> NumCopies -> MinCopies -> UnusedMaps -> Int -> CommandStart
start from numcopies mincopies = startUnused "dropunused"
	(perform from numcopies mincopies)
	(performOther gitAnnexBadLocation)
	(performOther gitAnnexTmpObjectLocation)

perform :: Maybe Remote -> NumCopies -> MinCopies -> Key -> CommandPerform
perform from numcopies mincopies key = case from of
	Just r -> do
		showAction $ "from " ++ Remote.name r
		Command.Drop.performRemote key (AssociatedFile Nothing) numcopies mincopies r
	Nothing -> ifM (inAnnex key)
		( droplocal
		, ifM (objectFileExists key)
			( ifM (Annex.getState Annex.force)
				( droplocal
				, do
					warning "Annexed object has been modified and dropping it would probably lose the only copy. Run this command with --force if you want to drop it anyway."
					next $ return False
				)
			, next $ return True
			)
		)
  where
	droplocal = Command.Drop.performLocal key (AssociatedFile Nothing) numcopies mincopies []

performOther :: (Key -> Git.Repo -> RawFilePath) -> Key -> CommandPerform
performOther filespec key = do
	f <- fromRepo $ filespec key
	pruneTmpWorkDirBefore f (liftIO . removeWhenExistsWith R.removeLink)
	next $ return True
