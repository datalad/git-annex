{- git-annex command
 -
 - Copyright 2010,2012 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.DropUnused where

import Common.Annex
import Command
import qualified Command.Drop
import qualified Remote
import qualified Git
import Command.Unused (withUnusedMaps, UnusedMaps(..), startUnused)
import Annex.NumCopies
import Annex.Content

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
	from <- maybe (pure Nothing) (Just <$$> getParsed) (dropFrom o)
	withUnusedMaps (start from numcopies) (rangesToDrop o)

start :: Maybe Remote -> NumCopies -> UnusedMaps -> Int -> CommandStart
start from numcopies = startUnused "dropunused"
	(perform from numcopies)
	(performOther gitAnnexBadLocation)
	(performOther gitAnnexTmpObjectLocation)

perform :: Maybe Remote -> NumCopies -> Key -> CommandPerform
perform from numcopies key = case from of
	Just r -> do
		showAction $ "from " ++ Remote.name r
		Command.Drop.performRemote key Nothing numcopies r
	Nothing -> ifM (inAnnex key)
		( Command.Drop.performLocal key Nothing numcopies []
		, next (return True)
		)

performOther :: (Key -> Git.Repo -> FilePath) -> Key -> CommandPerform
performOther filespec key = do
	f <- fromRepo $ filespec key
	liftIO $ nukeFile f
	next $ return True
