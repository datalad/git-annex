{- git-annex command
 -
 - Copyright 2010, 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Get where

import Common.Annex
import Command
import qualified Remote
import Annex.Content
import Logs.Transfer
import Logs.NumCopies
import Annex.Wanted
import GitAnnex.Options
import qualified Command.Move
import Types.Key

def :: [Command]
def = [withOptions getOptions $ command "get" paramPaths seek
	SectionCommon "make content of annexed files available"]

getOptions :: [Option]
getOptions = fromOption : keyOptions

seek :: CommandSeek
seek ps = do
	from <- getOptionField fromOption Remote.byNameWithUUID
	withKeyOptions
		(startKeys from)
		(withFilesInGit $ whenAnnexed $ start from)
		ps

start :: Maybe Remote -> FilePath -> (Key, Backend) -> CommandStart
start from file (key, _) = start' expensivecheck from key (Just file)
  where
	expensivecheck = checkAuto (numCopiesCheck file key (<) <||> wantGet False (Just file))

startKeys :: Maybe Remote -> Key -> CommandStart
startKeys from key = start' (return True) from key Nothing

start' :: Annex Bool -> Maybe Remote -> Key -> AssociatedFile -> CommandStart
start' expensivecheck from key afile = stopUnless (not <$> inAnnex key) $
	stopUnless expensivecheck $
		case from of
			Nothing -> go $ perform key afile
			Just src ->
				stopUnless (Command.Move.fromOk src key) $
					go $ Command.Move.fromPerform src False key afile
  where
  	go a = do
		showStart "get" (fromMaybe (key2file key) afile)
		next a

perform :: Key -> AssociatedFile -> CommandPerform
perform key afile = stopUnless (getViaTmp key $ getKeyFile key afile) $
	next $ return True -- no cleanup needed

{- Try to find a copy of the file in one of the remotes,
 - and copy it to here. -}
getKeyFile :: Key -> AssociatedFile -> FilePath -> Annex Bool
getKeyFile key afile dest = getKeyFile' key afile dest
	=<< Remote.keyPossibilities key

getKeyFile' :: Key -> AssociatedFile -> FilePath -> [Remote] -> Annex Bool
getKeyFile' key afile dest = dispatch
  where
	dispatch [] = do
		showNote "not available"
		showlocs
		return False
	dispatch remotes = trycopy remotes remotes
	trycopy full [] = do
		Remote.showTriedRemotes full
		showlocs
		return False
	trycopy full (r:rs) =
		ifM (probablyPresent r)
			( docopy r (trycopy full rs)
			, trycopy full rs
			)
	showlocs = Remote.showLocations key []
		"No other repository is known to contain the file."
	-- This check is to avoid an ugly message if a remote is a
	-- drive that is not mounted.
	probablyPresent r
		| Remote.hasKeyCheap r =
			either (const False) id <$> Remote.hasKey r key
		| otherwise = return True
	docopy r continue = do
		ok <- download (Remote.uuid r) key afile noRetry $ \p -> do
			showAction $ "from " ++ Remote.name r
			Remote.retrieveKeyFile r key afile dest p
		if ok then return ok else continue
