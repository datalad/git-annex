{- git-annex command
 -
 - Copyright 2010, 2013 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Get where

import Command
import qualified Remote
import Annex.Content
import Annex.Transfer
import Annex.NumCopies
import Annex.Wanted
import qualified Command.Move

cmd :: Command
cmd = withGlobalOptions (jobsOption : annexedMatchingOptions) $ 
	command "get" SectionCommon 
		"make content of annexed files available"
		paramPaths (seek <$$> optParser)

data GetOptions = GetOptions
	{ getFiles :: CmdParams
	, getFrom :: Maybe (DeferredParse Remote)
	, autoMode :: Bool
	, keyOptions :: Maybe KeyOptions
	}

optParser :: CmdParamsDesc -> Parser GetOptions
optParser desc = GetOptions
	<$> cmdParams desc
	<*> optional parseFromOption
	<*> parseAutoOption
	<*> optional (parseKeyOptions True)

seek :: GetOptions -> CommandSeek
seek o = allowConcurrentOutput $ do
	from <- maybe (pure Nothing) (Just <$$> getParsed) (getFrom o)
	withKeyOptions (keyOptions o) (autoMode o)
		(startKeys from)
		(withFilesInGit $ whenAnnexed $ start o from)
		(getFiles o)

start :: GetOptions -> Maybe Remote -> FilePath -> Key -> CommandStart
start o from file key = start' expensivecheck from key (Just file)
  where
	expensivecheck
		| autoMode o = numCopiesCheck file key (<) <||> wantGet False (Just key) (Just file)
		| otherwise = return True

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
		showStart' "get" key afile
		next a

perform :: Key -> AssociatedFile -> CommandPerform
perform key afile = stopUnless (getKey key afile) $
	next $ return True -- no cleanup needed

{- Try to find a copy of the file in one of the remotes,
 - and copy it to here. -}
getKey :: Key -> AssociatedFile -> Annex Bool
getKey key afile = getKey' key afile =<< Remote.keyPossibilities key

getKey' :: Key -> AssociatedFile -> [Remote] -> Annex Bool
getKey' key afile = dispatch
  where
	dispatch [] = do
		showNote "not available"
		showlocs
		return False
	dispatch remotes = notifyTransfer Download afile $ trycopy remotes remotes
	trycopy full [] _ = do
		Remote.showTriedRemotes full
		showlocs
		return False
	trycopy full (r:rs) witness =
		ifM (probablyPresent r)
			( docopy r witness <||> trycopy full rs witness
			, trycopy full rs witness
			)
	showlocs = Remote.showLocations False key []
		"No other repository is known to contain the file."
	-- This check is to avoid an ugly message if a remote is a
	-- drive that is not mounted.
	probablyPresent r
		| Remote.hasKeyCheap r =
			either (const False) id <$> Remote.hasKey r key
		| otherwise = return True
	docopy r witness = getViaTmp (RemoteVerify r) key $ \dest ->
		download (Remote.uuid r) key afile noRetry noObserver 
			(\p -> do
				showAction $ "from " ++ Remote.name r
				Remote.retrieveKeyFile r key afile dest p
			) witness
