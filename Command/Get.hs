{- git-annex command
 -
 - Copyright 2010-2023 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Command.Get where

import Command
import qualified Remote
import Annex.Transfer
import Annex.NumCopies
import Annex.Wanted
import qualified Command.Move
import Logs.Location

cmd :: Command
cmd = withAnnexOptions [jobsOption, jsonOptions, jsonProgressOption, annexedMatchingOptions] $ 
	command "get" SectionCommon 
		"make content of annexed files available"
		paramPaths (seek <$$> optParser)

data GetOptions = GetOptions
	{ getFiles :: CmdParams
	, getFrom :: Maybe (DeferredParse Remote)
	, autoMode :: Bool
	, keyOptions :: Maybe KeyOptions
	, batchOption :: BatchMode
	}

optParser :: CmdParamsDesc -> Parser GetOptions
optParser desc = GetOptions
	<$> cmdParams desc
	<*> optional (mkParseRemoteOption <$> parseFromOption)
	<*> parseAutoOption
	<*> optional (parseIncompleteOption <|> parseKeyOptions <|> parseFailedTransfersOption)
	<*> parseBatchOption True

seek :: GetOptions -> CommandSeek
seek o = startConcurrency transferStages $ do
	from <- maybe (pure Nothing) (Just <$$> getParsed) (getFrom o)
	let seeker = AnnexedFileSeeker
		{ startAction = const $ start o from
		, checkContentPresent = Just False
		, usesLocationLog = True
		}
	case batchOption o of
		NoBatch -> withKeyOptions (keyOptions o) (autoMode o) seeker
			(commandAction . startKeys from)
			(withFilesInGitAnnex ww seeker)
			=<< workTreeItems ww (getFiles o)
		Batch fmt -> batchOnly (keyOptions o) (getFiles o) $
			batchAnnexed fmt seeker (startKeys from)
  where
	ww = WarnUnmatchLsFiles "get"

start :: GetOptions -> Maybe Remote -> SeekInput -> RawFilePath -> Key -> CommandStart
start o from si file key = start' expensivecheck from key afile ai si
  where
	afile = AssociatedFile (Just file)
	ai = mkActionItem (key, afile)
	expensivecheck
		| autoMode o = numCopiesCheck file key (<)
			<||> wantGet False (Just key) afile
		| otherwise = return True

startKeys :: Maybe Remote -> (SeekInput, Key, ActionItem) -> CommandStart
startKeys from (si, key, ai) = checkFailedTransferDirection ai Download $
	start' (return True) from key (AssociatedFile Nothing) ai si

start' :: Annex Bool -> Maybe Remote -> Key -> AssociatedFile -> ActionItem -> SeekInput -> CommandStart
start' expensivecheck from key afile ai si =
	stopUnless expensivecheck $
		case from of
			Nothing -> go $ perform key afile
			Just src ->
				stopUnless (Command.Move.fromOk src key) $
					go $ Command.Move.fromPerform src Command.Move.RemoveNever key afile
  where
	go = starting "get" (OnlyActionOn key ai) si

perform :: Key -> AssociatedFile -> CommandPerform
perform key afile = stopUnless (getKey key afile) $
	next $ return True -- no cleanup needed

{- Try to find a copy of the file in one of the remotes,
 - and copy it to here. -}
getKey :: Key -> AssociatedFile -> Annex Bool
getKey key afile = getKey' key afile
	=<< Remote.keyPossibilities (Remote.IncludeIgnored False) key

getKey' :: Key -> AssociatedFile -> [Remote] -> Annex Bool
getKey' key afile = dispatch
  where
	dispatch [] = do
		showNote (UnquotedString "not available")
		showlocs []
		return False
	dispatch remotes = notifyTransfer Download afile $ \witness -> do
		ok <- pickRemote remotes $ \r -> ifM (probablyPresent r)
			( docopy r witness
			, return False
			)
		if ok
			then return ok
			else do
				Remote.showTriedRemotes remotes
				showlocs (map Remote.uuid remotes)
				return False
	showlocs exclude = Remote.showLocations False key
		(\u -> pure (u `elem` exclude))
		"No other repository is known to contain the file."
	-- This check is to avoid an ugly message if a remote is a
	-- drive that is not mounted.
	probablyPresent r
		| Remote.hasKeyCheap r =
			either (const False) id <$> Remote.hasKey r key
		| otherwise = return True
	docopy r witness = do
		showAction $ UnquotedString $ "from " ++ Remote.name r
		logStatusAfter key $
			download r key afile stdRetry witness
