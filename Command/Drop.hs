{- git-annex command
 -
 - Copyright 2010-2020 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Command.Drop where

import Command
import qualified Remote
import qualified Annex
import Annex.UUID
import Logs.Location
import Logs.Trust
import Logs.PreferredContent
import Annex.NumCopies
import Annex.Content
import Annex.Wanted
import Annex.Notification

import System.Log.Logger (debugM)
import qualified Data.Set as S

cmd :: Command
cmd = withGlobalOptions [jobsOption, jsonOptions, annexedMatchingOptions] $
	command "drop" SectionCommon
		"remove content of files from repository"
		paramPaths (seek <$$> optParser)

data DropOptions = DropOptions
	{ dropFiles :: CmdParams
	, dropFrom :: Maybe (DeferredParse Remote)
	, autoMode :: Bool
	, keyOptions :: Maybe KeyOptions
	, batchOption :: BatchMode
	}

optParser :: CmdParamsDesc -> Parser DropOptions
optParser desc = DropOptions
	<$> cmdParams desc
	<*> optional parseDropFromOption
	<*> parseAutoOption
	<*> optional parseKeyOptions
	<*> parseBatchOption

parseDropFromOption :: Parser (DeferredParse Remote)
parseDropFromOption = parseRemoteOption <$> strOption
	( long "from" <> short 'f' <> metavar paramRemote
	<> help "drop content from a remote"
	<> completeRemotes
	)

seek :: DropOptions -> CommandSeek
seek o = startConcurrency commandStages $ do
	from <- case dropFrom o of
		Nothing -> pure Nothing
		Just f -> getParsed f >>= \remote -> do
			u <- getUUID
			if Remote.uuid remote == u
				then pure Nothing
				else pure (Just remote)				
	let seeker = AnnexedFileSeeker
		{ startAction = start o from
		, checkContentPresent = case from of
			Nothing -> Just True
			Just _ -> Nothing
		, usesLocationLog = True
		}
	case batchOption o of
		Batch fmt -> batchAnnexedFilesMatching fmt seeker
		NoBatch -> withKeyOptions (keyOptions o) (autoMode o) seeker
			(commandAction . startKeys o from)
			(withFilesInGitAnnex ww seeker)
			=<< workTreeItems ww (dropFiles o)
  where
	ww = WarnUnmatchLsFiles

start :: DropOptions -> Maybe Remote -> RawFilePath -> Key -> CommandStart
start o from file key = start' o from key afile ai
  where
	afile = AssociatedFile (Just file)
	ai = mkActionItem (key, afile)

start' :: DropOptions -> Maybe Remote -> Key -> AssociatedFile -> ActionItem -> CommandStart
start' o from key afile ai = 
	checkDropAuto (autoMode o) from afile key $ \numcopies ->
		stopUnless want $
			case from of
				Nothing -> startLocal afile ai numcopies key []
				Just remote -> startRemote afile ai numcopies key remote
  where
	want
		| autoMode o = wantDrop False (Remote.uuid <$> from) (Just key) afile
		| otherwise = return True

startKeys :: DropOptions -> Maybe Remote -> (Key, ActionItem) -> CommandStart
startKeys o from (key, ai) = start' o from key (AssociatedFile Nothing) ai

startLocal :: AssociatedFile -> ActionItem -> NumCopies -> Key -> [VerifiedCopy] -> CommandStart
startLocal afile ai numcopies key preverified =
	-- This is a redundant check, because checkContentPresent was
	-- enabled when seeking. However, when two files have the same key,
	-- the content may have already been removed, which would cause
	-- this to fail, so it has to be checked again.
	stopUnless (inAnnex key) $
		starting "drop" (OnlyActionOn key ai) $
			performLocal key afile numcopies preverified

startRemote :: AssociatedFile -> ActionItem -> NumCopies -> Key -> Remote -> CommandStart
startRemote afile ai numcopies key remote = 
	starting ("drop " ++ Remote.name remote) (OnlyActionOn key ai) $
		performRemote key afile numcopies remote

performLocal :: Key -> AssociatedFile -> NumCopies -> [VerifiedCopy] -> CommandPerform
performLocal key afile numcopies preverified = lockContentForRemoval key $ \contentlock -> do
	u <- getUUID
	(tocheck, verified) <- verifiableCopies key [u]
	doDrop u (Just contentlock) key afile numcopies [] (preverified ++ verified) tocheck
		( \proof -> do
			liftIO $ debugM "drop" $ unwords
				[ "Dropping from here"
				, "proof:"
				, show proof
				]
			removeAnnex contentlock
			notifyDrop afile True
			next $ cleanupLocal key
		, do 
			notifyDrop afile False
			stop
		)

performRemote :: Key -> AssociatedFile -> NumCopies -> Remote -> CommandPerform
performRemote key afile numcopies remote = do
	-- Filter the uuid it's being dropped from out of the lists of
	-- places assumed to have the key, and places to check.
	(tocheck, verified) <- verifiableCopies key [uuid]
	doDrop uuid Nothing key afile numcopies [uuid] verified tocheck
		( \proof -> do 
			liftIO $ debugM "drop" $ unwords
				[ "Dropping from remote"
				, show remote
				, "proof:"
				, show proof
				]
			ok <- Remote.action (Remote.removeKey remote key)
			next $ cleanupRemote key remote ok
		, stop
		)
  where
	uuid = Remote.uuid remote

cleanupLocal :: Key -> CommandCleanup
cleanupLocal key = do
	logStatus key InfoMissing
	return True

cleanupRemote :: Key -> Remote -> Bool -> CommandCleanup
cleanupRemote key remote ok = do
	when ok $
		Remote.logStatus remote key InfoMissing
	return ok

{- Before running the dropaction, checks specified remotes to
 - verify that enough copies of a key exist to allow it to be
 - safely removed (with no data loss).
 -
 - Also checks if it's required content, and refuses to drop if so.
 -
 - --force overrides and always allows dropping.
 -}
doDrop
	:: UUID
	-> Maybe ContentRemovalLock
	-> Key
	-> AssociatedFile
	-> NumCopies
	-> [UUID]
	-> [VerifiedCopy]
	-> [UnVerifiedCopy]
	-> (Maybe SafeDropProof -> CommandPerform, CommandPerform)
	-> CommandPerform
doDrop dropfrom contentlock key afile numcopies skip preverified check (dropaction, nodropaction) = 
	ifM (Annex.getState Annex.force)
		( dropaction Nothing
		, ifM (checkRequiredContent dropfrom key afile)
			( verifyEnoughCopiesToDrop nolocmsg key 
				contentlock numcopies
				skip preverified check
					(dropaction . Just)
					(forcehint nodropaction)
			, stop
			)
		)
  where
	nolocmsg = "Rather than dropping this file, try using: git annex move"
	forcehint a = do
		showLongNote "(Use --force to override this check, or adjust numcopies.)"
		a

checkRequiredContent :: UUID -> Key -> AssociatedFile -> Annex Bool
checkRequiredContent u k afile =
	ifM (isRequiredContent (Just u) S.empty (Just k) afile False)
		( requiredContent
		, return True
		)

requiredContent :: Annex Bool
requiredContent = do
	showLongNote "That file is required content, it cannot be dropped!"
	showLongNote "(Use --force to override this check, or adjust required content configuration.)"
	return False

{- In auto mode, only runs the action if there are enough
 - copies on other semitrusted repositories. -}
checkDropAuto :: Bool -> Maybe Remote -> AssociatedFile -> Key -> (NumCopies -> CommandStart) -> CommandStart
checkDropAuto automode mremote afile key a =
	go =<< getAssociatedFileNumCopies afile
  where
	go numcopies
		| automode = do
			locs <- Remote.keyLocations key
			uuid <- getUUID
			let remoteuuid = fromMaybe uuid $ Remote.uuid <$> mremote
			locs' <- trustExclude UnTrusted $ filter (/= remoteuuid) locs
			if NumCopies (length locs') >= numcopies
				then a numcopies
				else stop
		| otherwise = a numcopies
