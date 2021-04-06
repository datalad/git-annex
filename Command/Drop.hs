{- git-annex command
 -
 - Copyright 2010-2021 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

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

start :: DropOptions -> Maybe Remote -> SeekInput -> RawFilePath -> Key -> CommandStart
start o from si file key = start' o from key afile ai si
  where
	afile = AssociatedFile (Just file)
	ai = mkActionItem (key, afile)

start' :: DropOptions -> Maybe Remote -> Key -> AssociatedFile -> ActionItem -> SeekInput -> CommandStart
start' o from key afile ai si = 
	checkDropAuto (autoMode o) from afile key $ \numcopies mincopies ->
		stopUnless want $
			case from of
				Nothing -> startLocal afile ai si numcopies mincopies key []
				Just remote -> startRemote afile ai si numcopies mincopies key remote
  where
	want
		| autoMode o = wantDrop False (Remote.uuid <$> from) (Just key) afile
		| otherwise = return True

startKeys :: DropOptions -> Maybe Remote -> (SeekInput, Key, ActionItem) -> CommandStart
startKeys o from (si, key, ai) = start' o from key (AssociatedFile Nothing) ai si

startLocal :: AssociatedFile -> ActionItem -> SeekInput -> NumCopies -> MinCopies -> Key -> [VerifiedCopy] -> CommandStart
startLocal afile ai si numcopies mincopies key preverified =
	starting "drop" (OnlyActionOn key ai) si $
		performLocal key afile numcopies mincopies preverified

startRemote :: AssociatedFile -> ActionItem -> SeekInput -> NumCopies -> MinCopies -> Key -> Remote -> CommandStart
startRemote afile ai si numcopies mincopies key remote = 
	starting ("drop " ++ Remote.name remote) (OnlyActionOn key ai) si $
		performRemote key afile numcopies mincopies remote

performLocal :: Key -> AssociatedFile -> NumCopies -> MinCopies -> [VerifiedCopy] -> CommandPerform
performLocal key afile numcopies mincopies preverified = lockContentForRemoval key fallback $ \contentlock -> do
	u <- getUUID
	(tocheck, verified) <- verifiableCopies key [u]
	doDrop u (Just contentlock) key afile numcopies mincopies [] (preverified ++ verified) tocheck
		( \proof -> do
			fastDebug "Command.Drop" $ unwords
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
  where
	-- This occurs when, for example, two files are being dropped
	-- and have the same content. The seek stage checks if the content
	-- is present, but due to buffering, may find it present for the
	-- second file before the first is dropped. If so, nothing remains
	-- to be done except for cleaning up.
	fallback = next $ cleanupLocal key

performRemote :: Key -> AssociatedFile -> NumCopies -> MinCopies -> Remote -> CommandPerform
performRemote key afile numcopies mincopies remote = do
	-- Filter the uuid it's being dropped from out of the lists of
	-- places assumed to have the key, and places to check.
	(tocheck, verified) <- verifiableCopies key [uuid]
	doDrop uuid Nothing key afile numcopies mincopies [uuid] verified tocheck
		( \proof -> do 
			fastDebug "Command.Drop" $ unwords
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
	-> MinCopies
	-> [UUID]
	-> [VerifiedCopy]
	-> [UnVerifiedCopy]
	-> (Maybe SafeDropProof -> CommandPerform, CommandPerform)
	-> CommandPerform
doDrop dropfrom contentlock key afile numcopies mincopies skip preverified check (dropaction, nodropaction) = 
	ifM (Annex.getState Annex.force)
		( dropaction Nothing
		, ifM (checkRequiredContent dropfrom key afile)
			( verifyEnoughCopiesToDrop nolocmsg key 
				contentlock numcopies mincopies
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
checkDropAuto :: Bool -> Maybe Remote -> AssociatedFile -> Key -> (NumCopies -> MinCopies -> CommandStart) -> CommandStart
checkDropAuto automode mremote afile key a =
	go =<< getAssociatedFileNumMinCopies afile
  where
	go (numcopies, mincopies)
		| automode = do
			locs <- Remote.keyLocations key
			uuid <- getUUID
			let remoteuuid = fromMaybe uuid $ Remote.uuid <$> mremote
			locs' <- trustExclude UnTrusted $ filter (/= remoteuuid) locs
			if NumCopies (length locs') >= numcopies
				then a numcopies mincopies
				else stop
		| otherwise = a numcopies mincopies
