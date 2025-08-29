{- git-annex command
 -
 - Copyright 2010-2025 Joey Hess <id@joeyh.name>
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

cmd :: Command
cmd = withAnnexOptions [jobsOption, jsonOptions, annexedMatchingOptions] $
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
	<*> parseBatchOption True

parseDropFromOption :: Parser (DeferredParse Remote)
parseDropFromOption = mkParseRemoteOption <$> strOption
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
		{ startAction = const $ start o from
		, checkContentPresent = case from of
			Nothing -> Just True
			Just _ -> Nothing
		, usesLocationLog = True
		}
	case batchOption o of
		NoBatch -> withKeyOptions (keyOptions o) (autoMode o) seeker
			(commandAction . startKeys o from)
			(withFilesInGitAnnex ww seeker)
			=<< workTreeItems ww (dropFiles o)
		Batch fmt -> batchOnly (keyOptions o) (dropFiles o) $
			batchAnnexed fmt seeker (startKeys o from)
  where
	ww = WarnUnmatchLsFiles "drop"

start :: DropOptions -> Maybe Remote -> SeekInput -> OsPath -> Key -> CommandStart
start o from si file key = start' o from key afile ai si
  where
	afile = AssociatedFile (Just file)
	ai = mkActionItem (key, afile)

start' :: DropOptions -> Maybe Remote -> Key -> AssociatedFile -> ActionItem -> SeekInput -> CommandStart
start' o from key afile ai si = do
	checkDropAuto (autoMode o) from afile key $ \numcopies mincopies -> do
		lu <- prepareLiveUpdate remoteuuid key RemovingKey
		stopUnless (wantdrop lu) $
			case from of
				Nothing -> startLocal lu pcc afile ai si numcopies mincopies key [] ud
				Just remote -> startRemote lu pcc afile ai si numcopies mincopies key ud remote
  where
	remoteuuid = Remote.uuid <$> from
	wantdrop lu
		| autoMode o = wantDrop lu False remoteuuid (Just key) afile Nothing
		| otherwise = return True
	pcc = PreferredContentChecked (autoMode o)
	ud = case (batchOption o, keyOptions o) of
		(NoBatch, Just WantUnusedKeys) -> DroppingUnused True
		_ -> DroppingUnused False

startKeys :: DropOptions -> Maybe Remote -> (SeekInput, Key, ActionItem) -> CommandStart
startKeys o from (si, key, ai) = start' o from key (AssociatedFile Nothing) ai si

startLocal :: LiveUpdate -> PreferredContentChecked -> AssociatedFile -> ActionItem -> SeekInput -> NumCopies -> MinCopies -> Key -> [VerifiedCopy] -> DroppingUnused -> CommandStart
startLocal lu pcc afile ai si numcopies mincopies key preverified ud =
	starting "drop" (OnlyActionOn key ai) si $
		performLocal lu pcc key afile numcopies mincopies preverified ud

startRemote :: LiveUpdate -> PreferredContentChecked -> AssociatedFile -> ActionItem -> SeekInput -> NumCopies -> MinCopies -> Key -> DroppingUnused -> Remote -> CommandStart
startRemote lu pcc afile ai si numcopies mincopies key ud remote = do
	fast <- Annex.getRead Annex.fast
	if fast
		then do
			remotes <- Remote.keyPossibilities (Remote.IncludeIgnored True) key
			if remote `elem` remotes
				then go
				else stop
		else go
  where
	go = starting "drop" (OnlyActionOn key ai) si $ do
		showAction $ UnquotedString $ "from " ++ Remote.name remote
		performRemote lu pcc key afile numcopies mincopies remote ud

performLocal :: LiveUpdate -> PreferredContentChecked -> Key -> AssociatedFile -> NumCopies -> MinCopies -> [VerifiedCopy] -> DroppingUnused -> CommandPerform
performLocal lu pcc key afile numcopies mincopies preverified ud = lockContentForRemoval key fallback $ \contentlock -> do
	u <- getUUID
	(tocheck, verified) <- verifiableCopies key [u]
	doDrop lu pcc u (Just contentlock) key afile numcopies mincopies [] (preverified ++ verified) tocheck
		( \proof -> do
			fastDebug "Command.Drop" $ unwords
				[ "Dropping from here"
				, "proof:"
				, show proof
				]
			removeAnnex contentlock
			notifyDrop afile True
			next $ cleanupLocal lu key ud
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
	fallback = next $ cleanupLocal lu key ud

performRemote :: LiveUpdate -> PreferredContentChecked -> Key -> AssociatedFile -> NumCopies -> MinCopies -> Remote -> DroppingUnused -> CommandPerform
performRemote lu pcc key afile numcopies mincopies remote ud = do
	-- Filter the uuid it's being dropped from out of the lists of
	-- places assumed to have the key, and places to check.
	(tocheck, verified) <- verifiableCopies key [uuid]
	doDrop lu pcc uuid Nothing key afile numcopies mincopies [uuid] verified tocheck
		( \proof -> do 
			fastDebug "Command.Drop" $ unwords
				[ "Dropping from remote"
				, show remote
				, "proof:"
				, show proof
				]
			ok <- Remote.action (Remote.removeKey remote proof key)
			next $ cleanupRemote lu key remote ud ok
		, stop
		)
  where
	uuid = Remote.uuid remote

cleanupLocal :: LiveUpdate -> Key -> DroppingUnused -> CommandCleanup
cleanupLocal lu key ud = do
	logStatus lu key (dropStatus ud)
	return True

cleanupRemote :: LiveUpdate -> Key -> Remote -> DroppingUnused -> Bool -> CommandCleanup
cleanupRemote lu key remote ud ok = do
	when ok $
		Remote.logStatus lu remote key (dropStatus ud)
	return ok

{- Set when the user explicitly chose to operate on unused content.
 - Presumably the user still expects the last git-annex unused to be
 - correct at this point. -}
newtype DroppingUnused = DroppingUnused Bool

{- When explicitly dropping unused content, mark the key as dead, at least
 - in the repository it was dropped from. It may still be in other
 - repositories, and will not be treated as dead until dropped from all of
 - them. -}
dropStatus :: DroppingUnused -> LogStatus
dropStatus (DroppingUnused False) = InfoMissing
dropStatus (DroppingUnused True) = InfoDead

{- Before running the dropaction, checks specified remotes to
 - verify that enough copies of a key exist to allow it to be
 - safely removed (with no data loss).
 -
 - --force overrides and always allows dropping.
 -}
doDrop
	:: LiveUpdate
	-> PreferredContentChecked
	-> UUID
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
doDrop lu pcc dropfrom contentlock key afile numcopies mincopies skip preverified check (dropaction, nodropaction) = 
	ifM (Annex.getRead Annex.force)
		( dropaction Nothing
		, ifM (checkRequiredContent lu pcc dropfrom key afile)
			( verifyEnoughCopiesToDrop nolocmsg key (Just dropfrom)
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

{- Checking preferred content also checks required content, so when
 - auto mode causes preferred content to be checked, it's redundant
 - for checkRequiredContent to separately check required content, and
 - providing this avoids that extra work. -}
newtype PreferredContentChecked = PreferredContentChecked Bool

checkRequiredContent :: LiveUpdate -> PreferredContentChecked -> UUID -> Key -> AssociatedFile -> Annex Bool
checkRequiredContent _ (PreferredContentChecked True) _ _ _ = return True
checkRequiredContent lu (PreferredContentChecked False) u k afile =
	checkDrop isRequiredContent lu False (Just u) (Just k) afile Nothing >>= \case
		Nothing -> return True
		Just afile' -> do
			if afile == afile'
				then showLongNote "That file is required content. It cannot be dropped!"
				else showLongNote $ "That file has the same content as another file"
					<> case afile' of
						AssociatedFile (Just f) -> " (" <> QuotedPath f <> "),"
						AssociatedFile Nothing -> ""
					<> " which is required content. It cannot be dropped!"
			showLongNote "(Use --force to override this check, or adjust required content configuration.)"
			return False

{- In auto mode, only runs the action if there are enough
 - copies on other semitrusted repositories. -}
checkDropAuto :: Bool -> Maybe Remote -> AssociatedFile -> Key -> (NumCopies -> MinCopies -> CommandStart) -> CommandStart
checkDropAuto automode mremote afile key a =
	go =<< getSafestNumMinCopies afile key
  where
	go (numcopies, mincopies)
		| automode = do
			locs <- Remote.keyLocations key
			uuid <- getUUID
			let remoteuuid = fromMaybe uuid $ Remote.uuid <$> mremote
			locs' <- trustExclude UnTrusted $ filter (/= remoteuuid) locs
			if numCopiesCheck'' locs' (>=) numcopies
				then a numcopies mincopies
				else stop
		| otherwise = a numcopies mincopies
