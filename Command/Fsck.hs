{- git-annex command
 -
 - Copyright 2010-2019 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Command.Fsck where

import Command
import qualified Annex
import qualified Remote
import qualified Types.Backend
import qualified Backend
import Annex.Content
import Annex.Perms
import Annex.Link
import Logs.Location
import Logs.Trust
import Logs.Activity
import Utility.TimeStamp
import Logs.PreferredContent
import Annex.NumCopies
import Annex.UUID
import Annex.ReplaceFile
import Utility.DataUnits
import Utility.HumanTime
import Utility.CopyFile
import Git.FilePath
import Utility.PID
import qualified Database.Keys
import qualified Database.Fsck as FsckDb
import Types.CleanupActions
import Types.Key
import Types.ActionItem

import Data.Time.Clock.POSIX
import System.Posix.Types (EpochTime)
import qualified Data.Set as S
import qualified Data.Map as M

cmd :: Command
cmd = withGlobalOptions [jobsOption, jsonOptions, annexedMatchingOptions] $
	command "fsck" SectionMaintenance
		"find and fix problems"
		paramPaths (seek <$$> optParser)

data FsckOptions = FsckOptions
	{ fsckFiles :: CmdParams
	, fsckFromOption :: Maybe (DeferredParse Remote)
	, incrementalOpt :: Maybe IncrementalOpt
	, keyOptions :: Maybe KeyOptions
	}

data IncrementalOpt
	= StartIncrementalO
	| MoreIncrementalO
	| ScheduleIncrementalO Duration

optParser :: CmdParamsDesc -> Parser FsckOptions
optParser desc = FsckOptions
	<$> cmdParams desc
	<*> optional (parseRemoteOption <$> strOption 
		( long "from" <> short 'f' <> metavar paramRemote 
		<> help "check remote"
		<> completeRemotes
		))
	<*> optional parseincremental
	<*> optional parseKeyOptions
  where
	parseincremental =
		flag' StartIncrementalO
			( long "incremental" <> short 'S'
			<> help "start an incremental fsck"
			)
		<|> flag' MoreIncrementalO
			( long "more" <> short 'm'
			<> help "continue an incremental fsck"
			)
		<|> (ScheduleIncrementalO <$> option (str >>= parseDuration)
			( long "incremental-schedule" <> metavar paramTime
			<> help "schedule incremental fscking"
			))

seek :: FsckOptions -> CommandSeek
seek o = startConcurrency commandStages $ do
	from <- maybe (pure Nothing) (Just <$$> getParsed) (fsckFromOption o)
	u <- maybe getUUID (pure . Remote.uuid) from
	checkDeadRepo u
	i <- prepIncremental u (incrementalOpt o)
	withKeyOptions (keyOptions o) False
		(\kai -> commandAction . startKey from i kai =<< getNumCopies)
		(withFilesInGit $ commandAction . (whenAnnexed (start from i)))
		=<< workTreeItems (fsckFiles o)
	cleanupIncremental i
	void $ tryIO $ recordActivity Fsck u

checkDeadRepo :: UUID -> Annex ()
checkDeadRepo u =
	whenM ((==) DeadTrusted <$> lookupTrust u) $
		earlyWarning "Warning: Fscking a repository that is currently marked as dead."

start :: Maybe Remote -> Incremental -> FilePath -> Key -> CommandStart
start from inc file key = Backend.getBackend file key >>= \case
	Nothing -> stop
	Just backend -> do
		numcopies <- getFileNumCopies file
		case from of
			Nothing -> go $ perform key file backend numcopies
			Just r -> go $ performRemote key afile backend numcopies r
  where
	go = runFsck inc (mkActionItem (key, afile)) key
	afile = AssociatedFile (Just file)

perform :: Key -> FilePath -> Backend -> NumCopies -> Annex Bool
perform key file backend numcopies = do
	keystatus <- getKeyFileStatus key file
	check
		-- order matters
		[ fixLink key file
		, verifyLocationLog key keystatus ai
		, verifyRequiredContent key ai
		, verifyAssociatedFiles key keystatus file
		, verifyWorkTree key file
		, checkKeySize key keystatus ai
		, checkBackend backend key keystatus afile
		, checkKeyUpgrade backend key ai afile
		, checkKeyNumCopies key afile numcopies
		]
  where
	afile = AssociatedFile (Just file)
	ai = mkActionItem (key, afile)

{- To fsck a remote, the content is retrieved to a tmp file,
 - and checked locally. -}
performRemote :: Key -> AssociatedFile -> Backend -> NumCopies -> Remote -> Annex Bool
performRemote key afile backend numcopies remote =
	dispatch =<< Remote.hasKey remote key
  where
	dispatch (Left err) = do
		showNote err
		return False
	dispatch (Right True) = withtmp $ \tmpfile ->
		getfile tmpfile >>= \case
			Nothing -> go True Nothing
			Just True -> go True (Just tmpfile)
			Just False -> do
				warning "failed to download file from remote"
				void $ go True Nothing
				return False
	dispatch (Right False) = go False Nothing
	go present localcopy = check
		[ verifyLocationLogRemote key ai remote present
		, verifyRequiredContent key ai
		, withLocalCopy localcopy $ checkKeySizeRemote key remote ai
		, withLocalCopy localcopy $ checkBackendRemote backend key remote ai
		, checkKeyNumCopies key afile numcopies
		]
	ai = mkActionItem (key, afile)
	withtmp a = do
		pid <- liftIO getPID
		t <- fromRepo gitAnnexTmpObjectDir
		createAnnexDirectory t
		let tmp = t </> "fsck" ++ show pid ++ "." ++ keyFile key
		let cleanup = liftIO $ catchIO (removeFile tmp) (const noop)
		cleanup
		cleanup `after` a tmp
	getfile tmp = ifM (checkDiskSpace (Just (takeDirectory tmp)) key 0 True)
		( ifM (Remote.retrieveKeyFileCheap remote key afile tmp)
			( return (Just True)
			, ifM (Annex.getState Annex.fast)
				( return Nothing
				, Just . fst <$>
					Remote.retrieveKeyFile remote key (AssociatedFile Nothing) tmp dummymeter
				)
			)
		, return (Just False)
		)
	dummymeter _ = noop

startKey :: Maybe Remote -> Incremental -> (Key, ActionItem) -> NumCopies -> CommandStart
startKey from inc (key, ai) numcopies =
	case Backend.maybeLookupBackendVariety (keyVariety key) of
		Nothing -> stop
		Just backend -> runFsck inc ai key $
			case from of
				Nothing -> performKey key backend numcopies
				Just r -> performRemote key (AssociatedFile Nothing) backend numcopies r

performKey :: Key -> Backend -> NumCopies -> Annex Bool
performKey key backend numcopies = do
	keystatus <- getKeyStatus key
	check
		[ verifyLocationLog key keystatus (mkActionItem key)
		, checkKeySize key keystatus (mkActionItem key)
		, checkBackend backend key keystatus (AssociatedFile Nothing)
		, checkKeyNumCopies key (AssociatedFile Nothing) numcopies
		]

check :: [Annex Bool] -> Annex Bool
check cs = and <$> sequence cs

{- Checks that symlinks points correctly to the annexed content. -}
fixLink :: Key -> FilePath -> Annex Bool
fixLink key file = do
	want <- calcRepo $ gitAnnexLink file key
	have <- getAnnexLinkTarget file
	maybe noop (go want) have
	return True
  where
	go want have
		| want /= fromInternalGitPath (fromRawFilePath have) = do
			showNote "fixing link"
			liftIO $ createDirectoryIfMissing True (parentDir file)
			liftIO $ removeFile file
			addAnnexLink want file
		| otherwise = noop

{- Checks that the location log reflects the current status of the key,
 - in this repository only. -}
verifyLocationLog :: Key -> KeyStatus -> ActionItem -> Annex Bool
verifyLocationLog key keystatus ai = do
	obj <- calcRepo $ gitAnnexLocation key
	present <- if isKeyUnlockedThin keystatus
		then liftIO (doesFileExist obj)
		else inAnnex key
	u <- getUUID
	
	{- Since we're checking that a key's object file is present, throw
	 - in a permission fixup here too. -}
	when present $ do
		void $ tryIO $ case keystatus of
			KeyUnlockedThin -> thawContent obj
			KeyLockedThin -> thawContent obj
			_ -> freezeContent obj
		unlessM (isContentWritePermOk obj) $
			warning $ "** Unable to set correct write mode for " ++ obj ++ " ; perhaps you don't own that file"
	whenM (liftIO $ doesDirectoryExist $ parentDir obj) $
		freezeContentDir obj

	{- Warn when annex.securehashesonly is set and content using an 
	 - insecure hash is present. This should only be able to happen
	 - if the repository already contained the content before the
	 - config was set. -}
	when (present && not (cryptographicallySecure (keyVariety key))) $
		whenM (annexSecureHashesOnly <$> Annex.getGitConfig) $
			warning $ "** Despite annex.securehashesonly being set, " ++ obj ++ " has content present in the annex using an insecure " ++ decodeBS (formatKeyVariety (keyVariety key)) ++ " key"

	if not present
		then return True
		else verifyLocationLog' key ai present u (logChange key u)

verifyLocationLogRemote :: Key -> ActionItem -> Remote -> Bool -> Annex Bool
verifyLocationLogRemote key ai remote present =
	verifyLocationLog' key ai present (Remote.uuid remote)
		(Remote.logStatus remote key)

verifyLocationLog' :: Key -> ActionItem -> Bool -> UUID -> (LogStatus -> Annex ()) -> Annex Bool
verifyLocationLog' key ai present u updatestatus = do
	uuids <- loggedLocations key
	case (present, u `elem` uuids) of
		(True, False) -> do
			fix InfoPresent
			-- There is no data loss, so do not fail.
			return True
		(False, True) -> do
			fix InfoMissing
			warning $
				"** Based on the location log, " ++
				actionItemDesc ai ++
				"\n** was expected to be present, " ++
				"but its content is missing."
			return False
		(False, False) -> do
			-- When the location log for the key is not present,
			-- create it, so that the key will be known.
			when (null uuids) $
				whenM (not <$> isKnownKey key) $
					updatestatus InfoMissing
			return True
		(True, True) -> return True
  where
	fix s = do
		showNote "fixing location log"
		updatestatus s

{- Verifies that all repos that are required to contain the content do,
 - checking against the location log. -}
verifyRequiredContent :: Key -> ActionItem -> Annex Bool
verifyRequiredContent key ai@(ActionItemAssociatedFile afile _) = do
	requiredlocs <- S.fromList . M.keys <$> requiredContentMap
	if S.null requiredlocs
		then return True
		else do
			presentlocs <- S.fromList <$> loggedLocations key
			missinglocs <- filterM
				(\u -> isRequiredContent (Just u) S.empty (Just key) afile False)
				(S.toList $ S.difference requiredlocs presentlocs)
			if null missinglocs
				then return True
				else do
					missingrequired <- Remote.prettyPrintUUIDs "missingrequired" missinglocs
					warning $
						"** Required content " ++
						actionItemDesc ai ++
						" is missing from these repositories:\n" ++
						missingrequired
					return False
verifyRequiredContent _ _ = return True

{- Verifies the associated file records. -}
verifyAssociatedFiles :: Key -> KeyStatus -> FilePath -> Annex Bool
verifyAssociatedFiles key keystatus file = do
	when (isKeyUnlockedThin keystatus) $ do
		f <- inRepo $ toTopFilePath file
		afs <- Database.Keys.getAssociatedFiles key
		unless (getTopFilePath f `elem` map getTopFilePath afs) $
			Database.Keys.addAssociatedFile key f
	return True

verifyWorkTree :: Key -> FilePath -> Annex Bool
verifyWorkTree key file = do
	{- Make sure that a pointer file is replaced with its content,
	 - when the content is available. -}
	mk <- liftIO $ isPointerFile file
	case mk of
		Just k | k == key -> whenM (inAnnex key) $ do
			showNote "fixing worktree content"
			replaceFile file $ \tmp -> do
				mode <- liftIO $ catchMaybeIO $ fileMode <$> getFileStatus file
				ifM (annexThin <$> Annex.getGitConfig)
					( void $ linkFromAnnex key tmp mode
					, do
						obj <- calcRepo $ gitAnnexLocation key
						void $ checkedCopyFile key obj tmp mode
						thawContent tmp
					)
			Database.Keys.storeInodeCaches key [file]
		_ -> return ()
	return True

{- The size of the data for a key is checked against the size encoded in
 - the key's metadata, if available.
 -
 - Not checked when a file is unlocked.
 -}
checkKeySize :: Key -> KeyStatus -> ActionItem -> Annex Bool
checkKeySize _ KeyUnlockedThin _ = return True
checkKeySize key _ ai = do
	file <- calcRepo $ gitAnnexLocation key
	ifM (liftIO $ doesFileExist file)
		( checkKeySizeOr badContent key file ai
		, return True
		)

withLocalCopy :: Maybe FilePath -> (FilePath -> Annex Bool) -> Annex Bool
withLocalCopy Nothing _ = return True
withLocalCopy (Just localcopy) f = f localcopy

checkKeySizeRemote :: Key -> Remote -> ActionItem -> FilePath -> Annex Bool
checkKeySizeRemote key remote ai localcopy =
	checkKeySizeOr (badContentRemote remote localcopy) key localcopy ai

checkKeySizeOr :: (Key -> Annex String) -> Key -> FilePath -> ActionItem -> Annex Bool
checkKeySizeOr bad key file ai = case keySize key of
	Nothing -> return True
	Just size -> do
		size' <- liftIO $ getFileSize file
		comparesizes size size'
  where
	comparesizes a b = do
		let same = a == b
		unless same $ badsize a b
		return same
	badsize a b = do
		msg <- bad key
		warning $ concat
			[ actionItemDesc ai
			, ": Bad file size ("
			, compareSizes storageUnits True a b
			, "); "
			, msg
			]

{- Check for keys that are upgradable.
 -
 - Warns and suggests the user migrate, but does not migrate itself,
 - because migration can cause more disk space to be used, and makes
 - worktree changes that need to be committed.
 -}
checkKeyUpgrade :: Backend -> Key -> ActionItem -> AssociatedFile -> Annex Bool
checkKeyUpgrade backend key ai (AssociatedFile (Just file)) =
	case Types.Backend.canUpgradeKey backend of
		Just a | a key -> do
			warning $ concat
				[ actionItemDesc ai
				, ": Can be upgraded to an improved key format. "
				, "You can do so by running: git annex migrate --backend="
				, decodeBS (formatKeyVariety (keyVariety key)) ++ " "
				, file
				]
			return True
		_ -> return True
checkKeyUpgrade _ _ _ (AssociatedFile Nothing) =
	-- Don't suggest migrating without a filename, because
	-- while possible to do, there is no actual benefit from
	-- doing that in this situation.
	return True

{- Runs the backend specific check on a key's content object.
 -
 - When a file is unlocked, it may be a hard link to the object,
 - thus when the user modifies the file, the object will be modified and
 - not pass the check, and we don't want to find an error in this case.
 - So, skip the check if the key is unlocked and modified.
 -}
checkBackend :: Backend -> Key -> KeyStatus -> AssociatedFile -> Annex Bool
checkBackend backend key keystatus afile = do
	content <- calcRepo $ gitAnnexLocation key
	ifM (pure (isKeyUnlockedThin keystatus) <&&> (not <$> isUnmodified key content))
		( nocheck
		, checkBackendOr badContent backend key content ai
		)
  where
	nocheck = return True

	ai = mkActionItem (key, afile)

checkBackendRemote :: Backend -> Key -> Remote -> ActionItem -> FilePath -> Annex Bool
checkBackendRemote backend key remote ai localcopy =
	checkBackendOr (badContentRemote remote localcopy) backend key localcopy ai

checkBackendOr :: (Key -> Annex String) -> Backend -> Key -> FilePath -> ActionItem -> Annex Bool
checkBackendOr bad backend key file ai =
	checkBackendOr' bad backend key file ai (return True)

-- The postcheck action is run after the content is verified,
-- in order to detect situations where the file is changed while being
-- verified.
checkBackendOr' :: (Key -> Annex String) -> Backend -> Key -> FilePath -> ActionItem -> Annex Bool -> Annex Bool
checkBackendOr' bad backend key file ai postcheck =
	case Types.Backend.verifyKeyContent backend of
		Nothing -> return True
		Just verifier -> do
			ok <- verifier key file
			ifM postcheck
				( do
					unless ok $ do
						msg <- bad key
						warning $ concat
							[ actionItemDesc ai
							, ": Bad file content; "
							, msg
							]
					return ok
				, return True
				)

checkKeyNumCopies :: Key -> AssociatedFile -> NumCopies -> Annex Bool
checkKeyNumCopies key afile numcopies = do
	let (desc, hasafile) = case afile of
		AssociatedFile Nothing -> (serializeKey key, False)
		AssociatedFile (Just af) -> (af, True)
	locs <- loggedLocations key
	(untrustedlocations, otherlocations) <- trustPartition UnTrusted locs
	(deadlocations, safelocations) <- trustPartition DeadTrusted otherlocations
	let present = NumCopies (length safelocations)
	if present < numcopies
		then ifM (pure (not hasafile) <&&> checkDead key)
			( do
				showLongNote $ "This key is dead, skipping."
				return True
			, do
				untrusted <- Remote.prettyPrintUUIDs "untrusted" untrustedlocations
				dead <- Remote.prettyPrintUUIDs "dead" deadlocations
				warning $ missingNote desc present numcopies untrusted dead
				when (fromNumCopies present == 0 && not hasafile) $
					showLongNote "(Avoid this check by running: git annex dead --key )"
				return False
			)
		else return True

missingNote :: String -> NumCopies -> NumCopies -> String -> String -> String
missingNote file (NumCopies 0) _ [] dead = 
		"** No known copies exist of " ++ file ++ honorDead dead
missingNote file (NumCopies 0) _ untrusted dead =
		"Only these untrusted locations may have copies of " ++ file ++
		"\n" ++ untrusted ++
		"Back it up to trusted locations with git-annex copy." ++ honorDead dead
missingNote file present needed [] _ =
		"Only " ++ show (fromNumCopies present) ++ " of " ++ show (fromNumCopies needed) ++ 
		" trustworthy copies exist of " ++ file ++
		"\nBack it up with git-annex copy."
missingNote file present needed untrusted dead = 
		missingNote file present needed [] dead ++
		"\nThe following untrusted locations may also have copies: " ++
		"\n" ++ untrusted
	
honorDead :: String -> String
honorDead dead
	| null dead = ""
	| otherwise = "\nThese dead repositories used to have copies\n" ++ dead

{- Bad content is moved aside. -}
badContent :: Key -> Annex String
badContent key = do
	dest <- moveBad key
	return $ "moved to " ++ dest

{- Bad content is dropped from the remote. We have downloaded a copy
 - from the remote to a temp file already (in some cases, it's just a
 - symlink to a file in the remote). To avoid any further data loss,
 - that temp file is moved to the bad content directory unless 
 - the local annex has a copy of the content. -}
badContentRemote :: Remote -> FilePath -> Key -> Annex String
badContentRemote remote localcopy key = do
	bad <- fromRepo gitAnnexBadDir
	let destbad = bad </> keyFile key
	movedbad <- ifM (inAnnex key <||> liftIO (doesFileExist destbad))
		( return False
		, do
			createAnnexDirectory (parentDir destbad)
			liftIO $ catchDefaultIO False $
				ifM (isSymbolicLink <$> getSymbolicLinkStatus localcopy)
					( copyFileExternal CopyTimeStamps localcopy destbad
					, do
						moveFile localcopy destbad
						return True
					)
		)

	dropped <- Remote.removeKey remote key
	when dropped $
		Remote.logStatus remote key InfoMissing
	return $ case (movedbad, dropped) of
		(True, True) -> "moved from " ++ Remote.name remote ++
			" to " ++ destbad
		(False, True) -> "dropped from " ++ Remote.name remote
		(_, False) -> "failed to drop from" ++ Remote.name remote

runFsck :: Incremental -> ActionItem -> Key -> Annex Bool -> CommandStart
runFsck inc ai key a = stopUnless (needFsck inc key) $
	starting "fsck" ai $ do
		ok <- a
		when ok $
			recordFsckTime inc key
		next $ return ok

{- Check if a key needs to be fscked, with support for incremental fscks. -}
needFsck :: Incremental -> Key -> Annex Bool
needFsck (ScheduleIncremental _ _ i) k = needFsck i k
needFsck (ContIncremental h) key = liftIO $ not <$> FsckDb.inDb h key
needFsck _ _ = return True

recordFsckTime :: Incremental -> Key -> Annex ()
recordFsckTime inc key = withFsckDb inc $ \h -> liftIO $ FsckDb.addDb h key

{- Records the start time of an incremental fsck.
 -
 - To guard against time stamp damange (for example, if an annex directory
 - is copied without -a), the fsckstate file contains a time that should
 - be identical to its modification time.
 - (This is not possible to do on Windows, and so the timestamp in
 - the file will only be equal or greater than the modification time.)
 -}
recordStartTime :: UUID -> Annex ()
recordStartTime u = do
	f <- fromRepo (gitAnnexFsckState u)
	createAnnexDirectory $ parentDir f
	liftIO $ nukeFile f
	liftIO $ withFile f WriteMode $ \h -> do
#ifndef mingw32_HOST_OS
		t <- modificationTime <$> getFileStatus f
#else
		t <- getPOSIXTime
#endif
		hPutStr h $ showTime $ realToFrac t
	setAnnexFilePerm f
  where
	showTime :: POSIXTime -> String
	showTime = show

resetStartTime :: UUID -> Annex ()
resetStartTime u = liftIO . nukeFile =<< fromRepo (gitAnnexFsckState u)

{- Gets the incremental fsck start time. -}
getStartTime :: UUID -> Annex (Maybe EpochTime)
getStartTime u = do
	f <- fromRepo (gitAnnexFsckState u)
	liftIO $ catchDefaultIO Nothing $ do
		timestamp <- modificationTime <$> getFileStatus f
		let fromstatus = Just (realToFrac timestamp)
		fromfile <- parsePOSIXTime <$> readFile f
		return $ if matchingtimestamp fromfile fromstatus
			then Just timestamp
			else Nothing
  where
	matchingtimestamp fromfile fromstatus =
#ifndef mingw32_HOST_OS
		fromfile == fromstatus
#else
		fromfile >= fromstatus
#endif

data Incremental
	= NonIncremental
	| ScheduleIncremental Duration UUID Incremental
	| StartIncremental FsckDb.FsckHandle 
	| ContIncremental FsckDb.FsckHandle

prepIncremental :: UUID -> Maybe IncrementalOpt -> Annex Incremental
prepIncremental _ Nothing = pure NonIncremental
prepIncremental u (Just StartIncrementalO) = do
	recordStartTime u
	ifM (FsckDb.newPass u)
		( StartIncremental <$> openFsckDb u
		, giveup "Cannot start a new --incremental fsck pass; another fsck process is already running."
		)
prepIncremental u (Just MoreIncrementalO) =
	ContIncremental <$> openFsckDb u
prepIncremental u (Just (ScheduleIncrementalO delta)) = do
	started <- getStartTime u
	i <- prepIncremental u $ Just $ case started of
		Nothing -> StartIncrementalO
		Just _ -> MoreIncrementalO
	return (ScheduleIncremental delta u i)

cleanupIncremental :: Incremental -> Annex ()
cleanupIncremental (ScheduleIncremental delta u i) = do
	v <- getStartTime u
	case v of
		Nothing -> noop
		Just started -> do
			now <- liftIO getPOSIXTime
			when (now - realToFrac started >= durationToPOSIXTime delta) $
				resetStartTime u
	cleanupIncremental i
cleanupIncremental _ = return ()

openFsckDb :: UUID -> Annex FsckDb.FsckHandle
openFsckDb u = do
	h <- FsckDb.openDb u
	Annex.addCleanup FsckCleanup $
		FsckDb.closeDb h
	return h

withFsckDb :: Incremental -> (FsckDb.FsckHandle -> Annex ()) -> Annex ()
withFsckDb (ContIncremental h) a = a h
withFsckDb (StartIncremental h) a = a h
withFsckDb NonIncremental _ = noop
withFsckDb (ScheduleIncremental _ _ i) a = withFsckDb i a

data KeyStatus
	= KeyMissing
	| KeyPresent
	| KeyUnlockedThin
	-- ^ An annex.thin worktree file is hard linked to the object.
	| KeyLockedThin
	-- ^ The object has hard links, but the file being fscked
	-- is not the one that hard links to it.
	deriving (Show)

isKeyUnlockedThin :: KeyStatus -> Bool
isKeyUnlockedThin KeyUnlockedThin = True
isKeyUnlockedThin KeyLockedThin = False
isKeyUnlockedThin KeyPresent = False
isKeyUnlockedThin KeyMissing = False

getKeyStatus :: Key -> Annex KeyStatus
getKeyStatus key = catchDefaultIO KeyMissing $ do
	afs <- not . null <$> Database.Keys.getAssociatedFiles key
	obj <- calcRepo $ gitAnnexLocation key
	multilink <- ((> 1) . linkCount <$> liftIO (getFileStatus obj))
	return $ if multilink && afs
		then KeyUnlockedThin
		else KeyPresent

getKeyFileStatus :: Key -> FilePath -> Annex KeyStatus
getKeyFileStatus key file = do
	s <- getKeyStatus key
	case s of
		KeyUnlockedThin -> catchDefaultIO KeyUnlockedThin $
			ifM (isJust <$> isAnnexLink file)
				( return KeyLockedThin
				, return KeyUnlockedThin
				)
		_ -> return s

