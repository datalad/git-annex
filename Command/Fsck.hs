{- git-annex command
 -
 - Copyright 2010-2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Command.Fsck where

import Common.Annex
import Command
import qualified Annex
import qualified Remote
import qualified Types.Backend
import qualified Types.Key
import qualified Backend
import Annex.Content
import Annex.Content.Direct
import Annex.Direct
import Annex.Perms
import Annex.Link
import Logs.Location
import Logs.Trust
import Config.NumCopies
import Annex.UUID
import Utility.DataUnits
import Utility.FileMode
import Config
import Types.Key
import Utility.HumanTime
import Git.FilePath
import Utility.PID

import Data.Time.Clock.POSIX
import Data.Time
import System.Posix.Types (EpochTime)
import System.Locale

def :: [Command]
def = [withOptions fsckOptions $ command "fsck" paramPaths seek
	SectionMaintenance "check for problems"]

fsckFromOption :: Option
fsckFromOption = fieldOption ['f'] "from" paramRemote "check remote"

startIncrementalOption :: Option
startIncrementalOption = flagOption ['S'] "incremental" "start an incremental fsck"

moreIncrementalOption :: Option
moreIncrementalOption = flagOption ['m'] "more" "continue an incremental fsck"

incrementalScheduleOption :: Option
incrementalScheduleOption = fieldOption [] "incremental-schedule" paramTime
	"schedule incremental fscking"

fsckOptions :: [Option]
fsckOptions = 
	[ fsckFromOption
	, startIncrementalOption
	, moreIncrementalOption
	, incrementalScheduleOption
	] ++ keyOptions

seek :: CommandSeek
seek ps = do
	from <- getOptionField fsckFromOption Remote.byNameWithUUID
	i <- getIncremental
	withKeyOptions
		(\k -> startKey i k =<< getNumCopies)
		(withFilesInGit $ whenAnnexed $ start from i)
		ps

getIncremental :: Annex Incremental
getIncremental = do
	i <- maybe (return False) (checkschedule . parseDuration)
		=<< Annex.getField (optionName incrementalScheduleOption)
	starti <- Annex.getFlag (optionName startIncrementalOption)
	morei <- Annex.getFlag (optionName moreIncrementalOption)
	case (i, starti, morei) of
		(False, False, False) -> return NonIncremental
		(False, True, False) -> startIncremental
		(False ,False, True) -> ContIncremental <$> getStartTime
		(True, False, False) ->
			maybe startIncremental (return . ContIncremental . Just)
				=<< getStartTime
		_ -> error "Specify only one of --incremental, --more, or --incremental-schedule"
  where
	startIncremental = do
		recordStartTime
		return StartIncremental

	checkschedule Nothing = error "bad --incremental-schedule value"
	checkschedule (Just delta) = do
		Annex.addCleanup "" $ do
			v <- getStartTime
			case v of
				Nothing -> noop
				Just started -> do
					now <- liftIO getPOSIXTime
					when (now - realToFrac started >= durationToPOSIXTime delta)
						resetStartTime
		return True

start :: Maybe Remote -> Incremental -> FilePath -> (Key, Backend) -> CommandStart
start from inc file (key, backend) = do
	numcopies <- getFileNumCopies file
	case from of
		Nothing -> go $ perform key file backend numcopies
		Just r -> go $ performRemote key file backend numcopies r
  where
	go = runFsck inc file key

perform :: Key -> FilePath -> Backend -> NumCopies -> Annex Bool
perform key file backend numcopies = check
	-- order matters
	[ fixLink key file
	, verifyLocationLog key file
	, verifyDirectMapping key file
	, verifyDirectMode key file
	, checkKeySize key
	, checkBackend backend key (Just file)
	, checkKeyNumCopies key file numcopies
	]

{- To fsck a remote, the content is retrieved to a tmp file,
 - and checked locally. -}
performRemote :: Key -> FilePath -> Backend -> NumCopies -> Remote -> Annex Bool
performRemote key file backend numcopies remote =
	dispatch =<< Remote.hasKey remote key
  where
	dispatch (Left err) = do
		showNote err
		return False
	dispatch (Right True) = withtmp $ \tmpfile ->
		ifM (getfile tmpfile)
			( go True (Just tmpfile)
			, go True Nothing
			)
	dispatch (Right False) = go False Nothing
	go present localcopy = check
		[ verifyLocationLogRemote key file remote present
		, checkKeySizeRemote key remote localcopy
		, checkBackendRemote backend key remote localcopy
		, checkKeyNumCopies key file numcopies
		]
	withtmp a = do
		pid <- liftIO getPID
		t <- fromRepo gitAnnexTmpDir
		createAnnexDirectory t
		let tmp = t </> "fsck" ++ show pid ++ "." ++ keyFile key
		let cleanup = liftIO $ catchIO (removeFile tmp) (const noop)
		cleanup
		cleanup `after` a tmp
	getfile tmp =
		ifM (Remote.retrieveKeyFileCheap remote key tmp)
			( return True
			, ifM (Annex.getState Annex.fast)
				( return False
				, Remote.retrieveKeyFile remote key Nothing tmp dummymeter
				)
			)
	dummymeter _ = noop

startKey :: Incremental -> Key -> NumCopies -> CommandStart
startKey inc key numcopies =
	case Backend.maybeLookupBackendName (Types.Key.keyBackendName key) of
		Nothing -> stop
		Just backend -> runFsck inc (key2file key) key $
			performKey key backend numcopies

performKey :: Key -> Backend -> NumCopies -> Annex Bool
performKey key backend numcopies = check
	[ verifyLocationLog key (key2file key)
	, checkKeySize key
	, checkBackend backend key Nothing
	, checkKeyNumCopies key (key2file key) numcopies
	]

check :: [Annex Bool] -> Annex Bool
check cs = and <$> sequence cs

{- Checks that the file's link points correctly to the content.
 -
 - In direct mode, there is only a link when the content is not present.
 -}
fixLink :: Key -> FilePath -> Annex Bool
fixLink key file = do
	want <- inRepo $ gitAnnexLink file key
	have <- getAnnexLinkTarget file
	maybe noop (go want) have
	return True
  where
	go want have
		| want /= fromInternalGitPath have = do
			showNote "fixing link"
			liftIO $ createDirectoryIfMissing True (parentDir file)
			liftIO $ removeFile file
			addAnnexLink want file
		| otherwise = noop

{- Checks that the location log reflects the current status of the key,
 - in this repository only. -}
verifyLocationLog :: Key -> String -> Annex Bool
verifyLocationLog key desc = do
	present <- inAnnex key
	direct <- isDirect
	u <- getUUID
	
	{- Since we're checking that a key's file is present, throw
	 - in a permission fixup here too. -}
	file <- calcRepo $ gitAnnexLocation key
	when (present && not direct) $
		freezeContent file
	whenM (liftIO $ doesDirectoryExist $ parentDir file) $
		freezeContentDir file

	{- In direct mode, modified files will show up as not present,
	 - but that is expected and not something to do anything about. -}
	if direct && not present
		then return True
		else verifyLocationLog' key desc present u (logChange key u)

verifyLocationLogRemote :: Key -> String -> Remote -> Bool -> Annex Bool
verifyLocationLogRemote key desc remote present =
	verifyLocationLog' key desc present (Remote.uuid remote)
		(Remote.logStatus remote key)

verifyLocationLog' :: Key -> String -> Bool -> UUID -> (LogStatus -> Annex ()) -> Annex Bool
verifyLocationLog' key desc present u updatestatus = do
	uuids <- Remote.keyLocations key
	case (present, u `elem` uuids) of
		(True, False) -> do
				fix InfoPresent
				-- There is no data loss, so do not fail.
				return True
		(False, True) -> do
				fix InfoMissing
				warning $
					"** Based on the location log, " ++ desc
					++ "\n** was expected to be present, " ++
					"but its content is missing."
				return False
		_ -> return True
  where
	fix s = do
		showNote "fixing location log"
		updatestatus s

{- Ensures the direct mode mapping file is consistent. Each file
 - it lists for the key should exist, and the specified file should be
 - included in it.
 -}
verifyDirectMapping :: Key -> FilePath -> Annex Bool
verifyDirectMapping key file = do
	whenM isDirect $ do
		fs <- addAssociatedFile key file
		forM_ fs $ \f -> 
			unlessM (liftIO $ doesFileExist f) $
				void $ removeAssociatedFile key f
	return True

{- Ensures that files whose content is available are in direct mode. -}
verifyDirectMode :: Key -> FilePath -> Annex Bool
verifyDirectMode key file = do
	whenM (isDirect <&&> isJust <$> isAnnexLink file) $ do
		v <- toDirectGen key file
		case v of
			Nothing -> noop
			Just a -> do
				showNote "fixing direct mode"
				a
	return True

{- The size of the data for a key is checked against the size encoded in
 - the key's metadata, if available.
 -
 - Not checked in direct mode, because files can be changed directly.
  -}
checkKeySize :: Key -> Annex Bool
checkKeySize key = ifM isDirect
	( return True
	, do
		file <- calcRepo $ gitAnnexLocation key
		ifM (liftIO $ doesFileExist file)
			( checkKeySizeOr badContent key file
			, return True
			)
	)

checkKeySizeRemote :: Key -> Remote -> Maybe FilePath -> Annex Bool
checkKeySizeRemote _ _ Nothing = return True
checkKeySizeRemote key remote (Just file) =
	checkKeySizeOr (badContentRemote remote) key file

checkKeySizeOr :: (Key -> Annex String) -> Key -> FilePath -> Annex Bool
checkKeySizeOr bad key file = case Types.Key.keySize key of
	Nothing -> return True
	Just size -> do
		size' <- fromIntegral . fileSize
			<$> liftIO (getFileStatus file)
		comparesizes size size'
  where
	comparesizes a b = do
		let same = a == b
		unless same $ badsize a b
		return same
	badsize a b = do
		msg <- bad key
		warning $ concat
			[ "Bad file size ("
			, compareSizes storageUnits True a b
			, "); "
			, msg
			]

{- Runs the backend specific check on a key's content.
 -
 - In direct mode this is not done if the file has clearly been modified,
 - because modification of direct mode files is allowed. It's still done
 - if the file does not appear modified, to catch disk corruption, etc.
 -}
checkBackend :: Backend -> Key -> Maybe FilePath -> Annex Bool
checkBackend backend key mfile = go =<< isDirect
  where
  	go False = do
		content <- calcRepo $ gitAnnexLocation key
		checkBackendOr badContent backend key content
	go True = maybe nocheck checkdirect mfile
	checkdirect file = ifM (goodContent key file)
		( checkBackendOr' (badContentDirect file) backend key file
			(goodContent key file)
		, nocheck
		)
	nocheck = return True

checkBackendRemote :: Backend -> Key -> Remote -> Maybe FilePath -> Annex Bool
checkBackendRemote backend key remote = maybe (return True) go
  where
	go = checkBackendOr (badContentRemote remote) backend key

checkBackendOr :: (Key -> Annex String) -> Backend -> Key -> FilePath -> Annex Bool
checkBackendOr bad backend key file =
	checkBackendOr' bad backend key file (return True)

checkBackendOr' :: (Key -> Annex String) -> Backend -> Key -> FilePath -> Annex Bool -> Annex Bool
checkBackendOr' bad backend key file postcheck =
	case Types.Backend.fsckKey backend of
		Nothing -> return True
		Just a -> do
			ok <- a key file
			ifM postcheck
				( do
					unless ok $ do
						msg <- bad key
						warning $ "Bad file content; " ++ msg
					return ok
				, return True
				)

checkKeyNumCopies :: Key -> String -> NumCopies -> Annex Bool
checkKeyNumCopies key file numcopies = do
	(untrustedlocations, safelocations) <- trustPartition UnTrusted =<< Remote.keyLocations key
	let present = NumCopies (length safelocations)
	if present < numcopies
		then do
			ppuuids <- Remote.prettyPrintUUIDs "untrusted" untrustedlocations
			warning $ missingNote file present numcopies ppuuids
			return False
		else return True

missingNote :: String -> NumCopies -> NumCopies -> String -> String
missingNote file (NumCopies 0) _ [] = 
		"** No known copies exist of " ++ file
missingNote file (NumCopies 0) _ untrusted =
		"Only these untrusted locations may have copies of " ++ file ++
		"\n" ++ untrusted ++
		"Back it up to trusted locations with git-annex copy."
missingNote file present needed [] =
		"Only " ++ show (fromNumCopies present) ++ " of " ++ show (fromNumCopies needed) ++ 
		" trustworthy copies exist of " ++ file ++
		"\nBack it up with git-annex copy."
missingNote file present needed untrusted = 
		missingNote file present needed [] ++
		"\nThe following untrusted locations may also have copies: " ++
		"\n" ++ untrusted

{- Bad content is moved aside. -}
badContent :: Key -> Annex String
badContent key = do
	dest <- moveBad key
	return $ "moved to " ++ dest

{- Bad content is left where it is, but we touch the file, so it'll be
 - committed to a new key. -}
badContentDirect :: FilePath -> Key -> Annex String
badContentDirect file key = do
	void $ liftIO $ catchMaybeIO $ touchFile file
	logStatus key InfoMissing
	return "left in place for you to examine"

badContentRemote :: Remote -> Key -> Annex String
badContentRemote remote key = do
	ok <- Remote.removeKey remote key
	when ok $
		Remote.logStatus remote key InfoMissing
	return $ (if ok then "dropped from " else "failed to drop from ")
		++ Remote.name remote

data Incremental = StartIncremental | ContIncremental (Maybe EpochTime) | NonIncremental
	deriving (Eq, Show)

runFsck :: Incremental -> FilePath -> Key -> Annex Bool -> CommandStart
runFsck inc file key a = ifM (needFsck inc key)
	( do
		showStart "fsck" file
		next $ do
			ok <- a
			when ok $
				recordFsckTime key
			next $ return ok
	, stop
	)

{- Check if a key needs to be fscked, with support for incremental fscks. -}
needFsck :: Incremental -> Key -> Annex Bool
needFsck (ContIncremental Nothing) _ = return True
needFsck (ContIncremental starttime) key = do
	fscktime <- getFsckTime key
	return $ fscktime < starttime
needFsck _ _ = return True

{- To record the time that a key was last fscked, without
 - modifying its mtime, we set the timestamp of its parent directory.
 - Each annexed file is the only thing in its directory, so this is fine.
 -
 - To record that the file was fscked, the directory's sticky bit is set.
 - (None of the normal unix behaviors of the sticky bit should matter, so
 - we can reuse this permission bit.)
 -
 - Note that this relies on the parent directory being deleted when a file
 - is dropped. That way, if it's later added back, the fsck record
 - won't still be present.
 -}
recordFsckTime :: Key -> Annex ()
recordFsckTime key = do
	parent <- parentDir <$> calcRepo (gitAnnexLocation key)
	liftIO $ void $ tryIO $ do
		touchFile parent
#ifndef mingw32_HOST_OS
		setSticky parent
#endif

getFsckTime :: Key -> Annex (Maybe EpochTime)
getFsckTime key = do
	parent <- parentDir <$> calcRepo (gitAnnexLocation key)
	liftIO $ catchDefaultIO Nothing $ do
		s <- getFileStatus parent
		return $ if isSticky $ fileMode s
			then Just $ modificationTime s
			else Nothing

{- Records the start time of an incremental fsck.
 -
 - To guard against time stamp damange (for example, if an annex directory
 - is copied without -a), the fsckstate file contains a time that should
 - be identical to its modification time.
 - (This is not possible to do on Windows, and so the timestamp in
 - the file will only be equal or greater than the modification time.)
 -}
recordStartTime :: Annex ()
recordStartTime = do
	f <- fromRepo gitAnnexFsckState
	createAnnexDirectory $ parentDir f
	liftIO $ do
		nukeFile f
		withFile f WriteMode $ \h -> do
#ifndef mingw32_HOST_OS
			t <- modificationTime <$> getFileStatus f
#else
			t <- getPOSIXTime
#endif
			hPutStr h $ showTime $ realToFrac t
  where
	showTime :: POSIXTime -> String
	showTime = show

resetStartTime :: Annex ()
resetStartTime = liftIO . nukeFile =<< fromRepo gitAnnexFsckState

{- Gets the incremental fsck start time. -}
getStartTime :: Annex (Maybe EpochTime)
getStartTime = do
	f <- fromRepo gitAnnexFsckState
	liftIO $ catchDefaultIO Nothing $ do
		timestamp <- modificationTime <$> getFileStatus f
		let fromstatus = Just (realToFrac timestamp)
		fromfile <- readishTime <$> readFile f
		return $ if matchingtimestamp fromfile fromstatus
			then Just timestamp
			else Nothing
  where
	readishTime :: String -> Maybe POSIXTime
	readishTime s = utcTimeToPOSIXSeconds <$>
		parseTime defaultTimeLocale "%s%Qs" s
	matchingtimestamp fromfile fromstatus =
#ifndef mingw32_HOST_OS
		fromfile == fromstatus
#else
		fromfile >= fromstatus
#endif
