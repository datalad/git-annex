{- git-annex command
 -
 - Copyright 2010-2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Fsck where

import Common.Annex
import Command
import qualified Annex
import qualified Annex.Queue
import qualified Remote
import qualified Types.Backend
import qualified Types.Key
import qualified Backend
import Annex.Content
import Annex.Perms
import Logs.Location
import Logs.Trust
import Annex.UUID
import Utility.DataUnits
import Utility.FileMode
import Config
import qualified Option
import Types.Key

import System.Posix.Process (getProcessID)
import Data.Time.Clock.POSIX
import Data.Time
import System.Posix.Types (EpochTime)
import System.Locale

def :: [Command]
def = [withOptions options $ command "fsck" paramPaths seek
	"check for problems"]

fromOption :: Option
fromOption = Option.field ['f'] "from" paramRemote "check remote"

startIncrementalOption :: Option
startIncrementalOption = Option.flag ['S'] "incremental" "start an incremental fsck"

incrementalOption :: Option
incrementalOption = Option.flag ['m'] "more" "continue an incremental fsck"

options :: [Option]
options = [fromOption, startIncrementalOption, incrementalOption]

seek :: [CommandSeek]
seek =
	[ withField fromOption Remote.byName $ \from ->
	  withIncremental $ \i -> withFilesInGit $ whenAnnexed $ start from i
	, withIncremental $ \i -> withBarePresentKeys $ startBare i
	]

withIncremental :: (Incremental -> CommandSeek) -> CommandSeek
withIncremental a = withFlag startIncrementalOption $ \startincremental ->
	withFlag incrementalOption $ \incremental ->
		a $ case (startincremental, incremental) of
			(False, False) -> NonIncremental
			(True, _) -> StartIncremental
			(False, True) -> ContIncremental

start :: Maybe Remote -> Incremental -> FilePath -> (Key, Backend) -> CommandStart
start from inc file (key, backend) = do
	numcopies <- numCopies file
	case from of
		Nothing -> go $ perform key file backend numcopies
		Just r -> go $ performRemote key file backend numcopies r
	where
		go = runFsck inc file key

perform :: Key -> FilePath -> Backend -> Maybe Int -> Annex Bool
perform key file backend numcopies = check
	-- order matters
	[ fixLink key file
	, verifyLocationLog key file
	, checkKeySize key
	, checkBackend backend key
	, checkKeyNumCopies key file numcopies
	]

{- To fsck a remote, the content is retrieved to a tmp file,
 - and checked locally. -}
performRemote :: Key -> FilePath -> Backend -> Maybe Int -> Remote -> Annex Bool
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
			pid <- liftIO getProcessID
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
					, Remote.retrieveKeyFile remote key Nothing tmp
					)
				)

{- To fsck a bare repository, fsck each key in the location log. -}
withBarePresentKeys :: (Key -> CommandStart) -> CommandSeek
withBarePresentKeys a params = isBareRepo >>= go
	where
		go False = return []
		go True = do
			unless (null params) $
				error "fsck should be run without parameters in a bare repository"
			map a <$> loggedKeys

startBare :: Incremental -> Key -> CommandStart
startBare inc key = case Backend.maybeLookupBackendName (Types.Key.keyBackendName key) of
	Nothing -> stop
	Just backend -> runFsck inc (key2file key) key $ performBare key backend

{- Note that numcopies cannot be checked in a bare repository, because
 - getting the numcopies value requires a working copy with .gitattributes
 - files. -}
performBare :: Key -> Backend -> Annex Bool
performBare key backend = check
	[ verifyLocationLog key (key2file key)
	, checkKeySize key
	, checkBackend backend key
	]

check :: [Annex Bool] -> Annex Bool
check cs = all id <$> sequence cs

{- Checks that the file's symlink points correctly to the content. -}
fixLink :: Key -> FilePath -> Annex Bool
fixLink key file = do
	want <- calcGitLink file key
	have <- liftIO $ readSymbolicLink file
	when (want /= have) $ do
		{- Version 3.20120227 had a bug that could cause content
		 - to be stored in the wrong hash directory. Clean up
		 - after the bug by moving the content.
		 -}
		whenM (liftIO $ doesFileExist file) $
			unlessM (inAnnex key) $ do
				showNote "fixing content location"
				dir <- liftIO $ parentDir <$> absPath file
				let content = absPathFrom dir have
				liftIO $ allowWrite (parentDir content)
				moveAnnex key content

		showNote "fixing link"
		liftIO $ createDirectoryIfMissing True (parentDir file)
		liftIO $ removeFile file
		liftIO $ createSymbolicLink want file
		Annex.Queue.addCommand "add" [Param "--force", Param "--"] [file]
	return True

{- Checks that the location log reflects the current status of the key,
   in this repository only. -}
verifyLocationLog :: Key -> String -> Annex Bool
verifyLocationLog key desc = do
	present <- inAnnex key
	
	-- Since we're checking that a key's file is present, throw
	-- in a permission fixup here too.
	when present $ do
		file <- inRepo $ gitAnnexLocation key
		freezeContent file
		freezeContentDir file

	u <- getUUID
	verifyLocationLog' key desc present u (logChange key u)

verifyLocationLogRemote :: Key -> String -> Remote -> Bool -> Annex Bool
verifyLocationLogRemote key desc remote present =
	verifyLocationLog' key desc present (Remote.uuid remote)
		(Remote.logStatus remote key)

verifyLocationLog' :: Key -> String -> Bool -> UUID -> (LogStatus -> Annex ()) -> Annex Bool
verifyLocationLog' key desc present u bad = do
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
			bad s

{- The size of the data for a key is checked against the size encoded in
 - the key's metadata, if available. -}
checkKeySize :: Key -> Annex Bool
checkKeySize key = do
	file <- inRepo $ gitAnnexLocation key
	ifM (liftIO $ doesFileExist file)
		( checkKeySizeOr badContent key file
		, return True
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

checkBackend :: Backend -> Key -> Annex Bool
checkBackend backend key = do
	file <- inRepo (gitAnnexLocation key)
	checkBackendOr badContent backend key file

checkBackendRemote :: Backend -> Key -> Remote -> Maybe FilePath -> Annex Bool
checkBackendRemote backend key remote = maybe (return True) go
	where
		go = checkBackendOr (badContentRemote remote) backend key

checkBackendOr :: (Key -> Annex String) -> Backend -> Key -> FilePath -> Annex Bool
checkBackendOr bad backend key file =
	case Types.Backend.fsckKey backend of
		Nothing -> return True
		Just a -> do
			ok <- a key file
			unless ok $ do
				msg <- bad key
				warning $ "Bad file content; " ++ msg
			return ok

checkKeyNumCopies :: Key -> FilePath -> Maybe Int -> Annex Bool
checkKeyNumCopies key file numcopies = do
	needed <- getNumCopies numcopies
	(untrustedlocations, safelocations) <- trustPartition UnTrusted =<< Remote.keyLocations key
	let present = length safelocations
	if present < needed
		then do
			ppuuids <- Remote.prettyPrintUUIDs "untrusted" untrustedlocations
			warning $ missingNote file present needed ppuuids
			return False
		else return True

missingNote :: String -> Int -> Int -> String -> String
missingNote file 0 _ [] = 
		"** No known copies exist of " ++ file
missingNote file 0 _ untrusted =
		"Only these untrusted locations may have copies of " ++ file ++
		"\n" ++ untrusted ++
		"Back it up to trusted locations with git-annex copy."
missingNote file present needed [] =
		"Only " ++ show present ++ " of " ++ show needed ++ 
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

badContentRemote :: Remote -> Key -> Annex String
badContentRemote remote key = do
	ok <- Remote.removeKey remote key
	-- better safe than sorry: assume the remote dropped the key
	-- even if it seemed to fail; the failure could have occurred
	-- after it really dropped it
	Remote.logStatus remote key InfoMissing
	return $ (if ok then "dropped from " else "failed to drop from ")
		++ Remote.name remote

data Incremental = StartIncremental | ContIncremental | NonIncremental
	deriving (Eq)

runFsck :: Incremental -> FilePath -> Key -> Annex Bool -> CommandStart
runFsck inc file key a = do
	starttime <- getstart
	ifM (needFsck inc starttime key)
		( do
			showStart "fsck" file
			next $ do
				ok <- a
				when ok $
					recordFsckTime key
				next $ return ok
		, stop
		)
	where
		getstart
			| inc == StartIncremental = Just <$> recordStartTime
			| inc == ContIncremental = getStartTime
			| otherwise = return Nothing

{- Check if a key needs to be fscked, with support for incremental fscks. -}
needFsck :: Incremental -> Maybe EpochTime -> Key -> Annex Bool
needFsck ContIncremental Nothing _ = return True
needFsck ContIncremental starttime key = do
	fscktime <- getFsckTime key
	return $ fscktime < starttime
needFsck _ _ _ = return True

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
	parent <- parentDir <$> inRepo (gitAnnexLocation key)
	liftIO $ void $ tryIO $ do
		touchFile parent
		setSticky parent

getFsckTime :: Key -> Annex (Maybe EpochTime)
getFsckTime key = do
	parent <- parentDir <$> inRepo (gitAnnexLocation key)
	liftIO $ catchDefaultIO Nothing $ do
		s <- getFileStatus parent
		return $ if isSticky $ fileMode s
			then Just $ modificationTime s
			else Nothing

{- Records the start time of an interactive fsck, also returning it.
 -
 - To guard against time stamp damange (for example, if an annex directory
 - is copied without -a), the fsckstate file contains a time that should
 - be identical to its modification time. -}
recordStartTime :: Annex (EpochTime)
recordStartTime = do
	f <- fromRepo gitAnnexFsckState
	createAnnexDirectory $ parentDir f
	liftIO $ do
		nukeFile f
		h <- openFile f WriteMode
		t <- modificationTime <$> getFileStatus f
		hPutStr h $ showTime $ realToFrac t
		hClose h
		return t
	where
		showTime :: POSIXTime -> String
		showTime = show

{- Gets the incremental fsck start time. -}
getStartTime :: Annex (Maybe EpochTime)
getStartTime = do
	f <- fromRepo gitAnnexFsckState
	liftIO $ catchDefaultIO Nothing $ do
		timestamp <- modificationTime <$> getFileStatus f
		t <- readishTime <$> readFile f
		return $ if Just (realToFrac timestamp) == t
			then Just timestamp
			else Nothing
	where
		readishTime :: String -> Maybe POSIXTime
		readishTime s = utcTimeToPOSIXSeconds <$>
			parseTime defaultTimeLocale "%s%Qs" s
