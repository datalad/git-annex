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
import qualified Remote
import qualified Types.Backend
import qualified Types.Key
import qualified Backend
import Annex.Content
import Logs.Location
import Logs.Trust
import Annex.UUID
import Utility.DataUnits
import Utility.FileMode
import Config
import qualified Option

def :: [Command]
def = [withOptions options $ command "fsck" paramPaths seek
	"check for problems"]

fromOption :: Option
fromOption = Option.field ['f'] "from" paramRemote "check remote"

options :: [Option]
options = [fromOption]

seek :: [CommandSeek]
seek =
	[ withField fromOption Remote.byName $ \from ->
		withFilesInGit $ whenAnnexed $ start from
	, withBarePresentKeys startBare
	]

start :: Maybe Remote -> FilePath -> (Key, Backend) -> CommandStart
start from file (key, backend) = do
	numcopies <- numCopies file
	showStart "fsck" file
	case from of
		Nothing -> next $ perform key file backend numcopies
		Just r -> next $ performRemote key file backend numcopies r

perform :: Key -> FilePath -> Backend -> Maybe Int -> CommandPerform
perform key file backend numcopies = check
	-- order matters
	[ verifyLocationLog key file
	, checkKeySize key
	, checkBackend backend key
	, checkKeyNumCopies key file numcopies
	]

{- To fsck a remote, the content is retrieved to a tmp file,
 - and checked locally. -}
performRemote :: Key -> FilePath -> Backend -> Maybe Int -> Remote -> CommandPerform
performRemote key file backend numcopies remote = do
	v <- Remote.hasKey remote key
	case v of
		Left err -> do
			showNote err
			stop
		Right True -> withtmp $ \tmpfile -> do
			copied <- getfile tmpfile
			if copied then go True (Just tmpfile) else go True Nothing
		Right False -> go False Nothing
	where
		go present localcopy = check
			[ verifyLocationLogRemote key file remote present
			, checkKeySizeRemote key remote localcopy
			, checkBackendRemote backend key remote localcopy
			, checkKeyNumCopies key file numcopies
			]
		withtmp a = do
			pid <- liftIO getProcessID
			t <- fromRepo gitAnnexTmpDir
			let tmp = t </> "fsck" ++ show pid ++ "." ++ keyFile key
			liftIO $ createDirectoryIfMissing True t
			let cleanup = liftIO $ catchIO (removeFile tmp) (const $ return ())
			cleanup
			cleanup `after` a tmp
		getfile tmp = do
			ok <- Remote.retrieveKeyFileCheap remote key tmp
			if ok
				then return ok
				else do
					fast <- Annex.getState Annex.fast
					if fast
						then return False
						else Remote.retrieveKeyFile remote key tmp

{- To fsck a bare repository, fsck each key in the location log. -}
withBarePresentKeys :: (Key -> CommandStart) -> CommandSeek
withBarePresentKeys a params = isBareRepo >>= go
	where
		go False = return []
		go True = do
			unless (null params) $
				error "fsck should be run without parameters in a bare repository"
			map a <$> loggedKeys

startBare :: Key -> CommandStart
startBare key = case Backend.maybeLookupBackendName (Types.Key.keyBackendName key) of
	Nothing -> stop
	Just backend -> do
		showStart "fsck" (show key)
		next $ performBare key backend

{- Note that numcopies cannot be checked in a bare repository, because
 - getting the numcopies value requires a working copy with .gitattributes
 - files. -}
performBare :: Key -> Backend -> CommandPerform
performBare key backend = check
	[ verifyLocationLog key (show key)
	, checkKeySize key
	, checkBackend backend key
	]

check :: [Annex Bool] -> CommandPerform	
check = sequence >=> dispatch
	where
		dispatch vs
			| all (== True) vs = next $ return True
			| otherwise = stop

{- Checks that the location log reflects the current status of the key,
   in this repository only. -}
verifyLocationLog :: Key -> String -> Annex Bool
verifyLocationLog key desc = do
	present <- inAnnex key
	
	-- Since we're checking that a key's file is present, throw
	-- in a permission fixup here too.
	when present $ do
		f <- inRepo $ gitAnnexLocation key
		liftIO $ do
			preventWrite f
			preventWrite (parentDir f)

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
	present <- liftIO $ doesFileExist file
	if present
		then checkKeySize' key file badContent
		else return True

checkKeySizeRemote :: Key -> Remote -> Maybe FilePath -> Annex Bool
checkKeySizeRemote _ _ Nothing = return True
checkKeySizeRemote key remote (Just file) = checkKeySize' key file
	(badContentRemote remote)

checkKeySize' :: Key -> FilePath -> (Key -> Annex String) -> Annex Bool
checkKeySize' key file bad = case Types.Key.keySize key of
	Nothing -> return True
	Just size -> do
		stat <- liftIO $ getFileStatus file
		let size' = fromIntegral (fileSize stat)
		if size == size'
			then return True
			else do
				msg <- bad key
				warning $ "Bad file size (" ++
					compareSizes storageUnits True size size' ++
					"); " ++ msg
				return False

checkBackend :: Backend -> Key -> Annex Bool
checkBackend backend key = do
	file <- inRepo (gitAnnexLocation key)
	checkBackend' backend key (Just file) badContent

checkBackendRemote :: Backend -> Key -> Remote -> Maybe FilePath -> Annex Bool
checkBackendRemote backend key remote localcopy =
	checkBackend' backend key localcopy (badContentRemote remote)

checkBackend' :: Backend -> Key -> Maybe FilePath -> (Key -> Annex String) -> Annex Bool
checkBackend' _ _ Nothing _ = return True
checkBackend' backend key (Just file) bad = case Types.Backend.fsckKey backend of
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
