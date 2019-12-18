{- git-annex file content managing
 -
 - Copyright 2010-2019 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Annex.Content (
	inAnnex,
	inAnnex',
	inAnnexSafe,
	inAnnexCheck,
	objectFileExists,
	lockContentShared,
	lockContentForRemoval,
	ContentRemovalLock,
	RetrievalSecurityPolicy(..),
	getViaTmp,
	getViaTmpFromDisk,
	checkDiskSpaceToGet,
	prepTmp,
	withTmp,
	checkDiskSpace,
	needMoreDiskSpace,
	moveAnnex,
	populatePointerFile,
	linkToAnnex,
	linkFromAnnex,
	LinkAnnexResult(..),
	unlinkAnnex,
	checkedCopyFile,
	linkOrCopy,
	linkOrCopy',
	sendAnnex,
	prepSendAnnex,
	removeAnnex,
	moveBad,
	KeyLocation(..),
	listKeys,
	saveState,
	downloadUrl,
	preseedTmp,
	dirKeys,
	withObjectLoc,
	staleKeysPrune,
	pruneTmpWorkDirBefore,
	isUnmodified,
	isUnmodifiedCheap,
	verifyKeyContent,
	VerifyConfig(..),
	Verification(..),
	unVerified,
	withTmpWorkDir,
) where

import System.IO.Unsafe (unsafeInterleaveIO)
import qualified Data.Set as S

import Annex.Common
import Logs.Location
import Types.Transfer
import Logs.Transfer
import qualified Git
import qualified Annex
import qualified Annex.Queue
import qualified Annex.Branch
import Utility.FileMode
import qualified Annex.Url as Url
import Utility.CopyFile
import Utility.Metered
import Git.FilePath
import Annex.Perms
import Annex.Link
import Annex.LockPool
import Messages.Progress
import Types.Remote (unVerified, Verification(..), RetrievalSecurityPolicy(..))
import qualified Types.Remote
import qualified Types.Backend
import qualified Backend
import qualified Database.Keys
import Types.NumCopies
import Types.Key
import Annex.UUID
import Annex.InodeSentinal
import Utility.InodeCache
import Annex.Content.LowLevel
import Annex.Content.PointerFile
import Annex.Concurrent
import Types.WorkerPool
import qualified Utility.RawFilePath as R

{- Checks if a given key's content is currently present. -}
inAnnex :: Key -> Annex Bool
inAnnex key = inAnnexCheck key $ liftIO . R.doesPathExist

{- Runs an arbitrary check on a key's content. -}
inAnnexCheck :: Key -> (RawFilePath -> Annex Bool) -> Annex Bool
inAnnexCheck key check = inAnnex' id False check key

{- inAnnex that performs an arbitrary check of the key's content. -}
inAnnex' :: (a -> Bool) -> a -> (RawFilePath -> Annex a) -> Key -> Annex a
inAnnex' isgood bad check key = withObjectLoc key $ \loc -> do
	r <- check loc
	if isgood r
		then ifM (annexThin <$> Annex.getGitConfig)
			-- When annex.thin is set, the object file
			-- could be modified; make sure it's not.
			-- (Suppress any messages about
			-- checksumming, to avoid them cluttering
			-- the display.)
			( ifM (doQuietAction $ isUnmodified key loc)
				( return r
				, return bad
				)
			, return r
			)
		else return bad

{- Like inAnnex, checks if the object file for a key exists,
 - but there are no guarantees it has the right content. -}
objectFileExists :: Key -> Annex Bool
objectFileExists key =
	calcRepo (gitAnnexLocation key)
		>>= liftIO . R.doesPathExist

{- A safer check; the key's content must not only be present, but
 - is not in the process of being removed. -}
inAnnexSafe :: Key -> Annex (Maybe Bool)
inAnnexSafe key = 
	inAnnex' (fromMaybe True) (Just False) (go . fromRawFilePath) key
  where
	is_locked = Nothing
	is_unlocked = Just True
	is_missing = Just False

	go contentfile = flip checklock contentfile =<< contentLockFile key

#ifndef mingw32_HOST_OS
	checklock Nothing contentfile = checkOr is_missing contentfile
	{- The content file must exist, but the lock file generally
	 - won't exist unless a removal is in process. -}
	checklock (Just lockfile) contentfile =
		ifM (liftIO $ doesFileExist contentfile)
			( checkOr is_unlocked lockfile
			, return is_missing
			)
	checkOr d lockfile = checkLocked lockfile >>= return . \case
		Nothing -> d
		Just True -> is_locked
		Just False -> is_unlocked
#else
	checklock Nothing contentfile = liftIO $ ifM (doesFileExist contentfile)
		( lockShared contentfile >>= \case
			Nothing -> return is_locked
			Just lockhandle -> do
				dropLock lockhandle
				return is_unlocked
		, return is_missing
		)
	{- In Windows, see if we can take a shared lock. If so, 
	 - remove the lock file to clean up after ourselves. -}
	checklock (Just lockfile) contentfile =
		ifM (liftIO $ doesFileExist contentfile)
			( modifyContent lockfile $ liftIO $
				lockShared lockfile >>= \case
					Nothing -> return is_locked
					Just lockhandle -> do
						dropLock lockhandle
						void $ tryIO $ nukeFile lockfile
						return is_unlocked
			, return is_missing
			)
#endif

{- Windows has to use a separate lock file from the content, since
 - locking the actual content file would interfere with the user's
 - use of it. -}
contentLockFile :: Key -> Annex (Maybe FilePath)
#ifndef mingw32_HOST_OS
contentLockFile _ = pure Nothing
#else
contentLockFile key = Just <$> calcRepo (gitAnnexContentLock key)
#endif

{- Prevents the content from being removed while the action is running.
 - Uses a shared lock.
 -
 - If locking fails, or the content is not present, throws an exception
 - rather than running the action.
 -}
lockContentShared :: Key -> (VerifiedCopy -> Annex a) -> Annex a
lockContentShared key a = lockContentUsing lock key $ ifM (inAnnex key)
	( do
		u <- getUUID
		withVerifiedCopy LockedCopy u (return True) a
	, giveup $ "failed to lock content: not present"
	)
  where
#ifndef mingw32_HOST_OS
	lock contentfile Nothing = tryLockShared Nothing contentfile
	lock _ (Just lockfile) = posixLocker tryLockShared lockfile
#else
	lock = winLocker lockShared
#endif

{- Exclusively locks content, while performing an action that
 - might remove it.
 -
 - If locking fails, throws an exception rather than running the action.
 -}
lockContentForRemoval :: Key -> (ContentRemovalLock -> Annex a) -> Annex a
lockContentForRemoval key a = lockContentUsing lock key $ 
	a (ContentRemovalLock key)
  where
#ifndef mingw32_HOST_OS
	{- Since content files are stored with the write bit disabled, have
	 - to fiddle with permissions to open for an exclusive lock. -}
	lock contentfile Nothing = bracket_
		(thawContent contentfile)
		(freezeContent contentfile)
		(tryLockExclusive Nothing contentfile)
	lock _ (Just lockfile) = posixLocker tryLockExclusive lockfile
#else
	lock = winLocker lockExclusive
#endif

{- Passed the object content file, and maybe a separate lock file to use,
 - when the content file itself should not be locked. -}
type ContentLocker = FilePath -> Maybe LockFile -> Annex (Maybe LockHandle)

#ifndef mingw32_HOST_OS
posixLocker :: (Maybe FileMode -> LockFile -> Annex (Maybe LockHandle)) -> LockFile -> Annex (Maybe LockHandle)
posixLocker takelock lockfile = do
	mode <- annexFileMode
	modifyContent lockfile $
		takelock (Just mode) lockfile
	
#else
winLocker :: (LockFile -> IO (Maybe LockHandle)) -> ContentLocker
winLocker takelock _ (Just lockfile) = do
	modifyContent lockfile $
		void $ liftIO $ tryIO $
			writeFile lockfile ""
	liftIO $ takelock lockfile
-- never reached; windows always uses a separate lock file
winLocker _ _ Nothing = return Nothing
#endif

lockContentUsing :: ContentLocker -> Key -> Annex a -> Annex a
lockContentUsing locker key a = do
	contentfile <- fromRawFilePath <$> calcRepo (gitAnnexLocation key)
	lockfile <- contentLockFile key
	bracket
		(lock contentfile lockfile)
		(unlock lockfile)
		(const a)
  where
	alreadylocked = giveup "content is locked"
	failedtolock e = giveup $ "failed to lock content: " ++ show e

	lock contentfile lockfile =
		(maybe alreadylocked return 
			=<< locker contentfile lockfile)
		`catchIO` failedtolock

#ifndef mingw32_HOST_OS
	unlock mlockfile lck = do
		maybe noop cleanuplockfile mlockfile
		liftIO $ dropLock lck
#else
	unlock mlockfile lck = do
		-- Can't delete a locked file on Windows
		liftIO $ dropLock lck
		maybe noop cleanuplockfile mlockfile
#endif

	cleanuplockfile lockfile = modifyContent lockfile $
		void $ liftIO $ tryIO $
			nukeFile lockfile

{- Runs an action, passing it the temp file to get,
 - and if the action succeeds, verifies the file matches
 - the key and moves the file into the annex as a key's content. -}
getViaTmp :: RetrievalSecurityPolicy -> VerifyConfig -> Key -> (FilePath -> Annex (Bool, Verification)) -> Annex Bool
getViaTmp rsp v key action = checkDiskSpaceToGet key False $
	getViaTmpFromDisk rsp v key action

{- Like getViaTmp, but does not check that there is enough disk space
 - for the incoming key. For use when the key content is already on disk
 - and not being copied into place. -}
getViaTmpFromDisk :: RetrievalSecurityPolicy -> VerifyConfig -> Key -> (FilePath -> Annex (Bool, Verification)) -> Annex Bool
getViaTmpFromDisk rsp v key action = checkallowed $ do
	tmpfile <- prepTmp key
	resuming <- liftIO $ doesFileExist tmpfile
	(ok, verification) <- action tmpfile
	-- When the temp file already had content, we don't know if
	-- that content is good or not, so only trust if it the action
	-- Verified it in passing. Otherwise, force verification even
	-- if the VerifyConfig normally disables it.
	let verification' = if resuming
		then case verification of
			Verified -> Verified
			_ -> MustVerify
		else verification
	if ok
		then ifM (verifyKeyContent rsp v verification' key tmpfile)
			( ifM (pruneTmpWorkDirBefore tmpfile (moveAnnex key))
				( do
					logStatus key InfoPresent
					return True
				, return False
				)
			, do
				warning "verification of content failed"
				-- The bad content is not retained, because
				-- a retry should not try to resume from it
				-- since it's apparently corrupted.
				-- Also, the bad content could be any data,
				-- including perhaps the content of another
				-- file than the one that was requested,
				-- and so it's best not to keep it on disk.
				pruneTmpWorkDirBefore tmpfile (liftIO . nukeFile)
				return False
			)
		-- On transfer failure, the tmp file is left behind, in case
		-- caller wants to resume its transfer
		else return False
  where
	-- Avoid running the action to get the content when the
	-- RetrievalSecurityPolicy would cause verification to always fail.
	checkallowed a = case rsp of
		RetrievalAllKeysSecure -> a
		RetrievalVerifiableKeysSecure
			| isVerifiable (fromKey keyVariety key) -> a
			| otherwise -> ifM (annexAllowUnverifiedDownloads <$> Annex.getGitConfig)
				( a
				, warnUnverifiableInsecure key >> return False
				)

{- Verifies that a file is the expected content of a key.
 -
 - Configuration can prevent verification, for either a
 - particular remote or always, unless the RetrievalSecurityPolicy
 - requires verification.
 -
 - Most keys have a known size, and if so, the file size is checked.
 -
 - When the key's backend allows verifying the content (via checksum),
 - it is checked. 
 -
 - If the RetrievalSecurityPolicy requires verification and the key's
 - backend doesn't support it, the verification will fail.
 -}
verifyKeyContent :: RetrievalSecurityPolicy -> VerifyConfig -> Verification -> Key -> FilePath -> Annex Bool
verifyKeyContent rsp v verification k f = case (rsp, verification) of
	(_, Verified) -> return True
	(RetrievalVerifiableKeysSecure, _)
		| isVerifiable (fromKey keyVariety k) -> verify
		| otherwise -> ifM (annexAllowUnverifiedDownloads <$> Annex.getGitConfig)
			( verify
			, warnUnverifiableInsecure k >> return False
			)
	(_, UnVerified) -> ifM (shouldVerify v)
		( verify
		, return True
		)
	(_, MustVerify) -> verify
  where
	verify = enteringStage VerifyStage $ verifysize <&&> verifycontent
	verifysize = case fromKey keySize k of
		Nothing -> return True
		Just size -> do
			size' <- liftIO $ catchDefaultIO 0 $ getFileSize f
			return (size' == size)
	verifycontent = case Types.Backend.verifyKeyContent =<< Backend.maybeLookupBackendVariety (fromKey keyVariety k) of
		Nothing -> return True
		Just verifier -> verifier k f

warnUnverifiableInsecure :: Key -> Annex ()
warnUnverifiableInsecure k = warning $ unwords
	[ "Getting " ++ kv ++ " keys with this remote is not secure;"
	, "the content cannot be verified to be correct."
	, "(Use annex.security.allow-unverified-downloads to bypass"
	, "this safety check.)"
	]
  where
	kv = decodeBS (formatKeyVariety (fromKey keyVariety k))

data VerifyConfig = AlwaysVerify | NoVerify | RemoteVerify Remote | DefaultVerify

shouldVerify :: VerifyConfig -> Annex Bool
shouldVerify AlwaysVerify = return True
shouldVerify NoVerify = return False
shouldVerify DefaultVerify = annexVerify <$> Annex.getGitConfig
shouldVerify (RemoteVerify r) = 
	(shouldVerify DefaultVerify
			<&&> pure (remoteAnnexVerify (Types.Remote.gitconfig r)))
	-- Export remotes are not key/value stores, so always verify
	-- content from them even when verification is disabled.
	<||> Types.Remote.isExportSupported r

{- Checks if there is enough free disk space to download a key
 - to its temp file.
 -
 - When the temp file already exists, count the space it is using as
 - free, since the download will overwrite it or resume.
 -
 - Wen there's enough free space, runs the download action.
 -}
checkDiskSpaceToGet :: Key -> a -> Annex a -> Annex a
checkDiskSpaceToGet key unabletoget getkey = do
	tmp <- fromRepo $ gitAnnexTmpObjectLocation key

	e <- liftIO $ doesFileExist tmp
	alreadythere <- liftIO $ if e
		then getFileSize tmp
		else return 0
	ifM (checkDiskSpace Nothing key alreadythere True)
		( do
			-- The tmp file may not have been left writable
			when e $ thawContent tmp
			getkey
		, return unabletoget
		)

prepTmp :: Key -> Annex FilePath
prepTmp key = do
	tmp <- fromRepo $ gitAnnexTmpObjectLocation key
	createAnnexDirectory (parentDir tmp)
	return tmp

{- Prepares a temp file for a key, runs an action on it, and cleans up
 - the temp file. If the action throws an exception, the temp file is
 - left behind, which allows for resuming.
 -}
withTmp :: Key -> (FilePath -> Annex a) -> Annex a
withTmp key action = do
	tmp <- prepTmp key
	res <- action tmp
	pruneTmpWorkDirBefore tmp (liftIO . nukeFile)
	return res

{- Moves a key's content into .git/annex/objects/
 -
 - When a key has associated pointer files, the object is hard
 - linked (or copied) to the files, and the object file is left thawed.
 -
 - What if the key there already has content? This could happen for
 - various reasons; perhaps the same content is being annexed again.
 - Perhaps there has been a hash collision generating the keys.
 -
 - The current strategy is to assume that in this case it's safe to delete
 - one of the two copies of the content; and the one already in the annex
 - is left there, assuming it's the original, canonical copy.
 -
 - I considered being more paranoid, and checking that both files had
 - the same content. Decided against it because A) users explicitly choose
 - a backend based on its hashing properties and so if they're dealing
 - with colliding files it's their own fault and B) adding such a check
 - would not catch all cases of colliding keys. For example, perhaps 
 - a remote has a key; if it's then added again with different content then
 - the overall system now has two different peices of content for that
 - key, and one of them will probably get deleted later. So, adding the
 - check here would only raise expectations that git-annex cannot truely
 - meet.
 -
 - May return false, when a particular variety of key is not being
 - accepted into the repository. Will display a warning message in this
 - case. May also throw exceptions in some cases.
 -}
moveAnnex :: Key -> FilePath -> Annex Bool
moveAnnex key src = ifM (checkSecureHashes key)
	( do
		withObjectLoc key storeobject
		return True
	, return False
	)
  where
	storeobject dest = ifM (liftIO $ R.doesPathExist dest)
		( alreadyhave
		, modifyContent dest' $ do
			freezeContent src
			liftIO $ moveFile src dest'
			g <- Annex.gitRepo 
			fs <- map (`fromTopFilePath` g)
				<$> Database.Keys.getAssociatedFiles key
			unless (null fs) $ do
				ics <- mapM (populatePointerFile (Restage True) key dest) fs
				Database.Keys.storeInodeCaches' key [dest] (catMaybes ics)
		)
	  where
		dest' = fromRawFilePath dest
	alreadyhave = liftIO $ removeFile src

checkSecureHashes :: Key -> Annex Bool
checkSecureHashes key
	| cryptographicallySecure (fromKey keyVariety key) = return True
	| otherwise = ifM (annexSecureHashesOnly <$> Annex.getGitConfig)
		( do
			warning $ "annex.securehashesonly blocked adding " ++ decodeBS (formatKeyVariety (fromKey keyVariety key)) ++ " key to annex objects"
			return False
		, return True
		)

data LinkAnnexResult = LinkAnnexOk | LinkAnnexFailed | LinkAnnexNoop

{- Populates the annex object file by hard linking or copying a source
 - file to it. -}
linkToAnnex :: Key -> FilePath -> Maybe InodeCache -> Annex LinkAnnexResult
linkToAnnex key src srcic = ifM (checkSecureHashes key)
	( do
		dest <- fromRawFilePath <$> calcRepo (gitAnnexLocation key)
		modifyContent dest $ linkAnnex To key src srcic dest Nothing
	, return LinkAnnexFailed
	)

{- Makes a destination file be a link or copy from the annex object. -}
linkFromAnnex :: Key -> FilePath -> Maybe FileMode -> Annex LinkAnnexResult
linkFromAnnex key dest destmode = do
	src <- calcRepo (gitAnnexLocation key)
	srcic <- withTSDelta (liftIO . genInodeCache src)
	linkAnnex From key (fromRawFilePath src) srcic dest destmode

data FromTo = From | To

{- Hard links or copies from or to the annex object location. 
 - Updates inode cache.
 -
 - Freezes or thaws the destination appropriately.
 -
 - When a hard link is made, the annex object necessarily has to be thawed
 - too. So, adding an object to the annex with a hard link can prevent
 - losing the content if the source file is deleted, but does not
 - guard against modifications.
 -
 - Nothing is done if the destination file already exists.
 -}
linkAnnex :: FromTo -> Key -> FilePath -> Maybe InodeCache -> FilePath -> Maybe FileMode -> Annex LinkAnnexResult
linkAnnex _ _ _ Nothing _ _ = return LinkAnnexFailed
linkAnnex fromto key src (Just srcic) dest destmode =
	withTSDelta (liftIO . genInodeCache dest') >>= \case
		Just destic -> do
			cs <- Database.Keys.getInodeCaches key
			if null cs
				then Database.Keys.addInodeCaches key [srcic, destic]
				else Database.Keys.addInodeCaches key [srcic]
			return LinkAnnexNoop
		Nothing -> linkOrCopy key src dest destmode >>= \case
			Nothing -> failed
			Just r -> do
				case fromto of
					From -> thawContent dest
					To -> case r of
						Copied -> freezeContent dest
						Linked -> noop
				checksrcunchanged
  where
	dest' = toRawFilePath dest
	failed = do
		Database.Keys.addInodeCaches key [srcic]
		return LinkAnnexFailed
	checksrcunchanged = withTSDelta (liftIO . genInodeCache (toRawFilePath src)) >>= \case
		Just srcic' | compareStrong srcic srcic' -> do
			destic <- withTSDelta (liftIO . genInodeCache dest')
			Database.Keys.addInodeCaches key $
				catMaybes [destic, Just srcic]
			return LinkAnnexOk
		_ -> do
			liftIO $ nukeFile dest
			failed

{- Removes the annex object file for a key. Lowlevel. -}
unlinkAnnex :: Key -> Annex ()
unlinkAnnex key = do
	obj <- fromRawFilePath <$> calcRepo (gitAnnexLocation key)
	modifyContent obj $ do
		secureErase obj
		liftIO $ nukeFile obj

{- Runs an action to transfer an object's content.
 -
 - In some cases, it's possible for the file to change as it's being sent.
 - If this happens, runs the rollback action and returns False. The
 - rollback action should remove the data that was transferred.
 -}
sendAnnex :: Key -> Annex () -> (FilePath -> Annex Bool) -> Annex Bool
sendAnnex key rollback sendobject = go =<< prepSendAnnex key
  where
	go Nothing = return False
	go (Just (f, checksuccess)) = do
		r <- sendobject f
		ifM checksuccess
			( return r
			, do
				rollback
				return False
			)

{- Returns a file that contains an object's content,
 - and a check to run after the transfer is complete.
 -
 - When a file is unlocked, it's possble for its content to
 - change as it's being sent. The check detects this case
 - and returns False.
 -
 - Note that the returned check action is, in some cases, run in the
 - Annex monad of the remote that is receiving the object, rather than
 - the sender. So it cannot rely on Annex state.
 -}
prepSendAnnex :: Key -> Annex (Maybe (FilePath, Annex Bool))
prepSendAnnex key = withObjectLoc key $ \f -> do
	cache <- Database.Keys.getInodeCaches key
	cache' <- if null cache
		-- Since no inode cache is in the database, this
		-- object is not currently unlocked. But that could
		-- change while the transfer is in progress, so
		-- generate an inode cache for the starting
		-- content.
		then maybeToList <$>
			withTSDelta (liftIO . genInodeCache f)
		else pure cache
	return $ if null cache'
		then Nothing
		else Just (fromRawFilePath f, sameInodeCache f cache')

{- Performs an action, passing it the location to use for a key's content. -}
withObjectLoc :: Key -> (RawFilePath -> Annex a) -> Annex a
withObjectLoc key a = a =<< calcRepo (gitAnnexLocation key)

cleanObjectLoc :: Key -> Annex () -> Annex ()
cleanObjectLoc key cleaner = do
	file <- fromRawFilePath <$> calcRepo (gitAnnexLocation key)
	void $ tryIO $ thawContentDir file
	cleaner
	liftIO $ removeparents file (3 :: Int)
  where
	removeparents _ 0 = noop
	removeparents file n = do
		let dir = parentDir file
		maybe noop (const $ removeparents dir (n-1))
			<=< catchMaybeIO $ removeDirectory dir

{- Removes a key's file from .git/annex/objects/
 -}
removeAnnex :: ContentRemovalLock -> Annex ()
removeAnnex (ContentRemovalLock key) = withObjectLoc key $ \file ->
	cleanObjectLoc key $ do
		let file' = fromRawFilePath file
		secureErase file'
		liftIO $ nukeFile file'
		g <- Annex.gitRepo 
		mapM_ (\f -> void $ tryIO $ resetpointer $ fromTopFilePath f g)
			=<< Database.Keys.getAssociatedFiles key
		Database.Keys.removeInodeCaches key
  where
	-- Check associated pointer file for modifications, and reset if
	-- it's unmodified.
	resetpointer file = ifM (isUnmodified key file)
		( depopulatePointerFile key file
		-- Modified file, so leave it alone.
		-- If it was a hard link to the annex object,
		-- that object might have been frozen as part of the
		-- removal process, so thaw it.
		, void $ tryIO $ thawContent $ fromRawFilePath file
		)

{- Check if a file contains the unmodified content of the key.
 -
 - The expensive way to tell is to do a verification of its content.
 - The cheaper way is to see if the InodeCache for the key matches the
 - file. -}
isUnmodified :: Key -> RawFilePath -> Annex Bool
isUnmodified key f = go =<< geti
  where
	go Nothing = return False
	go (Just fc) = isUnmodifiedCheap' key fc <||> expensivecheck fc
	expensivecheck fc = ifM (verifyKeyContent RetrievalAllKeysSecure AlwaysVerify UnVerified key (fromRawFilePath f))
		( do
			-- The file could have been modified while it was
			-- being verified. Detect that.
			ifM (geti >>= maybe (return False) (compareInodeCaches fc))
				( do
					-- Update the InodeCache to avoid
					-- performing this expensive check again.
					Database.Keys.addInodeCaches key [fc]
					return True
				, return False
				)
		, return False
		)
	geti = withTSDelta (liftIO . genInodeCache f)

{- Cheap check if a file contains the unmodified content of the key,
 - only checking the InodeCache of the key.
 -
 - Note that, on systems not supporting high-resolution mtimes,
 - this may report a false positive when repeated edits are made to a file
 - within a small time window (eg 1 second).
 -}
isUnmodifiedCheap :: Key -> RawFilePath -> Annex Bool
isUnmodifiedCheap key f = maybe (return False) (isUnmodifiedCheap' key) 
	=<< withTSDelta (liftIO . genInodeCache f)

isUnmodifiedCheap' :: Key -> InodeCache -> Annex Bool
isUnmodifiedCheap' key fc = 
	anyM (compareInodeCaches fc) =<< Database.Keys.getInodeCaches key

{- Moves a key out of .git/annex/objects/ into .git/annex/bad, and
 - returns the file it was moved to. -}
moveBad :: Key -> Annex FilePath
moveBad key = do
	src <- fromRawFilePath <$> calcRepo (gitAnnexLocation key)
	bad <- fromRepo gitAnnexBadDir
	let dest = bad </> takeFileName src
	createAnnexDirectory (parentDir dest)
	cleanObjectLoc key $
		liftIO $ moveFile src dest
	logStatus key InfoMissing
	return dest

data KeyLocation = InAnnex | InAnywhere

{- InAnnex only lists keys with content in .git/annex/objects.
 - InAnywhere lists all keys that have directories in
 - .git/annex/objects, whether or not the content is present.
 -}
listKeys :: KeyLocation -> Annex [Key]
listKeys keyloc = do
	dir <- fromRepo gitAnnexObjectDir
	{- In order to run Annex monad actions within unsafeInterleaveIO,
	 - the current state is taken and reused. No changes made to this
	 - state will be preserved. 
	 -}
	s <- Annex.getState id
	depth <- gitAnnexLocationDepth <$> Annex.getGitConfig
	liftIO $ walk s depth dir
  where
	walk s depth dir = do
		contents <- catchDefaultIO [] (dirContents dir)
		if depth < 2
			then do
				contents' <- filterM (present s) contents
				let keys = mapMaybe (fileKey . takeFileName) contents'
				continue keys []
			else do
				let deeper = walk s (depth - 1)
				continue [] (map deeper contents)
	continue keys [] = return keys
	continue keys (a:as) = do
		{- Force lazy traversal with unsafeInterleaveIO. -}
		morekeys <- unsafeInterleaveIO a
		continue (morekeys++keys) as

	inanywhere = case keyloc of
		InAnywhere -> True
		_ -> False

	present _ _ | inanywhere = pure True
	present _ d = presentInAnnex d

	presentInAnnex = doesFileExist . contentfile
	contentfile d = d </> takeFileName d

{- Things to do to record changes to content when shutting down.
 -
 - It's acceptable to avoid committing changes to the branch,
 - especially if performing a short-lived action.
 -}
saveState :: Bool -> Annex ()
saveState nocommit = doSideAction $ do
	Annex.Queue.flush
	Database.Keys.closeDb
	unless nocommit $
		whenM (annexAlwaysCommit <$> Annex.getGitConfig) $
			Annex.Branch.commit =<< Annex.Branch.commitMessage

{- Downloads content from any of a list of urls, displaying a progress
 - meter. -}
downloadUrl :: Key -> MeterUpdate -> [Url.URLString] -> FilePath -> Annex Bool
downloadUrl k p urls file = 
	-- Poll the file to handle configurations where an external
	-- download command is used.
	meteredFile file (Just p) k $
		Url.withUrlOptions $ \uo -> 
			anyM (\u -> Url.download p u file uo) urls

{- Copies a key's content, when present, to a temp file.
 - This is used to speed up some rsyncs. -}
preseedTmp :: Key -> FilePath -> Annex Bool
preseedTmp key file = go =<< inAnnex key
  where
	go False = return False
	go True = do
		ok <- copy
		when ok $ thawContent file
		return ok
	copy = ifM (liftIO $ doesFileExist file)
		( return True
		, do
			s <- fromRawFilePath <$> (calcRepo $ gitAnnexLocation key)
			liftIO $ ifM (doesFileExist s)
				( copyFileExternal CopyTimeStamps s file
				, return False
				)
		)

{- Finds files directly inside a directory like gitAnnexBadDir 
 - (not in subdirectories) and returns the corresponding keys. -}
dirKeys :: (Git.Repo -> FilePath) -> Annex [Key]
dirKeys dirspec = do
	dir <- fromRepo dirspec
	ifM (liftIO $ doesDirectoryExist dir)
		( do
			contents <- liftIO $ getDirectoryContents dir
			files <- liftIO $ filterM doesFileExist $
				map (dir </>) contents
			return $ mapMaybe (fileKey . takeFileName) files
		, return []
		)

{- Looks in the specified directory for bad/tmp keys, and returns a list
 - of those that might still have value, or might be stale and removable.
 - 
 - Also, stale keys that can be proven to have no value
 - (ie, their content is already present) are deleted.
 -}
staleKeysPrune :: (Git.Repo -> FilePath) -> Bool -> Annex [Key]
staleKeysPrune dirspec nottransferred = do
	contents <- dirKeys dirspec
	
	dups <- filterM inAnnex contents
	let stale = contents `exclude` dups

	dir <- fromRepo dirspec
	forM_ dups $ \k ->
		pruneTmpWorkDirBefore (dir </> keyFile k) (liftIO . removeFile)

	if nottransferred
		then do
			inprogress <- S.fromList . map (transferKey . fst)
				<$> getTransfers
			return $ filter (`S.notMember` inprogress) stale
		else return stale

{- Prune the work dir associated with the specified content file,
 - before performing an action that deletes the file, or moves it away.
 -
 - This preserves the invariant that the workdir never exists without
 - the content file.
 -}
pruneTmpWorkDirBefore :: FilePath -> (FilePath -> Annex a) -> Annex a
pruneTmpWorkDirBefore f action = do
	let workdir = gitAnnexTmpWorkDir f
	liftIO $ whenM (doesDirectoryExist workdir) $
		removeDirectoryRecursive workdir
	action f

{- Runs an action, passing it a temporary work directory where
 - it can write files while receiving the content of a key.
 -
 - Preserves the invariant that the workdir never exists without the
 - content file, by creating an empty content file first.
 -
 - On exception, or when the action returns Nothing,
 - the temporary work directory is retained (unless
 - empty), so anything in it can be used on resume.
 -}
withTmpWorkDir :: Key -> (FilePath -> Annex (Maybe a)) -> Annex (Maybe a)
withTmpWorkDir key action = do
	-- Create the object file if it does not exist. This way,
	-- staleKeysPrune only has to look for object files, and can
	-- clean up gitAnnexTmpWorkDir for those it finds.
	obj <- prepTmp key
	unlessM (liftIO $ doesFileExist obj) $ do
		liftIO $ writeFile obj ""
		setAnnexFilePerm obj
	let tmpdir = gitAnnexTmpWorkDir obj
	liftIO $ createDirectoryIfMissing True tmpdir
	setAnnexDirPerm tmpdir
	res <- action tmpdir
	case res of
		Just _ -> liftIO $ removeDirectoryRecursive tmpdir
		Nothing -> liftIO $ void $ tryIO $ removeDirectory tmpdir
	return res

{- Finds items in the first, smaller list, that are not
 - present in the second, larger list.
 - 
 - Constructing a single set, of the list that tends to be
 - smaller, appears more efficient in both memory and CPU
 - than constructing and taking the S.difference of two sets. -}
exclude :: Ord a => [a] -> [a] -> [a]
exclude [] _ = [] -- optimisation
exclude smaller larger = S.toList $ remove larger $ S.fromList smaller
  where
	remove a b = foldl (flip S.delete) b a
