{- git-annex file content managing
 -
 - Copyright 2010-2022 Joey Hess <id@joeyh.name>
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
	verificationOfContentFailed,
	checkDiskSpaceToGet,
	checkSecureHashes,
	prepTmp,
	withTmp,
	checkDiskSpace,
	needMoreDiskSpace,
	moveAnnex,
	populatePointerFile,
	linkToAnnex,
	linkFromAnnex,
	linkFromAnnex',
	LinkAnnexResult(..),
	unlinkAnnex,
	checkedCopyFile,
	linkOrCopy,
	linkOrCopy',
	sendAnnex,
	prepSendAnnex,
	prepSendAnnex',
	removeAnnex,
	moveBad,
	KeyLocation(..),
	listKeys,
	listKeys',
	saveState,
	downloadUrl,
	preseedTmp,
	dirKeys,
	withObjectLoc,
	staleKeysPrune,
	pruneTmpWorkDirBefore,
	isUnmodified,
	isUnmodifiedCheap,
	verifyKeyContentPostRetrieval,
	verifyKeyContent,
	VerifyConfig,
	VerifyConfigA(..),
	Verification(..),
	unVerified,
	withTmpWorkDir,
	KeyStatus(..),
	isKeyUnlockedThin,
	getKeyStatus,
	getKeyFileStatus,
	cleanObjectDirs,
) where

import System.IO.Unsafe (unsafeInterleaveIO)
import qualified Data.Set as S

import Annex.Common
import Annex.Content.Presence
import Annex.Content.LowLevel
import Annex.Content.PointerFile
import Annex.Verify
import qualified Git
import qualified Annex
import qualified Annex.Queue
import qualified Annex.Branch
import qualified Annex.Url as Url
import qualified Backend
import qualified Database.Keys
import Git.FilePath
import Annex.Perms
import Annex.Link
import Annex.LockPool
import Annex.UUID
import Annex.InodeSentinal
import Annex.ReplaceFile
import Annex.AdjustedBranch (adjustedBranchRefresh)
import Annex.DirHashes
import Messages.Progress
import Types.Remote (RetrievalSecurityPolicy(..), VerifyConfigA(..))
import Types.NumCopies
import Types.Key
import Types.Transfer
import Logs.Transfer
import Logs.Location
import Utility.InodeCache
import Utility.CopyFile
import Utility.Metered
import qualified Utility.RawFilePath as R

import qualified System.FilePath.ByteString as P
import System.PosixCompat.Files (isSymbolicLink, linkCount)

{- Prevents the content from being removed while the action is running.
 - Uses a shared lock.
 -
 - If locking fails, or the content is not present, throws an exception
 - rather than running the action.
 -}
lockContentShared :: Key -> (VerifiedCopy -> Annex a) -> Annex a
lockContentShared key a = lockContentUsing lock key notpresent $
	ifM (inAnnex key)
		( do
			u <- getUUID
			withVerifiedCopy LockedCopy u (return True) a
		, notpresent
		)
  where
	notpresent = giveup $ "failed to lock content: not present"
#ifndef mingw32_HOST_OS
	lock _ (Just lockfile) = 
		( posixLocker tryLockShared lockfile
		, Just (posixLocker tryLockExclusive lockfile)
		)
	lock contentfile Nothing =
		( tryLockShared Nothing contentfile
		, Nothing
		)
#else
	lock = winLocker lockShared
#endif

{- Exclusively locks content, while performing an action that
 - might remove it.
 -
 - If locking fails, throws an exception rather than running the action.
 -
 - When the content file itself is used as the lock file, 
 - and locking fails because the the content is not present, runs the
 - fallback action instead. However, the content is not guaranteed to be
 - present when this succeeds.
 -}
lockContentForRemoval :: Key -> Annex a -> (ContentRemovalLock -> Annex a) -> Annex a
lockContentForRemoval key fallback a = lockContentUsing lock key fallback $ 
	a (ContentRemovalLock key)
  where
#ifndef mingw32_HOST_OS
	lock _ (Just lockfile) = (posixLocker tryLockExclusive lockfile, Nothing)
	{- No lock file, so the content file itself is locked. 
	 - Since content files are stored with the write bit
	 - disabled, have to fiddle with permissions to open
	 - for an exclusive lock. -}
	lock contentfile Nothing =
		let lck = bracket_
			(thawContent contentfile)
			(freezeContent contentfile)
			(tryLockExclusive Nothing contentfile)
		in (lck, Nothing)
#else
	lock = winLocker lockExclusive
#endif

{- Passed the object content file, and maybe a separate lock file to use,
 - when the content file itself should not be locked. -}
type ContentLocker = RawFilePath -> Maybe LockFile -> (Annex (Maybe LockHandle), Maybe (Annex (Maybe LockHandle)))

#ifndef mingw32_HOST_OS
posixLocker :: (Maybe FileMode -> LockFile -> Annex (Maybe LockHandle)) -> LockFile -> Annex (Maybe LockHandle)
posixLocker takelock lockfile = do
	mode <- annexFileMode
	modifyContentDirWhenExists lockfile $
		takelock (Just mode) lockfile
#else
winLocker :: (LockFile -> IO (Maybe LockHandle)) -> ContentLocker
winLocker takelock _ (Just lockfile) = 
	let lck = do
		modifyContentDir lockfile $
			void $ liftIO $ tryIO $
				writeFile (fromRawFilePath lockfile) ""
		liftIO $ takelock lockfile
	in (lck, Nothing)
-- never reached; windows always uses a separate lock file
winLocker _ _ Nothing = (return Nothing, Nothing)
#endif

{- The fallback action is run if the ContentLocker throws an IO exception
 - and the content is not present. It's not guaranteed to always run when
 - the content is not present, because the content file is not always
 - the file that is locked. -}
lockContentUsing :: ContentLocker -> Key -> Annex a -> Annex a -> Annex a
lockContentUsing contentlocker key fallback a = withContentLockFile key $ \mlockfile -> do
	contentfile <- calcRepo (gitAnnexLocation key)
	let (locker, sharedtoexclusive) = contentlocker contentfile mlockfile
	bracket
		(lock locker mlockfile)
		(either (const noop) (unlock sharedtoexclusive mlockfile))
		go
  where
	alreadylocked = giveup "content is locked"
	failedtolock e = giveup $ "failed to lock content: " ++ show e

	lock locker mlockfile = tryIO $ locker >>= \case
		Nothing -> alreadylocked
		Just h ->
#ifndef mingw32_HOST_OS
			case mlockfile of
				Nothing -> return h
				Just lockfile ->
					ifM (checkSaneLock lockfile h)
						( return h
						, alreadylocked
						)
#else
			return h
#endif
	
	go (Right _) = a
	go (Left e) = ifM (inAnnex key)
		( failedtolock e
		, fallback 
		)

#ifndef mingw32_HOST_OS
	unlock sharedtoexclusive mlockfile lck = case (sharedtoexclusive, mlockfile) of
		-- We have a shared lock, so other processes may also
		-- have shared locks of the same lock file. To avoid
		-- deleting the lock file when there are other shared
		-- locks, try to convert to an exclusive lock, and only
		-- delete it when that succeeds.
		--
		-- Since other processes might be doing the same,
		-- a race is possible where we open the lock file
		-- and then another process takes the exclusive lock and
		-- deletes it, leaving us with an invalid lock. To avoid 
		-- that race, checkSaneLock is used after taking the lock
		-- here, and above.
		(Just exclusivelocker, Just lockfile) -> do
			liftIO $ dropLock lck
			exclusivelocker >>= \case
				Nothing -> return ()
				Just h -> do
					whenM (checkSaneLock lockfile h) $ do
						cleanuplockfile lockfile
					liftIO $ dropLock h
		-- We have an exclusive lock, so no other process can have
		-- the lock file locked, and so it's safe to remove it, as 
		-- long as all lock attempts use checkSaneLock.
		_ -> do
			maybe noop cleanuplockfile mlockfile
			liftIO $ dropLock lck
#else
	unlock _ mlockfile lck = do
		-- Can't delete a locked file on Windows,
		-- so close our lock first. If there are other shared
		-- locks, they will prevent the file deletion from
		-- happening.
		liftIO $ dropLock lck
		maybe noop cleanuplockfile mlockfile
#endif

	cleanuplockfile lockfile = void $ tryNonAsync $ do
		thawContentDir lockfile
		liftIO $ removeWhenExistsWith R.removeLink lockfile
		cleanObjectDirs lockfile

{- Runs an action, passing it the temp file to get,
 - and if the action succeeds, verifies the file matches
 - the key and moves the file into the annex as a key's content. -}
getViaTmp :: RetrievalSecurityPolicy -> VerifyConfig -> Key -> AssociatedFile -> (RawFilePath -> Annex (Bool, Verification)) -> Annex Bool
getViaTmp rsp v key af action = checkDiskSpaceToGet key False $
	getViaTmpFromDisk rsp v key af action

{- Like getViaTmp, but does not check that there is enough disk space
 - for the incoming key. For use when the key content is already on disk
 - and not being copied into place. -}
getViaTmpFromDisk :: RetrievalSecurityPolicy -> VerifyConfig -> Key -> AssociatedFile -> (RawFilePath -> Annex (Bool, Verification)) -> Annex Bool
getViaTmpFromDisk rsp v key af action = checkallowed $ do
	tmpfile <- prepTmp key
	resuming <- liftIO $ R.doesPathExist tmpfile
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
		then ifM (verifyKeyContentPostRetrieval rsp v verification' key tmpfile)
			( pruneTmpWorkDirBefore tmpfile (moveAnnex key af)
			, do
				verificationOfContentFailed tmpfile
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
		RetrievalVerifiableKeysSecure -> ifM (isVerifiable key)
			( a
			, ifM (annexAllowUnverifiedDownloads <$> Annex.getGitConfig)
				( a
				, warnUnverifiableInsecure key >> return False
				)
			)

{- When the content of a file that was successfully transferred from a remote
 - fails to verify, use this to display a message so the user knows why it
 - failed, and to clean up the corrupted content.
 -
 - The bad content is not retained, because the transfer of it succeeded.
 - So it's not incomplete and a resume using it will not work. While
 - some protocols like rsync could recover such a bad content file,
 - they are assumed to not write out bad data to a file in the first place.
 - Most protocols, including the P2P protocol, pick up downloads where they
 - left off, and so if the bad content were not deleted, repeated downloads
 - would continue to fail.
 -}
verificationOfContentFailed :: RawFilePath -> Annex ()
verificationOfContentFailed tmpfile = do
	warning "Verification of content failed"
	pruneTmpWorkDirBefore tmpfile
		(liftIO . removeWhenExistsWith R.removeLink)

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
	tmp <- fromRepo (gitAnnexTmpObjectLocation key)
	e <- liftIO $ doesFileExist (fromRawFilePath tmp)
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

prepTmp :: Key -> Annex RawFilePath
prepTmp key = do
	tmp <- fromRepo $ gitAnnexTmpObjectLocation key
	createAnnexDirectory (parentDir tmp)
	return tmp

{- Prepares a temp file for a key, runs an action on it, and cleans up
 - the temp file. If the action throws an exception, the temp file is
 - left behind, which allows for resuming.
 -}
withTmp :: Key -> (RawFilePath -> Annex a) -> Annex a
withTmp key action = do
	tmp <- prepTmp key
	res <- action tmp
	pruneTmpWorkDirBefore tmp (liftIO . removeWhenExistsWith R.removeLink)
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
 - the overall system now has two different pieces of content for that
 - key, and one of them will probably get deleted later. So, adding the
 - check here would only raise expectations that git-annex cannot truly
 - meet.
 -
 - May return false, when a particular variety of key is not being
 - accepted into the repository. Will display a warning message in this
 - case. May also throw exceptions in some cases.
 -}
moveAnnex :: Key -> AssociatedFile -> RawFilePath -> Annex Bool
moveAnnex key af src = ifM (checkSecureHashes' key)
	( do
		withObjectLoc key storeobject
		return True
	, return False
	)
  where
	storeobject dest = ifM (liftIO $ R.doesPathExist dest)
		( alreadyhave
		, adjustedBranchRefresh af $ modifyContentDir dest $ do
			liftIO $ moveFile src dest
			-- Freeze the object file now that it is in place.
			-- Waiting until now to freeze it allows for freeze
			-- hooks that prevent moving the file.
			freezeContent dest
			g <- Annex.gitRepo 
			fs <- map (`fromTopFilePath` g)
				<$> Database.Keys.getAssociatedFiles key
			unless (null fs) $ do
				destic <- withTSDelta $
					liftIO . genInodeCache dest
				ics <- mapM (populatePointerFile (Restage True) key dest) fs
				Database.Keys.addInodeCaches key
					(catMaybes (destic:ics))
		)
	alreadyhave = liftIO $ R.removeLink src

checkSecureHashes :: Key -> Annex (Maybe String)
checkSecureHashes key = ifM (Backend.isCryptographicallySecure key)
	( return Nothing
	, ifM (annexSecureHashesOnly <$> Annex.getGitConfig)
		( return $ Just $ "annex.securehashesonly blocked adding " ++ decodeBS (formatKeyVariety (fromKey keyVariety key)) ++ " key"
		, return Nothing
		)
	)

checkSecureHashes' :: Key -> Annex Bool
checkSecureHashes' key = checkSecureHashes key >>= \case
	Nothing -> return True
	Just msg -> do
		warning $ msg ++ "to annex objects"
		return False

data LinkAnnexResult = LinkAnnexOk | LinkAnnexFailed | LinkAnnexNoop
	deriving (Eq)

{- Populates the annex object file by hard linking or copying a source
 - file to it. -}
linkToAnnex :: Key -> RawFilePath -> Maybe InodeCache -> Annex LinkAnnexResult
linkToAnnex key src srcic = ifM (checkSecureHashes' key)
	( do
		dest <- calcRepo (gitAnnexLocation key)
		modifyContentDir dest $ linkAnnex To key src srcic dest Nothing
	, return LinkAnnexFailed
	)

{- Makes a destination file be a link or copy from the annex object.
 -
 - linkAnnex stats the file after copying it to add to the inode
 - cache. But dest may be a file in the working tree, which could
 - get modified immediately after being populated. To avoid such a
 - race, call linkAnnex on a temporary file and move it into place
 - afterwards. Note that a consequence of this is that, if the file
 - already exists, it will be overwritten.
 -}
linkFromAnnex :: Key -> RawFilePath -> Maybe FileMode -> Annex LinkAnnexResult
linkFromAnnex key dest destmode =
	replaceFile' (const noop) (fromRawFilePath dest) (== LinkAnnexOk) $ \tmp ->
		linkFromAnnex' key (toRawFilePath tmp) destmode

{- This is only safe to use when dest is not a worktree file. -}
linkFromAnnex' :: Key -> RawFilePath -> Maybe FileMode -> Annex LinkAnnexResult
linkFromAnnex' key dest destmode = do
	src <- calcRepo (gitAnnexLocation key)
	srcic <- withTSDelta (liftIO . genInodeCache src)
	linkAnnex From key src srcic dest destmode

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
linkAnnex :: FromTo -> Key -> RawFilePath -> Maybe InodeCache -> RawFilePath -> Maybe FileMode -> Annex LinkAnnexResult
linkAnnex _ _ _ Nothing _ _ = return LinkAnnexFailed
linkAnnex fromto key src (Just srcic) dest destmode =
	withTSDelta (liftIO . genInodeCache dest) >>= \case
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
	failed = do
		Database.Keys.addInodeCaches key [srcic]
		return LinkAnnexFailed
	checksrcunchanged = withTSDelta (liftIO . genInodeCache src) >>= \case
		Just srcic' | compareStrong srcic srcic' -> do
			destic <- withTSDelta (liftIO . genInodeCache dest)
			Database.Keys.addInodeCaches key $
				catMaybes [destic, Just srcic]
			return LinkAnnexOk
		_ -> do
			liftIO $ removeWhenExistsWith R.removeLink dest
			failed

{- Removes the annex object file for a key. Lowlevel. -}
unlinkAnnex :: Key -> Annex ()
unlinkAnnex key = do
	obj <- calcRepo (gitAnnexLocation key)
	modifyContentDir obj $ do
		secureErase obj
		liftIO $ removeWhenExistsWith R.removeLink obj

{- Runs an action to transfer an object's content.
 -
 - In some cases, it's possible for the file to change as it's being sent.
 - If this happens, runs the rollback action and throws an exception.
 - The rollback action should remove the data that was transferred.
 -}
sendAnnex :: Key -> Annex () -> (FilePath -> Annex a) -> Annex a
sendAnnex key rollback sendobject = go =<< prepSendAnnex' key
  where
	go (Just (f, check)) = do
		r <- sendobject f
		check >>= \case
			Nothing -> return r
			Just err -> do
				rollback
				giveup err
	go Nothing = giveup "content not available to send"

{- Returns a file that contains an object's content,
 - and a check to run after the transfer is complete.
 -
 - When a file is unlocked, it's possible for its content to
 - change as it's being sent. The check detects this case
 - and returns False.
 -
 - Note that the returned check action is, in some cases, run in the
 - Annex monad of the remote that is receiving the object, rather than
 - the sender. So it cannot rely on Annex state.
 -}
prepSendAnnex :: Key -> Annex (Maybe (FilePath, Annex Bool))
prepSendAnnex key = withObjectLoc key $ \f -> do
	let retval c = return $ Just (fromRawFilePath f, sameInodeCache f c)
	cache <- Database.Keys.getInodeCaches key
	if null cache
		-- Since no inode cache is in the database, this
		-- object is not currently unlocked. But that could
		-- change while the transfer is in progress, so
		-- generate an inode cache for the starting
		-- content.
		then maybe (return Nothing) (retval . (:[]))
			=<< withTSDelta (liftIO . genInodeCache f)
		-- Verify that the object is not modified. Usually this
		-- only has to check the inode cache, but if the cache
		-- is somehow stale, it will fall back to verifying its
		-- content.
		else withTSDelta (liftIO . genInodeCache f) >>= \case
			Just fc -> ifM (isUnmodified' key f fc cache)
				( retval (fc:cache)
				, return Nothing
				)
			Nothing -> return Nothing

prepSendAnnex' :: Key -> Annex (Maybe (FilePath, Annex (Maybe String)))
prepSendAnnex' key = prepSendAnnex key >>= \case
	Just (f, checksuccess) -> 
		let checksuccess' = ifM checksuccess
			( return Nothing
			, return (Just "content changed while it was being sent")
			)
		in return (Just (f, checksuccess'))
	Nothing -> return Nothing

cleanObjectLoc :: Key -> Annex () -> Annex ()
cleanObjectLoc key cleaner = do
	file <- calcRepo (gitAnnexLocation key)
	void $ tryIO $ thawContentDir file
	{- Thawing is not necessary when the file was frozen only
	 - by removing write perms. But if there is a thaw hook, it may do
	 - something else that is necessary to allow the file to be
	 - deleted. 
	 -}
	whenM hasThawHook $
		void $ tryIO $ thawContent file
		
	cleaner
	cleanObjectDirs file

{- Given a filename inside the object directory, tries to remove the object
 - directory, as well as the object hash directories.
 - 
 - Does nothing if the object directory is not empty, and does not
 - throw an exception if it's unable to remove a directory. -}
cleanObjectDirs :: RawFilePath -> Annex ()
cleanObjectDirs f = do
	HashLevels n <- objectHashLevels <$> Annex.getGitConfig
	liftIO $ go f (succ n)
  where
	go _ 0 = noop
	go file n = do
		let dir = parentDir file
		maybe noop (const $ go dir (n-1))
			<=< catchMaybeIO $ tryWhenExists $
				removeDirectory (fromRawFilePath dir)

{- Removes a key's file from .git/annex/objects/ -}
removeAnnex :: ContentRemovalLock -> Annex ()
removeAnnex (ContentRemovalLock key) = withObjectLoc key $ \file ->
	cleanObjectLoc key $ do
		secureErase file
		liftIO $ removeWhenExistsWith R.removeLink file
		g <- Annex.gitRepo 
		mapM_ (\f -> void $ tryIO $ resetpointer $ fromTopFilePath f g)
			=<< Database.Keys.getAssociatedFiles key
		Database.Keys.removeInodeCaches key
  where
	-- Check associated pointer file for modifications, and reset if
	-- it's unmodified.
	resetpointer file = unlessM (liftIO $ isSymbolicLink <$> R.getSymbolicLinkStatus file) $
		ifM (isUnmodified key file)
			( adjustedBranchRefresh (AssociatedFile (Just file)) $
				depopulatePointerFile key file
			-- Modified file, so leave it alone.
			-- If it was a hard link to the annex object,
			-- that object might have been frozen as part of the
			-- removal process, so thaw it.
			, void $ tryIO $ thawContent file
			)

{- Moves a key out of .git/annex/objects/ into .git/annex/bad, and
 - returns the file it was moved to. -}
moveBad :: Key -> Annex RawFilePath
moveBad key = do
	src <- calcRepo (gitAnnexLocation key)
	bad <- fromRepo gitAnnexBadDir
	let dest = bad P.</> P.takeFileName src
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
listKeys keyloc = listKeys' keyloc (const (pure True))

{- Due to use of unsafeInterleaveIO, the passed filter action
 - will be run in a copy of the Annex state, so any changes it
 - makes to the state will not be preserved. -}
listKeys' :: KeyLocation -> (Key -> Annex Bool) -> Annex [Key]
listKeys' keyloc want = do
	dir <- fromRepo gitAnnexObjectDir
	s <- Annex.getState id
	r <- Annex.getRead id
	depth <- gitAnnexLocationDepth <$> Annex.getGitConfig
	liftIO $ walk (s, r) depth (fromRawFilePath dir)
  where
	walk s depth dir = do
		contents <- catchDefaultIO [] (dirContents dir)
		if depth < 2
			then do
				contents' <- filterM present contents
				keys <- filterM (Annex.eval s . want) $
					mapMaybe (fileKey . P.takeFileName . toRawFilePath) contents'
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

	present _ | inanywhere = pure True
	present d = presentInAnnex d

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
	Database.Keys.flushDb
	unless nocommit $
		whenM (annexAlwaysCommit <$> Annex.getGitConfig) $
			Annex.Branch.commit =<< Annex.Branch.commitMessage

{- Downloads content from any of a list of urls, displaying a progress
 - meter.
 -
 - Only displays error message if all the urls fail to download.
 - When listfailedurls is set, lists each url and why it failed.
 - Otherwise, only displays one error message, from one of the urls
 - that failed.
 -}
downloadUrl :: Bool -> Key -> MeterUpdate -> Maybe IncrementalVerifier -> [Url.URLString] -> FilePath -> Url.UrlOptions -> Annex Bool
downloadUrl listfailedurls k p iv urls file uo = 
	-- Poll the file to handle configurations where an external
	-- download command is used.
	meteredFile file (Just p) k (go urls [])
  where
	go (u:us) errs = Url.download' p iv u file uo >>= \case
		Right () -> return True
		Left err -> do
			-- If the incremental verifier was fed anything
			-- while the download that failed ran, it's unable
			-- to be used for the other urls.
			case iv of
				Just iv' -> 
					liftIO $ positionIncrementalVerifier iv' >>= \case
					Just n | n > 0 -> unableIncrementalVerifier iv'
					_ -> noop
				Nothing -> noop
			go us ((u, err) : errs)
	go [] [] = return False
	go [] errs@((_, err):_) = do
		if listfailedurls
			then warning $ unlines $ flip map errs $ \(u, err') ->
				u ++ " " ++ err'
			else warning err
		return False

{- Copies a key's content, when present, to a temp file.
 - This is used to speed up some rsyncs. -}
preseedTmp :: Key -> FilePath -> Annex Bool
preseedTmp key file = go =<< inAnnex key
  where
	go False = return False
	go True = do
		ok <- copy
		when ok $ thawContent (toRawFilePath file)
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
dirKeys :: (Git.Repo -> RawFilePath) -> Annex [Key]
dirKeys dirspec = do
	dir <- fromRawFilePath <$> fromRepo dirspec
	ifM (liftIO $ doesDirectoryExist dir)
		( do
			contents <- liftIO $ getDirectoryContents dir
			files <- liftIO $ filterM doesFileExist $
				map (dir </>) contents
			return $ mapMaybe (fileKey . P.takeFileName . toRawFilePath) files
		, return []
		)

{- Looks in the specified directory for bad/tmp keys, and returns a list
 - of those that might still have value, or might be stale and removable.
 - 
 - Also, stale keys that can be proven to have no value
 - (ie, their content is already present) are deleted.
 -}
staleKeysPrune :: (Git.Repo -> RawFilePath) -> Bool -> Annex [Key]
staleKeysPrune dirspec nottransferred = do
	contents <- dirKeys dirspec
	
	dups <- filterM inAnnex contents
	let stale = contents `exclude` dups

	dir <- fromRepo dirspec
	forM_ dups $ \k ->
		pruneTmpWorkDirBefore (dir P.</> keyFile k)
			(liftIO . R.removeLink)

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
pruneTmpWorkDirBefore :: RawFilePath -> (RawFilePath -> Annex a) -> Annex a
pruneTmpWorkDirBefore f action = do
	let workdir = fromRawFilePath $ gitAnnexTmpWorkDir f
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
withTmpWorkDir :: Key -> (RawFilePath -> Annex (Maybe a)) -> Annex (Maybe a)
withTmpWorkDir key action = do
	-- Create the object file if it does not exist. This way,
	-- staleKeysPrune only has to look for object files, and can
	-- clean up gitAnnexTmpWorkDir for those it finds.
	obj <- prepTmp key
	let obj' = fromRawFilePath obj
	unlessM (liftIO $ doesFileExist obj') $ do
		liftIO $ writeFile obj' ""
		setAnnexFilePerm obj
	let tmpdir = gitAnnexTmpWorkDir obj
	createAnnexDirectory tmpdir
	res <- action tmpdir
	case res of
		Just _ -> liftIO $ removeDirectoryRecursive (fromRawFilePath tmpdir)
		Nothing -> liftIO $ void $ tryIO $ removeDirectory (fromRawFilePath tmpdir)
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
	obj <- calcRepo (gitAnnexLocation key)
	multilink <- ((> 1) . linkCount <$> liftIO (R.getFileStatus obj))
	return $ if multilink && afs
		then KeyUnlockedThin
		else KeyPresent

getKeyFileStatus :: Key -> RawFilePath -> Annex KeyStatus
getKeyFileStatus key file = do
	s <- getKeyStatus key
	case s of
		KeyUnlockedThin -> catchDefaultIO KeyUnlockedThin $
			ifM (isJust <$> isAnnexLink file)
				( return KeyLockedThin
				, return KeyUnlockedThin
				)
		_ -> return s

