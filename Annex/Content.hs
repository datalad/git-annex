{- git-annex file content managing
 -
 - Copyright 2010-2017 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Annex.Content (
	inAnnex,
	inAnnex',
	inAnnexSafe,
	inAnnexCheck,
	lockContentShared,
	lockContentForRemoval,
	ContentRemovalLock,
	getViaTmp,
	getViaTmp',
	checkDiskSpaceToGet,
	prepTmp,
	withTmp,
	checkDiskSpace,
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
	getKeysPresent,
	saveState,
	downloadUrl,
	preseedTmp,
	dirKeys,
	withObjectLoc,
	staleKeysPrune,
	isUnmodified,
	verifyKeyContent,
	VerifyConfig(..),
	Verification(..),
	unVerified,
) where

import System.IO.Unsafe (unsafeInterleaveIO)
import System.PosixCompat.Files
import qualified Data.Set as S

import Annex.Common
import Logs.Location
import Types.Transfer
import Logs.Transfer
import qualified Git
import qualified Annex
import qualified Annex.Queue
import qualified Annex.Branch
import Utility.DiskFree
import Utility.FileMode
import qualified Annex.Url as Url
import Utility.DataUnits
import Utility.CopyFile
import Utility.Metered
import Config
import Git.FilePath
import Annex.Perms
import Annex.Link
import qualified Annex.Content.Direct as Direct
import Annex.ReplaceFile
import Annex.LockPool
import Messages.Progress
import Types.Remote (unVerified, Verification(..))
import qualified Types.Remote
import qualified Types.Backend
import qualified Backend
import qualified Database.Keys
import Types.NumCopies
import Types.Key
import Annex.UUID
import Annex.InodeSentinal
import Utility.InodeCache

{- Checks if a given key's content is currently present. -}
inAnnex :: Key -> Annex Bool
inAnnex key = inAnnexCheck key $ liftIO . doesFileExist

{- Runs an arbitrary check on a key's content. -}
inAnnexCheck :: Key -> (FilePath -> Annex Bool) -> Annex Bool
inAnnexCheck key check = inAnnex' id False check key

{- inAnnex that performs an arbitrary check of the key's content.
 -
 - When the content is unlocked, it must also be unmodified, or the bad
 - value will be returned.
 -
 - In direct mode, at least one of the associated files must pass the
 - check. Additionally, the file must be unmodified.
 -}
inAnnex' :: (a -> Bool) -> a -> (FilePath -> Annex a) -> Key -> Annex a
inAnnex' isgood bad check key = withObjectLoc key checkindirect checkdirect
  where
	checkindirect loc = do
		r <- check loc
		if isgood r
			then do
				cache <- Database.Keys.getInodeCaches key
				if null cache
					then return r
					else ifM (sameInodeCache loc cache)
						( return r
						, return bad
						)
			else return bad
	checkdirect [] = return bad
	checkdirect (loc:locs) = do
		r <- check loc
		if isgood r
			then ifM (Direct.goodContent key loc)
				( return r
				, checkdirect locs
				)
			else checkdirect locs

{- A safer check; the key's content must not only be present, but
 - is not in the process of being removed. -}
inAnnexSafe :: Key -> Annex (Maybe Bool)
inAnnexSafe key = inAnnex' (fromMaybe True) (Just False) go key
  where
	is_locked = Nothing
	is_unlocked = Just True
	is_missing = Just False

	go contentfile = maybe (checkindirect contentfile) (checkdirect contentfile)
		=<< contentLockFile key

#ifndef mingw32_HOST_OS
	checkindirect contentfile = checkOr is_missing contentfile
	{- In direct mode, the content file must exist, but
	 - the lock file generally won't exist unless a removal is in
	 - process. -}
	checkdirect contentfile lockfile =
		ifM (liftIO $ doesFileExist contentfile)
			( checkOr is_unlocked lockfile
			, return is_missing
			)
	checkOr d lockfile = checkLocked lockfile >>= return . \case
		Nothing -> d
		Just True -> is_locked
		Just False -> is_unlocked
#else
	checkindirect f = liftIO $ ifM (doesFileExist f)
		( lockShared f >>= \case
			Nothing -> return is_locked
			Just lockhandle -> do
				dropLock lockhandle
				return is_unlocked
		, return is_missing
		)
	{- In Windows, see if we can take a shared lock. If so, 
	 - remove the lock file to clean up after ourselves. -}
	checkdirect contentfile lockfile =
		ifM (liftIO $ doesFileExist contentfile)
			( modifyContent lockfile $ liftIO $
				lockShared >>= \case
					Nothing -> return is_locked
					Just lockhandle -> do
						dropLock lockhandle
						void $ tryIO $ nukeFile lockfile
						return is_unlocked
			, return is_missing
			)
#endif

{- Direct mode and especially Windows has to use a separate lock
 - file from the content, since locking the actual content file
 - would interfere with the user's use of it. -}
contentLockFile :: Key -> Annex (Maybe FilePath)
#ifndef mingw32_HOST_OS
contentLockFile key = ifM isDirect
	( Just <$> calcRepo (gitAnnexContentLock key)
	, return Nothing
	)
#else
contentLockFile key = Just <$> calcRepo (gitAnnexContentLock key)
#endif

{- Prevents the content from being removed while the action is running.
 - Uses a shared lock.
 -
 - Does not actually check if the content is present. Use inAnnex for that.
 - However, since the contentLockFile is the content file in indirect mode,
 - if the content is not present, locking it will fail.
 -
 - If locking fails, throws an exception rather than running the action.
 -
 - Note that, in direct mode, nothing prevents the user from directly
 - editing or removing the content, even while it's locked by this.
 -}
lockContentShared :: Key -> (VerifiedCopy -> Annex a) -> Annex a
lockContentShared key a = lockContentUsing lock key $ do
	u <- getUUID
	withVerifiedCopy LockedCopy u (return True) a
  where
#ifndef mingw32_HOST_OS
	lock contentfile Nothing = tryLockShared Nothing contentfile
	lock _ (Just lockfile) = posixLocker tryLockShared lockfile
#else
	lock = winLocker lockShared
#endif

{- Exclusively locks content, while performing an action that
 - might remove it.
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
	contentfile <- calcRepo $ gitAnnexLocation key
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
getViaTmp :: VerifyConfig -> Key -> (FilePath -> Annex (Bool, Verification)) -> Annex Bool
getViaTmp v key action = checkDiskSpaceToGet key False $
	getViaTmp' v key action

{- Like getViaTmp, but does not check that there is enough disk space
 - for the incoming key. For use when the key content is already on disk
 - and not being copied into place. -}
getViaTmp' :: VerifyConfig -> Key -> (FilePath -> Annex (Bool, Verification)) -> Annex Bool
getViaTmp' v key action = do
	tmpfile <- prepTmp key
	(ok, verification) <- action tmpfile
	if ok
		then ifM (verifyKeyContent v verification key tmpfile)
			( ifM (moveAnnex key tmpfile)
				( do
					logStatus key InfoPresent
					return True
				, return False
				)
			, do
				warning "verification of content failed"
				liftIO $ nukeFile tmpfile
				return False
			)
		-- On transfer failure, the tmp file is left behind, in case
		-- caller wants to resume its transfer
		else return False

{- Verifies that a file is the expected content of a key.
 - Configuration can prevent verification, for either a
 - particular remote or always.
 -
 - Most keys have a known size, and if so, the file size is checked.
 -
 - When the key's backend allows verifying the content (eg via checksum),
 - it is checked. 
 -}
verifyKeyContent :: VerifyConfig -> Verification -> Key -> FilePath -> Annex Bool
verifyKeyContent _ Verified _ _ = return True
verifyKeyContent v UnVerified k f = ifM (shouldVerify v)
	( verifysize <&&> verifycontent
	, return True
	)
  where
	verifysize = case keySize k of
		Nothing -> return True
		Just size -> do
			size' <- liftIO $ catchDefaultIO 0 $ getFileSize f
			return (size' == size)
	verifycontent = case Types.Backend.verifyKeyContent =<< Backend.maybeLookupBackendVariety (keyVariety k) of
		Nothing -> return True
		Just verifier -> verifier k f

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

{- Creates a temp file for a key, runs an action on it, and cleans up
 - the temp file. If the action throws an exception, the temp file is
 - left behind, which allows for resuming.
 -}
withTmp :: Key -> (FilePath -> Annex a) -> Annex a
withTmp key action = do
	tmp <- prepTmp key
	res <- action tmp
	liftIO $ nukeFile tmp
	return res

{- Checks that there is disk space available to store a given key,
 - in a destination directory (or the annex) printing a warning if not. 
 -
 - If the destination is on the same filesystem as the annex,
 - checks for any other running downloads, removing the amount of data still
 - to be downloaded from the free space. This way, we avoid overcommitting
 - when doing concurrent downloads.
 -}
checkDiskSpace :: Maybe FilePath -> Key -> Integer -> Bool -> Annex Bool
checkDiskSpace destdir key = checkDiskSpace' (fromMaybe 1 (keySize key)) destdir key

{- Allows specifying the size of the key, if it's known, which is useful
 - as not all keys know their size. -}
checkDiskSpace' :: Integer -> Maybe FilePath -> Key -> Integer -> Bool -> Annex Bool
checkDiskSpace' need destdir key alreadythere samefilesystem = ifM (Annex.getState Annex.force)
	( return True
	, do
		-- We can't get inprogress and free at the same
		-- time, and both can be changing, so there's a
		-- small race here. Err on the side of caution
		-- by getting inprogress first, so if it takes
		-- a while, we'll see any decrease in the free
		-- disk space.
		inprogress <- if samefilesystem
			then sizeOfDownloadsInProgress (/= key)
			else pure 0
		dir >>= liftIO . getDiskFree >>= \case
			Just have -> do
				reserve <- annexDiskReserve <$> Annex.getGitConfig
				let delta = need + reserve - have - alreadythere + inprogress
				let ok = delta <= 0
				unless ok $
					needmorespace delta
				return ok
			_ -> return True
	)
  where
	dir = maybe (fromRepo gitAnnexDir) return destdir
	needmorespace n =
		warning $ "not enough free space, need " ++ 
			roughSize storageUnits True n ++
			" more" ++ forcemsg
	forcemsg = " (use --force to override this check or adjust annex.diskreserve)"

{- Moves a key's content into .git/annex/objects/
 -
 - When a key has associated pointer files, the object is hard
 - linked (or copied) to the files, and the object file is left thawed.
 
 - In direct mode, moves the object file to the associated file, or files.
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
		withObjectLoc key storeobject storedirect
		return True
	, return False
	)
  where
	storeobject dest = ifM (liftIO $ doesFileExist dest)
		( alreadyhave
		, modifyContent dest $ do
			freezeContent src
			liftIO $ moveFile src dest
			g <- Annex.gitRepo 
			fs <- map (`fromTopFilePath` g)
				<$> Database.Keys.getAssociatedFiles key
			unless (null fs) $ do
				mapM_ (populatePointerFile key dest) fs
				Database.Keys.storeInodeCaches key (dest:fs)
		)
	storeindirect = storeobject =<< calcRepo (gitAnnexLocation key)

	{- In direct mode, the associated file's content may be locally
	 - modified. In that case, it's preserved. However, the content
	 - we're moving into the annex may be the only extant copy, so
	 - it's important we not lose it. So, when the key's content
	 - cannot be moved to any associated file, it's stored in indirect
	 - mode.
	 -}
	storedirect = storedirect' storeindirect
	storedirect' fallback [] = fallback
	storedirect' fallback (f:fs) = do
		thawContent src
		v <- isAnnexLink f
		if Just key == v
			then do
				Direct.updateInodeCache key src
				replaceFile f $ liftIO . moveFile src
				chmodContent f
				forM_ fs $
					Direct.addContentWhenNotPresent key f
			else ifM (Direct.goodContent key f)
				( storedirect' alreadyhave fs
				, storedirect' fallback fs
				)
	
	alreadyhave = liftIO $ removeFile src

checkSecureHashes :: Key -> Annex Bool
checkSecureHashes key
	| cryptographicallySecure (keyVariety key) = return True
	| otherwise = ifM (annexSecureHashesOnly <$> Annex.getGitConfig)
		( do
			warning $ "annex.securehashesonly blocked adding " ++ formatKeyVariety (keyVariety key) ++ " key to annex objects"
			return False
		, return True
		)

populatePointerFile :: Key -> FilePath -> FilePath -> Annex ()
populatePointerFile k obj f = go =<< liftIO (isPointerFile f)
  where
	go (Just k') | k == k' = do
		destmode <- liftIO $ catchMaybeIO $ fileMode <$> getFileStatus f
		liftIO $ nukeFile f
		ifM (linkOrCopy k obj f destmode)
			( thawContent f
			, liftIO $ writePointerFile f k destmode
			)
	go _ = return ()

data LinkAnnexResult = LinkAnnexOk | LinkAnnexFailed | LinkAnnexNoop

{- Populates the annex object file by hard linking or copying a source
 - file to it. -}
linkToAnnex :: Key -> FilePath -> Maybe InodeCache -> Annex LinkAnnexResult
linkToAnnex key src srcic = ifM (checkSecureHashes key)
	( do
		dest <- calcRepo (gitAnnexLocation key)
		modifyContent dest $ linkAnnex To key src srcic dest Nothing
	, return LinkAnnexFailed
	)

{- Makes a destination file be a link or copy from the annex object. -}
linkFromAnnex :: Key -> FilePath -> Maybe FileMode -> Annex LinkAnnexResult
linkFromAnnex key dest destmode = do
	src <- calcRepo (gitAnnexLocation key)
	srcic <- withTSDelta (liftIO . genInodeCache src)
	linkAnnex From key src srcic dest destmode

data FromTo = From | To

{- Hard links or copies from or to the annex object location. 
 - Updates inode cache.
 -
 - Thaws the file that is not the annex object.
 - When a hard link was made, this necessarily thaws
 - the annex object too. So, adding an object to the annex this
 - way can prevent losing the content if the source file
 - is deleted, but does not guard against modifications.
 -
 - Nothing is done if the destination file already exists.
 -}
linkAnnex :: FromTo -> Key -> FilePath -> Maybe InodeCache -> FilePath -> Maybe FileMode -> Annex LinkAnnexResult
linkAnnex _ _ _ Nothing _ _ = return LinkAnnexFailed
linkAnnex fromto key src (Just srcic) dest destmode =
	withTSDelta (liftIO . genInodeCache dest) >>= \case
		Just destic -> do
			cs <- Database.Keys.getInodeCaches key
			if null cs
				then Database.Keys.addInodeCaches key [srcic, destic]
				else Database.Keys.addInodeCaches key [srcic]
			return LinkAnnexNoop
		Nothing -> ifM (linkOrCopy key src dest destmode)
			( do
				thawContent $ case fromto of
					From -> dest
					To -> src
				checksrcunchanged
			, failed
			)
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
			liftIO $ nukeFile dest
			failed

{- Hard links or copies src to dest, which must not already exists.
 -
 - Only uses a hard link when annex.thin is enabled and when src is
 - not already hardlinked to elsewhere.
 -
 - Checks disk reserve before copying against the size of the key,
 - and will fail if not enough space, or if the dest file already exists.
 -
 - The FileMode, if provided, influences the mode of the dest file.
 - In particular, if it has an execute bit set, the dest file's
 - execute bit will be set. The mode is not fully copied over because
 - git doesn't support file modes beyond execute.
 -}
linkOrCopy :: Key -> FilePath -> FilePath -> Maybe FileMode -> Annex Bool
linkOrCopy = linkOrCopy' (annexThin <$> Annex.getGitConfig)

linkOrCopy' :: Annex Bool -> Key -> FilePath -> FilePath -> Maybe FileMode -> Annex Bool
linkOrCopy' canhardlink key src dest destmode
	| maybe False isExecutable destmode = copy =<< getstat
	| otherwise = catchBoolIO $
		ifM canhardlink
			( hardlink
			, copy =<< getstat
			)
  where
	hardlink = do
		s <- getstat
		if linkCount s > 1
			then copy s
			else liftIO (createLink src dest >> preserveGitMode dest destmode >> return True)
				`catchIO` const (copy s)
	copy = checkedCopyFile' key src dest destmode
	getstat = liftIO $ getFileStatus src

{- Removes the annex object file for a key. Lowlevel. -}
unlinkAnnex :: Key -> Annex ()
unlinkAnnex key = do
	obj <- calcRepo $ gitAnnexLocation key
	modifyContent obj $ do
		secureErase obj
		liftIO $ nukeFile obj

{- Checks disk space before copying. -}
checkedCopyFile :: Key -> FilePath -> FilePath -> Maybe FileMode -> Annex Bool
checkedCopyFile key src dest destmode = catchBoolIO $
	checkedCopyFile' key src dest destmode
		=<< liftIO (getFileStatus src)

checkedCopyFile' :: Key -> FilePath -> FilePath -> Maybe FileMode -> FileStatus -> Annex Bool
checkedCopyFile' key src dest destmode s = catchBoolIO $
	ifM (checkDiskSpace' (fromIntegral $ fileSize s) (Just $ takeDirectory dest) key 0 True)
		( liftIO $
			copyFileExternal CopyAllMetaData src dest
				<&&> preserveGitMode dest destmode
		, return False
		)

preserveGitMode :: FilePath -> Maybe FileMode -> IO Bool
preserveGitMode f (Just mode)
	| isExecutable mode = catchBoolIO $ do
		modifyFileMode f $ addModes executeModes
		return True
	| otherwise = catchBoolIO $ do
		modifyFileMode f $ removeModes executeModes
		return True
preserveGitMode _ _ = return True

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
 - When a file is unlocked (or in direct mode), it's possble for its
 - content to change as it's being sent. The check detects this case
 - and returns False.
 -
 - Note that the returned check action is, in some cases, run in the
 - Annex monad of the remote that is receiving the object, rather than
 - the sender. So it cannot rely on Annex state.
 -}
prepSendAnnex :: Key -> Annex (Maybe (FilePath, Annex Bool))
prepSendAnnex key = withObjectLoc key indirect direct
  where
	indirect f = do
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
			else Just (f, sameInodeCache f cache')
	direct [] = return Nothing
	direct (f:fs) = do
		cache <- Direct.recordedInodeCache key
		-- check that we have a good file
		ifM (sameInodeCache f cache)
			( return $ Just (f, sameInodeCache f cache)
			, direct fs
			)

{- Performs an action, passing it the location to use for a key's content.
 -
 - In direct mode, the associated files will be passed. But, if there are
 - no associated files for a key, the indirect mode action will be
 - performed instead. -}
withObjectLoc :: Key -> (FilePath -> Annex a) -> ([FilePath] -> Annex a) -> Annex a
withObjectLoc key indirect direct = ifM isDirect
	( do
		fs <- Direct.associatedFiles key
		if null fs
			then goindirect
			else direct fs
	, goindirect
	)
  where
	goindirect = indirect =<< calcRepo (gitAnnexLocation key)

cleanObjectLoc :: Key -> Annex () -> Annex ()
cleanObjectLoc key cleaner = do
	file <- calcRepo $ gitAnnexLocation key
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
 -
 - When a key has associated pointer files, they are checked for
 - modifications, and if unmodified, are reset.
 -
 - In direct mode, deletes the associated files or files, and replaces
 - them with symlinks.
 -}
removeAnnex :: ContentRemovalLock -> Annex ()
removeAnnex (ContentRemovalLock key) = withObjectLoc key remove removedirect
  where
	remove file = cleanObjectLoc key $ do
		secureErase file
		liftIO $ nukeFile file
		g <- Annex.gitRepo 
		mapM_ (\f -> void $ tryIO $ resetpointer $ fromTopFilePath f g)
			=<< Database.Keys.getAssociatedFiles key
		Database.Keys.removeInodeCaches key
		Direct.removeInodeCache key
	resetpointer file = ifM (isUnmodified key file)
		( do
			mode <- liftIO $ catchMaybeIO $ fileMode <$> getFileStatus file
			secureErase file
			liftIO $ nukeFile file
			liftIO $ writePointerFile file key mode
		-- Can't delete the pointer file.
		-- If it was a hard link to the annex object,
		-- that object might have been frozen as part of the
		-- removal process, so thaw it.
		, void $ tryIO $ thawContent file
		)
	removedirect fs = do
		cache <- Direct.recordedInodeCache key
		Direct.removeInodeCache key
		mapM_ (resetfile cache) fs
	resetfile cache f = whenM (Direct.sameInodeCache f cache) $ do
		l <- calcRepo $ gitAnnexLink f key
		secureErase f
		replaceFile f $ makeAnnexLink l

{- Check if a file contains the unmodified content of the key.
 -
 - The expensive way to tell is to do a verification of its content.
 - The cheaper way is to see if the InodeCache for the key matches the
 - file. -}
isUnmodified :: Key -> FilePath -> Annex Bool
isUnmodified key f = go =<< geti
  where
	go Nothing = return False
	go (Just fc) = cheapcheck fc <||> expensivecheck fc
	cheapcheck fc = anyM (compareInodeCaches fc)
		=<< Database.Keys.getInodeCaches key
	expensivecheck fc = ifM (verifyKeyContent AlwaysVerify UnVerified key f)
		-- The file could have been modified while it was
		-- being verified. Detect that.
		( geti >>= maybe (return False) (compareInodeCaches fc)
		, return False
		)
	geti = withTSDelta (liftIO . genInodeCache f)

{- Runs the secure erase command if set, otherwise does nothing.
 - File may or may not be deleted at the end; caller is responsible for
 - making sure it's deleted. -}
secureErase :: FilePath -> Annex ()
secureErase file = maybe noop go =<< annexSecureEraseCommand <$> Annex.getGitConfig
  where
	go basecmd = void $ liftIO $
		boolSystem "sh" [Param "-c", Param $ gencmd basecmd]
	gencmd = massReplace [ ("%file", shellEscape file) ]

{- Moves a key out of .git/annex/objects/ into .git/annex/bad, and
 - returns the file it was moved to. -}
moveBad :: Key -> Annex FilePath
moveBad key = do
	src <- calcRepo $ gitAnnexLocation key
	bad <- fromRepo gitAnnexBadDir
	let dest = bad </> takeFileName src
	createAnnexDirectory (parentDir dest)
	cleanObjectLoc key $
		liftIO $ moveFile src dest
	logStatus key InfoMissing
	return dest

data KeyLocation = InAnnex | InRepository | InAnywhere

{- List of keys whose content exists in the specified location.
 
 - InAnnex only lists keys with content in .git/annex/objects,
 - while InRepository, in direct mode, also finds keys with content
 - in the work tree. InAnywhere lists all keys that have directories
 - in .git/annex/objects, whether or not the content is present.
 -
 - Note that InRepository has to check whether direct mode files
 - have goodContent.
 -}
getKeysPresent :: KeyLocation -> Annex [Key]
getKeysPresent keyloc = do
	direct <- isDirect
	dir <- fromRepo gitAnnexObjectDir
	s <- getstate direct
	depth <- gitAnnexLocationDepth <$> Annex.getGitConfig
	liftIO $ walk s direct depth dir
  where
	walk s direct depth dir = do
		contents <- catchDefaultIO [] (dirContents dir)
		if depth < 2
			then do
				contents' <- filterM (present s direct) contents
				let keys = mapMaybe (fileKey . takeFileName) contents'
				continue keys []
			else do
				let deeper = walk s direct (depth - 1)
				continue [] (map deeper contents)
	continue keys [] = return keys
	continue keys (a:as) = do
		{- Force lazy traversal with unsafeInterleaveIO. -}
		morekeys <- unsafeInterleaveIO a
		continue (morekeys++keys) as

	inanywhere = case keyloc of
		InAnywhere -> True
		_ -> False

	present _ _ _ | inanywhere = pure True
	present _ False d = presentInAnnex d
	present s True d = presentDirect s d <||> presentInAnnex d

	presentInAnnex = doesFileExist . contentfile
	contentfile d = d </> takeFileName d

	presentDirect s d = case keyloc of
		InAnnex -> return False
		InRepository -> case fileKey (takeFileName d) of
			Nothing -> return False
			Just k -> Annex.eval s $ 
				anyM (Direct.goodContent k) =<< Direct.associatedFiles k
		InAnywhere -> return True

	{- In order to run Annex monad actions within unsafeInterleaveIO,
	 - the current state is taken and reused. No changes made to this
	 - state will be preserved. 
	 -
	 - As an optimsation, call inodesChanged to prime the state with
	 - a cached value that will be used in the call to goodContent.
	 -}
	getstate direct = do
		when direct $
			void inodesChanged
		Annex.getState id

{- Things to do to record changes to content when shutting down.
 -
 - It's acceptable to avoid committing changes to the branch,
 - especially if performing a short-lived action.
 -}
saveState :: Bool -> Annex ()
saveState nocommit = doSideAction $ do
	Annex.Queue.flush
	unless nocommit $
		whenM (annexAlwaysCommit <$> Annex.getGitConfig) $
			Annex.Branch.commit "update"

{- Downloads content from any of a list of urls. -}
downloadUrl :: Key -> MeterUpdate -> [Url.URLString] -> FilePath -> Annex Bool
downloadUrl k p urls file = meteredFile file (Just p) k $
	go =<< annexWebDownloadCommand <$> Annex.getGitConfig
  where
	go Nothing = do
		a <- ifM commandProgressDisabled
			( return Url.downloadQuiet
			, return Url.download
			)
		Url.withUrlOptions $ \uo ->
			anyM (\u -> a u file uo) urls
	go (Just basecmd) = anyM (downloadcmd basecmd) urls
	downloadcmd basecmd url =
		progressCommand "sh" [Param "-c", Param $ gencmd url basecmd]
			<&&> liftIO (doesFileExist file)
	gencmd url = massReplace
		[ ("%file", shellEscape file)
		, ("%url", shellEscape url)
		]

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
			s <- calcRepo $ gitAnnexLocation key
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
	liftIO $ forM_ dups $ \t -> removeFile $ dir </> keyFile t

	if nottransferred
		then do
			inprogress <- S.fromList . map (transferKey . fst)
				<$> getTransfers
			return $ filter (`S.notMember` inprogress) stale
		else return stale

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
