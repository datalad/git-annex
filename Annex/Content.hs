{- git-annex file content managing
 -
 - Copyright 2010-2014 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Annex.Content (
	inAnnex,
	inAnnexSafe,
	inAnnexCheck,
	lockContent,
	getViaTmp,
	getViaTmpChecked,
	getViaTmpUnchecked,
	prepGetViaTmpChecked,
	withTmp,
	checkDiskSpace,
	moveAnnex,
	sendAnnex,
	prepSendAnnex,
	removeAnnex,
	fromAnnex,
	moveBad,
	getKeysPresent,
	saveState,
	downloadUrl,
	preseedTmp,
	freezeContent,
	thawContent,
	dirKeys,
	withObjectLoc,
) where

import System.IO.Unsafe (unsafeInterleaveIO)

import Common.Annex
import Logs.Location
import qualified Git
import qualified Annex
import qualified Annex.Queue
import qualified Annex.Branch
import Utility.DiskFree
import Utility.FileMode
import qualified Annex.Url as Url
import Types.Key
import Utility.DataUnits
import Utility.CopyFile
import Config
import Git.SharedRepository
import Annex.Perms
import Annex.Link
import Annex.Content.Direct
import Annex.ReplaceFile
import Annex.Exception

#ifdef mingw32_HOST_OS
import Utility.WinLock
#endif

{- Checks if a given key's content is currently present. -}
inAnnex :: Key -> Annex Bool
inAnnex key = inAnnexCheck key $ liftIO . doesFileExist

{- Runs an arbitrary check on a key's content. -}
inAnnexCheck :: Key -> (FilePath -> Annex Bool) -> Annex Bool
inAnnexCheck key check = inAnnex' id False check key

{- Generic inAnnex, handling both indirect and direct mode.
 -
 - In direct mode, at least one of the associated files must pass the
 - check. Additionally, the file must be unmodified.
 -}
inAnnex' :: (a -> Bool) -> a -> (FilePath -> Annex a) -> Key -> Annex a
inAnnex' isgood bad check key = withObjectLoc key checkindirect checkdirect
  where
	checkindirect loc = do
		whenM (fromRepo Git.repoIsUrl) $
			error "inAnnex cannot check remote repo"
		check loc
	checkdirect [] = return bad
	checkdirect (loc:locs) = do
		r <- check loc
		if isgood r
			then ifM (goodContent key loc)
				( return r
				, checkdirect locs
				)
			else checkdirect locs

{- A safer check; the key's content must not only be present, but
 - is not in the process of being removed. -}
inAnnexSafe :: Key -> Annex (Maybe Bool)
inAnnexSafe key = inAnnex' (fromMaybe False) (Just False) go key
  where
	is_locked = Nothing
	is_unlocked = Just True
	is_missing = Just False

	go contentfile = maybe (checkindirect contentfile) (checkdirect contentfile)
		=<< contentLockFile key

#ifndef mingw32_HOST_OS
	checkindirect f = liftIO $ openforlock f >>= check is_missing
	{- In direct mode, the content file must exist, but
	 - the lock file often generally won't exist unless a removal is in
	 - process. This does not create the lock file, it only checks for
	 - it. -}
	checkdirect contentfile lockfile = liftIO $
		ifM (doesFileExist contentfile)
			( openforlock lockfile >>= check is_unlocked
			, return is_missing
			)
	openforlock f = catchMaybeIO $
		openFd f ReadOnly Nothing defaultFileFlags
	check _ (Just h) = do
		v <- getLock h (ReadLock, AbsoluteSeek, 0, 0)
		closeFd h
		return $ case v of
			Just _ -> is_locked
			Nothing -> is_unlocked
	check def Nothing = return def
#else
	checkindirect _ = return is_missing
	{- In Windows, see if we can take a shared lock. If so, 
	 - remove the lock file to clean up after ourselves. -}
	checkdirect contentfile lockfile =
		ifM (liftIO $ doesFileExist contentfile)
			( modifyContent lockfile $ liftIO $ do
				v <- lockShared lockfile
				case v of
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
contentLockFile key = ifM isDirect
	( Just <$> calcRepo (gitAnnexContentLock key)
	, return Nothing
	)

{- Content is exclusively locked while running an action that might remove
 - it. (If the content is not present, no locking is done.) -}
lockContent :: Key -> Annex a -> Annex a
lockContent key a = do
	contentfile <- calcRepo $ gitAnnexLocation key
	lockfile <- contentLockFile key
	maybe noop setuplockfile lockfile
	bracketAnnex (liftIO $ lock contentfile lockfile) (unlock lockfile) (const a)
  where
	alreadylocked = error "content is locked"
	setuplockfile lockfile = modifyContent lockfile $
		void $ liftIO $ tryIO $
			writeFile lockfile ""
	cleanuplockfile lockfile = modifyContent lockfile $
		void $ liftIO $ tryIO $
			nukeFile lockfile
#ifndef mingw32_HOST_OS
	lock contentfile Nothing = opencontentforlock contentfile >>= dolock
	lock _ (Just lockfile) = openforlock lockfile >>= dolock . Just
	{- Since content files are stored with the write bit disabled, have
	 - to fiddle with permissions to open for an exclusive lock. -}
	opencontentforlock f = catchMaybeIO $ ifM (doesFileExist f)
		( withModifiedFileMode f
			(`unionFileModes` ownerWriteMode)
			(openforlock f)
		, openforlock f
		)
	openforlock f = openFd f ReadWrite Nothing defaultFileFlags
	dolock Nothing = return Nothing
	dolock (Just fd) = do
		v <- tryIO $ setLock fd (WriteLock, AbsoluteSeek, 0, 0)
		case v of
			Left _ -> alreadylocked
			Right _ -> return $ Just fd
	unlock mlockfile mfd = do
		maybe noop cleanuplockfile mlockfile
		liftIO $ maybe noop closeFd mfd
#else
	lock _ (Just lockfile) = maybe alreadylocked (return . Just) =<< lockExclusive lockfile
	lock _ Nothing = return Nothing
	unlock mlockfile mlockhandle = do
		liftIO $ maybe noop dropLock mlockhandle
		maybe noop cleanuplockfile mlockfile
#endif

{- Runs an action, passing it a temporary filename to get,
 - and if the action succeeds, moves the temp file into 
 - the annex as a key's content. -}
getViaTmp :: Key -> (FilePath -> Annex Bool) -> Annex Bool
getViaTmp = getViaTmpChecked (return True)

{- Like getViaTmp, but does not check that there is enough disk space
 - for the incoming key. For use when the key content is already on disk
 - and not being copied into place. -}
getViaTmpUnchecked :: Key -> (FilePath -> Annex Bool) -> Annex Bool
getViaTmpUnchecked = finishGetViaTmp (return True)

getViaTmpChecked :: Annex Bool -> Key -> (FilePath -> Annex Bool) -> Annex Bool
getViaTmpChecked check key action = 
	prepGetViaTmpChecked key $
		finishGetViaTmp check key action

{- Prepares to download a key via a tmp file, and checks that there is
 - enough free disk space.
 -
 - When the temp file already exists, count the space it is using as
 - free, since the download will overwrite it or resume.
 -
 - Wen there's enough free space, runs the download action.
 -}
prepGetViaTmpChecked :: Key -> Annex Bool -> Annex Bool
prepGetViaTmpChecked key getkey = do
	tmp <- fromRepo $ gitAnnexTmpLocation key

	e <- liftIO $ doesFileExist tmp
	alreadythere <- if e
		then fromIntegral . fileSize <$> liftIO (getFileStatus tmp)
		else return 0
	ifM (checkDiskSpace Nothing key alreadythere)
		( do
			-- The tmp file may not have been left writable
			when e $ thawContent tmp
			getkey
		, return False
		)

finishGetViaTmp :: Annex Bool -> Key -> (FilePath -> Annex Bool) -> Annex Bool
finishGetViaTmp check key action = do
	tmpfile <- prepTmp key
	ifM (action tmpfile <&&> check)
		( do
			moveAnnex key tmpfile
			logStatus key InfoPresent
			return True
		-- the tmp file is left behind, in case caller wants
		-- to resume its transfer
		, return False
		)

prepTmp :: Key -> Annex FilePath
prepTmp key = do
	tmp <- fromRepo $ gitAnnexTmpLocation key
	createAnnexDirectory (parentDir tmp)
	return tmp

{- Creates a temp file, runs an action on it, and cleans up the temp file. -}
withTmp :: Key -> (FilePath -> Annex a) -> Annex a
withTmp key action = do
	tmp <- prepTmp key
	res <- action tmp
	liftIO $ nukeFile tmp
	return res

{- Checks that there is disk space available to store a given key,
 - in a destination (or the annex) printing a warning if not. -}
checkDiskSpace :: Maybe FilePath -> Key -> Integer -> Annex Bool
checkDiskSpace destination key alreadythere = do
	reserve <- annexDiskReserve <$> Annex.getGitConfig
	free <- liftIO . getDiskFree =<< dir
	force <- Annex.getState Annex.force
	case (free, keySize key) of
		(Just have, Just need) -> do
			let ok = (need + reserve <= have + alreadythere) || force
			unless ok $
				needmorespace (need + reserve - have - alreadythere)
			return ok
		_ -> return True
  where
	dir = maybe (fromRepo gitAnnexDir) return destination
	needmorespace n =
		warning $ "not enough free space, need " ++ 
			roughSize storageUnits True n ++
			" more" ++ forcemsg
	forcemsg = " (use --force to override this check or adjust annex.diskreserve)"

{- Moves a key's content into .git/annex/objects/
 -
 - In direct mode, moves it to the associated file, or files.
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
 -}
moveAnnex :: Key -> FilePath -> Annex ()
moveAnnex key src = withObjectLoc key storeobject storedirect
  where
	storeobject dest = ifM (liftIO $ doesFileExist dest)
		( alreadyhave
		, modifyContent dest $ do
			liftIO $ moveFile src dest
			freezeContent dest
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
				updateInodeCache key src
				replaceFile f $ liftIO . moveFile src
				chmodContent f
				forM_ fs $
					addContentWhenNotPresent key f
			else ifM (goodContent key f)
				( storedirect' alreadyhave fs
				, storedirect' fallback fs
				)
	
	alreadyhave = liftIO $ removeFile src

{- Runs an action to transfer an object's content.
 -
 - In direct mode, it's possible for the file to change as it's being sent.
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
 - and an check to run after the transfer is complete.
 -
 - In direct mode, it's possible for the file to change as it's being sent,
 - and the check detects this case and returns False.
 -
 - Note that the returned check action is, in some cases, run in the
 - Annex monad of the remote that is receiving the object, rather than
 - the sender. So it cannot rely on Annex state.
 -}
prepSendAnnex :: Key -> Annex (Maybe (FilePath, Annex Bool))
prepSendAnnex key = withObjectLoc key indirect direct
  where
	indirect f = return $ Just (f, return True)
	direct [] = return Nothing
	direct (f:fs) = do
		cache <- recordedInodeCache key
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
		fs <- associatedFiles key
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
	void $ tryAnnexIO $ thawContentDir file
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
 - In direct mode, deletes the associated files or files, and replaces
 - them with symlinks. -}
removeAnnex :: Key -> Annex ()
removeAnnex key = withObjectLoc key remove removedirect
  where
	remove file = cleanObjectLoc key $ do
		secureErase file
		liftIO $ nukeFile file
		removeInodeCache key
	removedirect fs = do
		cache <- recordedInodeCache key
		removeInodeCache key
		mapM_ (resetfile cache) fs
	resetfile cache f = whenM (sameInodeCache f cache) $ do
		l <- inRepo $ gitAnnexLink f key
		secureErase f
		replaceFile f $ makeAnnexLink l

{- Runs the secure erase command if set, otherwise does nothing.
 - File may or may not be deleted at the end; caller is responsible for
 - making sure it's deleted. -}
secureErase :: FilePath -> Annex ()
secureErase file = maybe noop go =<< annexSecureEraseCommand <$> Annex.getGitConfig
  where
  	go basecmd = void $ liftIO $
		boolSystem "sh" [Param "-c", Param $ gencmd basecmd]
	gencmd = massReplace [ ("%file", shellEscape file) ]

{- Moves a key's file out of .git/annex/objects/ -}
fromAnnex :: Key -> FilePath -> Annex ()
fromAnnex key dest = cleanObjectLoc key $ do
	file <- calcRepo $ gitAnnexLocation key
	thawContent file
	liftIO $ moveFile file dest

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

{- List of keys whose content exists in the annex. -}
getKeysPresent :: Annex [Key]
getKeysPresent = do
	direct <- isDirect
	dir <- fromRepo gitAnnexObjectDir
	liftIO $ traverse direct (2 :: Int) dir
  where
	traverse direct depth dir = do
		contents <- catchDefaultIO [] (dirContents dir)
		if depth == 0
			then do
				contents' <- filterM (present direct) contents
				let keys = mapMaybe (fileKey . takeFileName) contents'
				continue keys []
			else do
				let deeper = traverse direct (depth - 1)
				continue [] (map deeper contents)
	continue keys [] = return keys
	continue keys (a:as) = do
		{- Force lazy traversal with unsafeInterleaveIO. -}
		morekeys <- unsafeInterleaveIO a
		continue (morekeys++keys) as

	{- In indirect mode, look for the key. In direct mode,
	 - the inode cache file is only present when a key's content
	 - is present, so can be used as a surrogate if the content
	 - is not located in the annex directory. -}
	present False d = doesFileExist $ contentfile d
	present True d = doesFileExist (contentfile d ++ ".cache")
		<||> present False d
	contentfile d = d </> takeFileName d

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
downloadUrl :: [Url.URLString] -> FilePath -> Annex Bool
downloadUrl urls file = go =<< annexWebDownloadCommand <$> Annex.getGitConfig
  where
  	go Nothing = do
		opts <- map Param . annexWebOptions <$> Annex.getGitConfig
		headers <- getHttpHeaders
		anyM (\u -> Url.withUserAgent $ Url.download u headers opts file) urls
	go (Just basecmd) = liftIO $ anyM (downloadcmd basecmd) urls
	downloadcmd basecmd url =
		boolSystem "sh" [Param "-c", Param $ gencmd url basecmd]
			<&&> doesFileExist file
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
			liftIO $ copyFileExternal s file
		)

{- Blocks writing to an annexed file, and modifies file permissions to
 - allow reading it, per core.sharedRepository setting. -}
freezeContent :: FilePath -> Annex ()
freezeContent file = unlessM crippledFileSystem $
	liftIO . go =<< fromRepo getSharedRepository
  where
	go GroupShared = modifyFileMode file $
		removeModes writeModes .
		addModes [ownerReadMode, groupReadMode]
	go AllShared = modifyFileMode file $
		removeModes writeModes .
		addModes readModes
	go _ = modifyFileMode file $
		removeModes writeModes .
		addModes [ownerReadMode]

{- Adjusts read mode of annexed file per core.sharedRepository setting. -}
chmodContent :: FilePath -> Annex ()
chmodContent file = unlessM crippledFileSystem $
	liftIO . go =<< fromRepo getSharedRepository
  where
	go GroupShared = modifyFileMode file $
		addModes [ownerReadMode, groupReadMode]
	go AllShared = modifyFileMode file $
		addModes readModes
	go _ = modifyFileMode file $
		addModes [ownerReadMode]

{- Allows writing to an annexed file that freezeContent was called on
 - before. -}
thawContent :: FilePath -> Annex ()
thawContent file = unlessM crippledFileSystem $
	liftIO . go =<< fromRepo getSharedRepository
  where
	go GroupShared = groupWriteRead file
	go AllShared = groupWriteRead file
	go _ = allowWrite file

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

