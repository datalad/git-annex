{- git-annex file content managing
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Annex.Content (
	inAnnex,
	inAnnexSafe,
	lockContent,
	calcGitLink,
	logStatus,
	getViaTmp,
	getViaTmpUnchecked,
	withTmp,
	checkDiskSpace,
	moveAnnex,
	removeAnnex,
	fromAnnex,
	moveBad,
	getKeysPresent,
	saveState,
	downloadUrl,
	preseedTmp,
) where

import Control.Exception (bracket_)
import System.Posix.Types
import System.IO.Unsafe (unsafeInterleaveIO)

import Common.Annex
import Logs.Location
import Annex.UUID
import qualified Git
import qualified Annex
import qualified Annex.Queue
import qualified Annex.Branch
import Utility.DiskFree
import Utility.FileMode
import qualified Utility.Url as Url
import Types.Key
import Utility.DataUnits
import Utility.CopyFile
import Config
import Annex.Exception

{- Checks if a given key's content is currently present. -}
inAnnex :: Key -> Annex Bool
inAnnex = inAnnex' doesFileExist
inAnnex' :: (FilePath -> IO a) -> Key -> Annex a
inAnnex' a key = do
	whenM (fromRepo Git.repoIsUrl) $
		error "inAnnex cannot check remote repo"
	inRepo $ \g -> gitAnnexLocation key g >>= a

{- A safer check; the key's content must not only be present, but
 - is not in the process of being removed. -}
inAnnexSafe :: Key -> Annex (Maybe Bool)
inAnnexSafe = inAnnex' $ \f -> openForLock f False >>= check
	where
		check Nothing = return is_missing
		check (Just h) = do
			v <- getLock h (ReadLock, AbsoluteSeek, 0, 0)
			closeFd h
			return $ case v of
				Just _ -> is_locked
				Nothing -> is_unlocked
		is_locked = Nothing
		is_unlocked = Just True
		is_missing = Just False

{- Content is exclusively locked while running an action that might remove
 - it. (If the content is not present, no locking is done.) -}
lockContent :: Key -> Annex a -> Annex a
lockContent key a = do
	file <- inRepo $ gitAnnexLocation key
	bracketIO (openForLock file True >>= lock) unlock a
	where
		lock Nothing = return Nothing
		lock (Just l) = do
			v <- tryIO $ setLock l (WriteLock, AbsoluteSeek, 0, 0)
			case v of
				Left _ -> error "content is locked"
				Right _ -> return $ Just l
		unlock Nothing = return ()
		unlock (Just l) = closeFd l

openForLock :: FilePath -> Bool -> IO (Maybe Fd)
openForLock file writelock = bracket_ prep cleanup go
	where
		go = catchMaybeIO $ openFd file mode Nothing defaultFileFlags
		mode = if writelock then ReadWrite else ReadOnly
		{- Since files are stored with the write bit disabled,
		 - have to fiddle with permissions to open for an
		 - exclusive lock. -}
		forwritelock a = 
			when writelock $ whenM (doesFileExist file) a
		prep = forwritelock $ allowWrite file
		cleanup = forwritelock $ preventWrite file

{- Calculates the relative path to use to link a file to a key. -}
calcGitLink :: FilePath -> Key -> Annex FilePath
calcGitLink file key = do
	cwd <- liftIO getCurrentDirectory
	let absfile = fromMaybe whoops $ absNormPath cwd file
	loc <- inRepo $ gitAnnexLocation key
	return $ relPathDirToFile (parentDir absfile) loc
	where
		whoops = error $ "unable to normalize " ++ file

{- Updates the Logs.Location when a key's presence changes in the current
 - repository. -}
logStatus :: Key -> LogStatus -> Annex ()
logStatus key status = do
	u <- getUUID
	logChange key u status

{- Runs an action, passing it a temporary filename to get,
 - and if the action succeeds, moves the temp file into 
 - the annex as a key's content. -}
getViaTmp :: Key -> (FilePath -> Annex Bool) -> Annex Bool
getViaTmp key action = do
	tmp <- fromRepo $ gitAnnexTmpLocation key

	-- Check that there is enough free disk space.
	-- When the temp file already exists, count the space
	-- it is using as free.
	e <- liftIO $ doesFileExist tmp
	if e
		then do
			stat <- liftIO $ getFileStatus tmp
			checkDiskSpace' (fromIntegral $ fileSize stat) key
		else checkDiskSpace key

	when e $ liftIO $ allowWrite tmp

	getViaTmpUnchecked key action

prepTmp :: Key -> Annex FilePath
prepTmp key = do
	tmp <- fromRepo $ gitAnnexTmpLocation key
	liftIO $ createDirectoryIfMissing True (parentDir tmp)
	return tmp

{- Like getViaTmp, but does not check that there is enough disk space
 - for the incoming key. For use when the key content is already on disk
 - and not being copied into place. -}
getViaTmpUnchecked :: Key -> (FilePath -> Annex Bool) -> Annex Bool
getViaTmpUnchecked key action = do
	tmp <- prepTmp key
	ifM (action tmp)
		( do
			moveAnnex key tmp
			logStatus key InfoPresent
			return True
		, do
			-- the tmp file is left behind, in case caller wants
			-- to resume its transfer
			return False
		)

{- Creates a temp file, runs an action on it, and cleans up the temp file. -}
withTmp :: Key -> (FilePath -> Annex a) -> Annex a
withTmp key action = do
	tmp <- prepTmp key
	res <- action tmp
	liftIO $ whenM (doesFileExist tmp) $ liftIO $ removeFile tmp
	return res

{- Checks that there is disk space available to store a given key,
 - throwing an error if not. -}
checkDiskSpace :: Key -> Annex ()
checkDiskSpace = checkDiskSpace' 0

checkDiskSpace' :: Integer -> Key -> Annex ()
checkDiskSpace' adjustment key = do
	reserve <- getDiskReserve
	free <- inRepo $ getDiskFree . gitAnnexDir
	case (free, keySize key) of
		(Just have, Just need) ->
			when (need + reserve > have + adjustment) $
				needmorespace (need + reserve - have - adjustment)
		_ -> return ()
	where
		needmorespace n = unlessM (Annex.getState Annex.force) $
			error $ "not enough free space, need " ++ 
				roughSize storageUnits True n ++
				" more" ++ forcemsg
		forcemsg = " (use --force to override this check or adjust annex.diskreserve)"

{- Moves a file into .git/annex/objects/
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
moveAnnex key src = do
	dest <- inRepo $ gitAnnexLocation key
	let dir = parentDir dest
	liftIO $ ifM (doesFileExist dest)
		( removeFile src
		, do
			createDirectoryIfMissing True dir
			allowWrite dir -- in case the directory already exists
			moveFile src dest
			preventWrite dest
			preventWrite dir
		)

withObjectLoc :: Key -> ((FilePath, FilePath) -> Annex a) -> Annex a
withObjectLoc key a = do
	file <- inRepo $ gitAnnexLocation key
	let dir = parentDir file
	a (dir, file)

cleanObjectLoc :: Key -> Annex ()
cleanObjectLoc key = do
	file <- inRepo $ gitAnnexLocation key
	liftIO $ removeparents file (3 :: Int)
	where
		removeparents _ 0 = return ()
		removeparents file n = do
			let dir = parentDir file
			maybe (return ()) (const $ removeparents dir (n-1))
				=<< catchMaybeIO (removeDirectory dir)

{- Removes a key's file from .git/annex/objects/ -}
removeAnnex :: Key -> Annex ()
removeAnnex key = withObjectLoc key $ \(dir, file) -> do
	liftIO $ do
		allowWrite dir
		removeFile file
	cleanObjectLoc key

{- Moves a key's file out of .git/annex/objects/ -}
fromAnnex :: Key -> FilePath -> Annex ()
fromAnnex key dest = withObjectLoc key $ \(dir, file) -> do
	liftIO $ do
		allowWrite dir
		allowWrite file
		moveFile file dest
	cleanObjectLoc key

{- Moves a key out of .git/annex/objects/ into .git/annex/bad, and
 - returns the file it was moved to. -}
moveBad :: Key -> Annex FilePath
moveBad key = do
	src <- inRepo $ gitAnnexLocation key
	bad <- fromRepo gitAnnexBadDir
	let dest = bad </> takeFileName src
	liftIO $ do
		createDirectoryIfMissing True (parentDir dest)
		allowWrite (parentDir src)
		moveFile src dest
	cleanObjectLoc key
	logStatus key InfoMissing
	return dest

{- List of keys whose content exists in .git/annex/objects/ -}
getKeysPresent :: Annex [Key]
getKeysPresent = liftIO . traverse (2 :: Int) =<< fromRepo gitAnnexObjectDir
	where
		traverse depth dir = do
			contents <- catchDefaultIO (dirContents dir) []
			if depth == 0
				then continue (mapMaybe (fileKey . takeFileName) contents) []
				else do
					let deeper = traverse (depth - 1)
					continue [] (map deeper contents)
		continue keys [] = return keys
		continue keys (a:as) = do
			{- Force lazy traversal with unsafeInterleaveIO. -}
			morekeys <- unsafeInterleaveIO a
			continue (morekeys++keys) as

{- Things to do to record changes to content when shutting down.
 -
 - It's acceptable to avoid committing changes to the branch,
 - especially if performing a short-lived action.
 -}
saveState :: Bool -> Annex ()
saveState oneshot = do
	Annex.Queue.flush False
	unless oneshot $
		ifM alwayscommit
			( Annex.Branch.commit "update" , Annex.Branch.stage)
	where
		alwayscommit = fromMaybe True . Git.configTrue
			<$> getConfig "annex.alwayscommit" ""

{- Downloads content from any of a list of urls. -}
downloadUrl :: [Url.URLString] -> FilePath -> Annex Bool
downloadUrl urls file = do
	o <- map Param . words <$> getConfig "annex.web-options" ""
	liftIO $ anyM (\u -> Url.download u o file) urls

{- Copies a key's content, when present, to a temp file.
 - This is used to speed up some rsyncs. -}
preseedTmp :: Key -> FilePath -> Annex Bool
preseedTmp key file = go =<< inAnnex key
	where
		go False = return False
		go True = do
			ok <- copy
			when ok $ liftIO $ allowWrite file
			return ok
		copy = ifM (liftIO $ doesFileExist file)
				( return True
				, do
					s <- inRepo $ gitAnnexLocation key
					liftIO $ copyFileExternal s file
				)
