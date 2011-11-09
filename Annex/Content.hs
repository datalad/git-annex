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
	saveState
) where

import Common.Annex
import Logs.Location
import Annex.UUID
import qualified Git
import qualified Annex
import qualified Annex.Queue
import qualified Annex.Branch
import Utility.StatFS
import Utility.FileMode
import Types.Key
import Utility.DataUnits
import Config

{- Checks if a given key's content is currently present. -}
inAnnex :: Key -> Annex Bool
inAnnex = inAnnex' doesFileExist
inAnnex' :: (FilePath -> IO a) -> Key -> Annex a
inAnnex' a key = do
	whenM (fromRepo Git.repoIsUrl) $
		error "inAnnex cannot check remote repo"
	inRepo $ a . gitAnnexLocation key

{- A safer check; the key's content must not only be present, but
 - is not in the process of being removed. -}
inAnnexSafe :: Key -> Annex (Maybe Bool)
inAnnexSafe = inAnnex' $ \f -> do
	e <- doesFileExist f
	if e
		then do
			locked <- testlock f
			if locked
				then return Nothing
				else return $ Just True
		else return $ Just False
	where
		testlock f = return False -- TODO

{- Content is exclusively locked to indicate that it's in the process of
 - being removed. -}
lockContent :: Key -> Annex a -> Annex a
lockContent key a = a -- TODO

{- Calculates the relative path to use to link a file to a key. -}
calcGitLink :: FilePath -> Key -> Annex FilePath
calcGitLink file key = do
	cwd <- liftIO getCurrentDirectory
	let absfile = fromMaybe whoops $ absNormPath cwd file
	top <- fromRepo Git.workTree
	return $ relPathDirToFile (parentDir absfile) 
			top </> ".git" </> annexLocation key
	where
		whoops = error $ "unable to normalize " ++ file

{- Updates the Logs.Location when a key's presence changes in the current
 - repository. -}
logStatus :: Key -> LogStatus -> Annex ()
logStatus key status = do
	u <- getUUID
	logChange key u status

{- Runs an action, passing it a temporary filename to download,
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
	success <- action tmp
	if success
		then do
			moveAnnex key tmp
			logStatus key InfoPresent
			return True
		else do
			-- the tmp file is left behind, in case caller wants
			-- to resume its transfer
			return False

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
	g <- gitRepo
	r <- getConfig g "diskreserve" ""
	let reserve = fromMaybe megabyte $ readSize dataUnits r
	stats <- liftIO $ getFileSystemStats (gitAnnexDir g)
	case (stats, keySize key) of
		(Nothing, _) -> return ()
		(_, Nothing) -> return ()
		(Just (FileSystemStats { fsStatBytesAvailable = have }), Just need) ->
			when (need + reserve > have + adjustment) $
				needmorespace (need + reserve - have - adjustment)
	where
		megabyte :: Integer
		megabyte = 1000000
		needmorespace n = unlessM (Annex.getState Annex.force) $
			error $ "not enough free space, need " ++ 
				roughSize storageUnits True n ++
				" more (use --force to override this check or adjust annex.diskreserve)"

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
	dest <- fromRepo $ gitAnnexLocation key
	let dir = parentDir dest
	e <- liftIO $ doesFileExist dest
	if e
		then liftIO $ removeFile src
		else liftIO $ do
			createDirectoryIfMissing True dir
			allowWrite dir -- in case the directory already exists
			renameFile src dest
			preventWrite dest
			preventWrite dir

withObjectLoc :: Key -> ((FilePath, FilePath) -> Annex a) -> Annex a
withObjectLoc key a = do
	file <- fromRepo $gitAnnexLocation key
	let dir = parentDir file
	a (dir, file)

{- Removes a key's file from .git/annex/objects/ -}
removeAnnex :: Key -> Annex ()
removeAnnex key = withObjectLoc key $ \(dir, file) -> liftIO $ do
	allowWrite dir
	removeFile file
	removeDirectory dir

{- Moves a key's file out of .git/annex/objects/ -}
fromAnnex :: Key -> FilePath -> Annex ()
fromAnnex key dest = withObjectLoc key $ \(dir, file) -> liftIO $ do
	allowWrite dir
	allowWrite file
	renameFile file dest
	removeDirectory dir

{- Moves a key out of .git/annex/objects/ into .git/annex/bad, and
 - returns the file it was moved to. -}
moveBad :: Key -> Annex FilePath
moveBad key = do
	src <- fromRepo $ gitAnnexLocation key
	bad <- fromRepo $ gitAnnexBadDir
	let dest = bad </> takeFileName src
	liftIO $ do
		createDirectoryIfMissing True (parentDir dest)
		allowWrite (parentDir src)
		renameFile src dest
		removeDirectory (parentDir src)
	logStatus key InfoMissing
	return dest

{- List of keys whose content exists in .git/annex/objects/ -}
getKeysPresent :: Annex [Key]
getKeysPresent = getKeysPresent' =<< fromRepo gitAnnexObjectDir
getKeysPresent' :: FilePath -> Annex [Key]
getKeysPresent' dir = do
	exists <- liftIO $ doesDirectoryExist dir
	if not exists
		then return []
		else liftIO $ do
			-- 2 levels of hashing
			levela <- dirContents dir
			levelb <- mapM dirContents levela
			contents <- mapM dirContents (concat levelb)
			let files = concat contents
			return $ mapMaybe (fileKey . takeFileName) files

{- Things to do to record changes to content. -}
saveState :: Annex ()
saveState = do
	Annex.Queue.flush False
	Annex.Branch.commit "update"
