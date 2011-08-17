{- git-annex file content managing
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Content (
	inAnnex,
	calcGitLink,
	logStatus,
	getViaTmp,
	getViaTmpUnchecked,
	withTmp,
	checkDiskSpace,
	preventWrite,
	allowWrite,
	moveAnnex,
	removeAnnex,
	fromAnnex,
	moveBad,
	getKeysPresent,
	saveState
) where

import System.IO.Error (try)
import System.Directory
import Control.Monad.State (liftIO)
import System.Path
import Control.Monad (when, filterM)
import System.Posix.Files
import System.FilePath
import Data.Maybe

import Types
import Locations
import LocationLog
import UUID
import qualified Git
import qualified Annex
import qualified AnnexQueue
import qualified Branch
import Utility
import StatFS
import Types.Key
import Utility.DataUnits
import Config

{- Checks if a given key is currently present in the gitAnnexLocation. -}
inAnnex :: Key -> Annex Bool
inAnnex key = do
	g <- Annex.gitRepo
	when (Git.repoIsUrl g) $ error "inAnnex cannot check remote repo"
	liftIO $ doesFileExist $ gitAnnexLocation g key

{- Calculates the relative path to use to link a file to a key. -}
calcGitLink :: FilePath -> Key -> Annex FilePath
calcGitLink file key = do
	g <- Annex.gitRepo
	cwd <- liftIO getCurrentDirectory
	let absfile = fromMaybe whoops $ absNormPath cwd file
	return $ relPathDirToFile (parentDir absfile) 
			(Git.workTree g) </> ".git" </> annexLocation key
	where
		whoops = error $ "unable to normalize " ++ file

{- Updates the LocationLog when a key's presence changes in the current
 - repository. -}
logStatus :: Key -> LogStatus -> Annex ()
logStatus key status = do
	g <- Annex.gitRepo
	u <- getUUID g
	logChange g key u status

{- Runs an action, passing it a temporary filename to download,
 - and if the action succeeds, moves the temp file into 
 - the annex as a key's content. -}
getViaTmp :: Key -> (FilePath -> Annex Bool) -> Annex Bool
getViaTmp key action = do
	g <- Annex.gitRepo
	let tmp = gitAnnexTmpLocation g key

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
	g <- Annex.gitRepo
	let tmp = gitAnnexTmpLocation g key
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
	g <- Annex.gitRepo
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

{- Removes the write bits from a file. -}
preventWrite :: FilePath -> IO ()
preventWrite f = unsetFileMode f writebits
	where
		writebits = foldl unionFileModes ownerWriteMode
					[groupWriteMode, otherWriteMode]

{- Turns a file's write bit back on. -}
allowWrite :: FilePath -> IO ()
allowWrite f = do
	s <- getFileStatus f
	setFileMode f $ fileMode s `unionFileModes` ownerWriteMode

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
	g <- Annex.gitRepo
	let dest = gitAnnexLocation g key
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
	g <- Annex.gitRepo
	let file = gitAnnexLocation g key
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
	g <- Annex.gitRepo
	let src = gitAnnexLocation g key
	let dest = gitAnnexBadDir g </> takeFileName src
	liftIO $ do
		createDirectoryIfMissing True (parentDir dest)
		allowWrite (parentDir src)
		renameFile src dest
		removeDirectory (parentDir src)
	logStatus key InfoMissing
	return dest

{- List of keys whose content exists in .git/annex/objects/ -}
getKeysPresent :: Annex [Key]
getKeysPresent = do
	g <- Annex.gitRepo
	getKeysPresent' $ gitAnnexObjectDir g
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
			files <- filterM present (concat contents)
			return $ mapMaybe (fileKey . takeFileName) files
	where
		present d = do
			result <- try $
				getFileStatus $ d </> takeFileName d
			case result of
				Right s -> return $ isRegularFile s
				Left _ -> return False

{- Things to do to record changes to content. -}
saveState :: Annex ()
saveState = do
	AnnexQueue.flush False
	Branch.commit "update"
