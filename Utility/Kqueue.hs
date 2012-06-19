{- BSD kqueue file modification notification interface
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE ForeignFunctionInterface #-}

module Utility.Kqueue (
	Kqueue,
	initKqueue,
	stopKqueue,
	waitChange,
	Change(..),
	changedFile,
	isAdd,
	isDelete,
	runHooks,
) where

import Common
import Utility.Types.DirWatcher

import System.Posix.Types
import Foreign.C.Types
import Foreign.C.Error
import Foreign.Ptr
import Foreign.Marshal
import qualified Data.Map as M
import qualified Data.Set as S
import qualified System.Posix.Files as Files
import Control.Concurrent

data Change
	= Deleted FilePath
	| Added FilePath
	deriving (Show)

isAdd :: Change -> Bool
isAdd (Added _) = True
isAdd (Deleted _) = False

isDelete :: Change -> Bool
isDelete = not . isAdd

changedFile :: Change -> FilePath
changedFile (Added f) = f
changedFile (Deleted f) = f

data Kqueue = Kqueue Fd DirMap Pruner

type Pruner = FilePath -> Bool

type DirMap = M.Map Fd DirInfo

{- A directory, and its last known contents (with filenames relative to it) -}
data DirInfo = DirInfo
	{ dirName :: FilePath
	, dirCache :: S.Set FilePath
	}
	deriving (Show)

getDirInfo :: FilePath -> IO DirInfo
getDirInfo dir = do
	contents <- S.fromList . filter (not . dirCruft)
		<$> getDirectoryContents dir
	return $ DirInfo dir contents

{- Difference between the dirCaches of two DirInfos. -}
(//) :: DirInfo -> DirInfo -> [Change]
oldc // newc = deleted ++ added
	where
		deleted = calc Deleted oldc newc
		added   = calc Added newc oldc
		calc a x y = map a . map (dirName x </>) $
			S.toList $ S.difference (dirCache x) (dirCache y)

{- Builds a map of directories in a tree, possibly pruning some.
 - Opens each directory in the tree, and records its current contents. -}
scanRecursive :: FilePath -> Pruner -> IO DirMap
scanRecursive topdir prune = M.fromList <$> walk [] [topdir]
	where
		walk c [] = return c
		walk c (dir:rest)
			| prune dir = walk c rest
			| otherwise = do
				minfo <- catchMaybeIO $ getDirInfo dir
				case minfo of
					Nothing -> walk c rest
					Just info -> do
						mfd <- catchMaybeIO $
							openFd dir ReadOnly Nothing defaultFileFlags
						case mfd of
							Nothing -> walk c rest
							Just fd -> do
								let subdirs = map (dir </>) $
									S.toList $ dirCache info
								walk ((fd, info):c) (subdirs ++ rest)

{- Adds a list of subdirectories (and all their children), unless pruned to a
 - directory map. Adding a subdirectory that's already in the map will
 - cause its contents to be refreshed. -}
addSubDirs :: DirMap -> Pruner -> [FilePath] -> IO DirMap
addSubDirs dirmap prune dirs = do
	newmap <- foldr M.union M.empty <$>
		mapM (\d -> scanRecursive d prune) dirs
	return $ M.union newmap dirmap -- prefer newmap

{- Removes a subdirectory (and all its children) from a directory map. -}
removeSubDir :: DirMap -> FilePath -> IO DirMap
removeSubDir dirmap dir = do
	mapM_ closeFd $ M.keys toremove
	return rest
	where
		(toremove, rest) = M.partition (dirContains dir . dirName) dirmap

foreign import ccall unsafe "libkqueue.h init_kqueue" c_init_kqueue
	:: IO Fd
foreign import ccall unsafe "libkqueue.h addfds_kqueue" c_addfds_kqueue
	:: Fd -> CInt -> Ptr Fd -> IO ()
foreign import ccall unsafe "libkqueue.h waitchange_kqueue" c_waitchange_kqueue
	:: Fd -> IO Fd

{- Initializes a Kqueue to watch a directory, and all its subdirectories. -}
initKqueue :: FilePath -> Pruner -> IO Kqueue
initKqueue dir pruned = do
	dirmap <- scanRecursive dir pruned
	h <- c_init_kqueue
	let kq = Kqueue h dirmap pruned
	updateKqueue kq
	return kq

{- Updates a Kqueue, adding watches for its map. -}
updateKqueue :: Kqueue -> IO ()
updateKqueue (Kqueue h dirmap _) =
	withArrayLen (M.keys dirmap) $ \fdcnt c_fds -> do
		c_addfds_kqueue h (fromIntegral fdcnt) c_fds

{- Stops a Kqueue. Note: Does not directly close the Fds in the dirmap,
 - so it can be reused.  -}
stopKqueue :: Kqueue -> IO ()
stopKqueue (Kqueue h _ _) = closeFd h

{- Waits for a change on a Kqueue.
 - May update the Kqueue.
 -}
waitChange :: Kqueue -> IO (Kqueue, [Change])
waitChange kq@(Kqueue h dirmap _) = do
	changedfd <- c_waitchange_kqueue h
	if changedfd == -1
		then ifM ((==) eINTR <$> getErrno)
			(yield >> waitChange kq, nochange)
		else case M.lookup changedfd dirmap of
			Nothing -> nochange
			Just info -> handleChange kq changedfd info
	where
		nochange = return (kq, [])

{- The kqueue interface does not tell what type of change took place in
 - the directory; it could be an added file, a deleted file, a renamed
 - file, a new subdirectory, or a deleted subdirectory, or a moved
 - subdirectory. 
 -
 - So to determine this, the contents of the directory are compared
 - with its last cached contents. The Kqueue is updated to watch new
 - directories as necessary.
 -}
handleChange :: Kqueue -> Fd -> DirInfo -> IO (Kqueue, [Change])
handleChange (Kqueue h dirmap pruner) fd olddirinfo =
	go =<< catchMaybeIO (getDirInfo $ dirName olddirinfo)
	where
		go (Just newdirinfo) = do
			let changes = olddirinfo // newdirinfo
			let (added, deleted) = partition isAdd changes

			-- Scan newly added directories to add to the map.
			-- (Newly added files will fail getDirInfo.)
			newdirinfos <- catMaybes <$>
				mapM (catchMaybeIO . getDirInfo . changedFile) added
			newmap <- addSubDirs dirmap pruner $ map dirName newdirinfos

			-- Remove deleted directories from the map.
			newmap' <- foldM removeSubDir newmap (map changedFile deleted)

			-- Update the cached dirinfo just looked up.
			let newmap'' = M.insertWith' const fd newdirinfo newmap'

			-- When new directories were added, need to update
			-- the kqueue to watch them.
			let kq' = Kqueue h newmap'' pruner
			unless (null newdirinfos) $
				updateKqueue kq'

			return (kq', changes)
		go Nothing = do
			-- The directory has been moved or deleted, so
			-- remove it from our map.
			newmap <- removeSubDir dirmap (dirName olddirinfo)
			return (Kqueue h newmap pruner, [])

{- Processes changes on the Kqueue, calling the hooks as appropriate.
 - Never returns. -}
runHooks :: Kqueue -> WatchHooks -> IO ()
runHooks kq hooks = do
	(kq', changes) <- waitChange kq
	forM_ changes $ \c -> do
		print c
		dispatch kq' c
	runHooks kq' hooks
	where
		-- Kqueue returns changes for both whole directories
		-- being added and deleted, and individual files being
		-- added and deleted.
		dispatch q change
			| isAdd change = withstatus change $ dispatchadd q
			| otherwise = callhook delDirHook Nothing change
		dispatchadd q change s
			| Files.isSymbolicLink s = callhook addSymlinkHook (Just s) change
			| Files.isDirectory s = print $ "TODO: recursive directory add: " ++ show change
			| Files.isRegularFile s = callhook addHook (Just s) change
			| otherwise = print "not a file??"
		callhook h s change = case h hooks of
			Nothing -> print "missing hook??"
			Just a -> a (changedFile change) s
		withstatus change a = maybe noop (a change) =<<
			(catchMaybeIO (getSymbolicLinkStatus (changedFile change)))
