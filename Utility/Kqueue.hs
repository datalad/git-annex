{- BSD kqueue file modification notification interface
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE ForeignFunctionInterface #-}

module Utility.Kqueue (
	scanRecursive,
	addSubDir,
	removeSubDir,

	initKqueue,
	stopKqueue,

	waitChange,
) where

import Common

import System.Posix.Types
import Foreign.C.Types
import Foreign.Ptr
import Foreign.Marshal
import qualified Data.Map as M

type DirMap = M.Map Fd FilePath

data Kqueue = Kqueue Fd DirMap

{- Builds a map of directories in a tree, possibly pruning some.
 - Opens each directory in the tree. -}
scanRecursive :: FilePath -> (FilePath -> Bool) -> IO DirMap
scanRecursive dir prune = M.fromList <$> (mapM opendir =<< dirTree dir prune)
	where
		opendir d = (,)
			<$> openFd d ReadOnly Nothing defaultFileFlags
			<*> pure d

{- Adds a subdirectory (and all its subdirectories, unless pruned) to a
 - directory map. -}
addSubDir :: DirMap -> FilePath -> (FilePath -> Bool) -> IO DirMap
addSubDir dirmap dir prune = M.union dirmap <$> scanRecursive dir prune

{- Removes a subdirectory (and all its subdirectories) from a directory map. -}
removeSubDir :: FilePath -> DirMap -> IO DirMap
removeSubDir dir dirmap = do
	mapM_ closeFd $ M.keys toremove) $ closeFd
	return rest
	where
		(toremove, rest) = M.partition (dirContains dir) dirmap

foreign import ccall unsafe "libkqueue.h init_kqueue" c_init_kqueue
	:: CInt -> Ptr Fd -> IO Fd
foreign import ccall unsafe "libkqueue.h waitchange_kqueue" c_waitchange_kqueue
	:: Fd -> IO Fd

{- Initializes a Kqueue to watch a map of directories. -}
initKqueue :: DirMap -> IO Kqueue
initKqueue dirmap = withArrayLen (M.keys dirmap) $ \fdcnt c_fds -> do
	h <- c_init_kqueue (fromIntegral fdcnt) c_fds
	return $ Kqueue h dirmap

{- Stops a Kqueue. Note: Does not directly close the Fds in the dirmap,
 - so it can be reused.  -}
stopKqueue :: Kqueue -> IO ()
stopKqueue (Kqueue h _) = closeFd h

{- Waits for a change on a Kqueue, and returns the directory
 - where a change took place.
 -
 - The kqueue interface does not tell what type of change took place in
 - the directory; it could be an added file, a deleted file, a renamed
 - file, a new subdirectory, or a deleted subdirectory, or a moved
 - subdirectory.
 -
 - Note that if subdirectories have changed, the caller should re-run
 - initKqueue to get them watched. -}
waitChange :: Kqueue -> IO (Maybe FilePath)
waitChange (Kqueue h dirmap) = do
	changed <- c_waitchange_kqueue h
	return $ M.lookup changed dirmap
