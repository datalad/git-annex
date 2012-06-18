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
	waitChange,
) where

import Common

import System.Posix.Types
import Foreign.C.Types
import Foreign.C.Error
import Foreign.Ptr
import Foreign.Marshal
import qualified Data.Map as M

type DirMap = M.Map Fd FilePath

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
removeSubDir :: FilePath -> DirMap -> DirMap
removeSubDir dir = M.filter (not . dirContains dir)

foreign import ccall unsafe "libkqueue.h waitchange" c_waitchange
	:: Ptr Fd -> IO Fd

{- Waits for a change in a map of directories, and returns the directory
 - where the change took place. 
 -
 - The kqueue interface does not tell what type of change took place in
 - the directory; it could be an added file, a deleted file, a renamed
 - file, a new subdirectory, or a deleted subdirectory, or a moved
 - subdirectory.
 -
 - Note that if subdirectories have changed, the caller will want to
 - update the map before calling this again. -}
waitChange :: DirMap -> IO (Maybe FilePath)
waitChange dirmap = withArray (M.keys dirmap) $ \c_fds -> do
	changed <- c_waitchange c_fds
	ifM (safeErrno <$> getErrno)
		( return $ M.lookup changed dirmap
		, return Nothing
		)
	where
		safeErrno (Errno v) = v == 0
