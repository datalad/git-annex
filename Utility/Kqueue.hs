{- BSD kqueue file modification notification interface
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE ForeignFunctionInterface #-}

module Utility.Kqueue (
	waitChange,
	scanRecursive
) where

import Common

import System.Posix.Types
import Foreign.C.Types
import Foreign.C.Error
import Foreign.Ptr
import Foreign.Marshal
import qualified Data.Map as M

type DirMap = M.Map Fd FilePath

foreign import ccall unsafe "libkqueue.h waitchange" c_waitchange
	:: Ptr Fd -> IO Fd

waitChange :: DirMap -> IO (Maybe FilePath)
waitChange dirmap = withArray (M.keys dirmap) $ \c_fds -> do
	changed <- c_waitchange c_fds
	ifM (safeErrno <$> getErrno)
		( return $ M.lookup changed dirmap
		, return Nothing
		)
	where
		safeErrno (Errno v) = v == 0

scanRecursive :: FilePath -> (FilePath -> Bool) -> IO DirMap
scanRecursive dir prune = M.fromList <$> (mapM opendir =<< dirTree dir prune)
	where
		opendir d = (,)
			<$> openFd d ReadOnly Nothing defaultFileFlags
			<*> pure d
