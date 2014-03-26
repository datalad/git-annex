{- disk free space checking 
 -
 - Copyright 2012, 2014 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE ForeignFunctionInterface, CPP #-}

module Utility.DiskFree (
	getDiskFree,
	getDiskSize
) where

#ifdef WITH_CLIBS

import Common

import Foreign.C.Types
import Foreign.C.String
import Foreign.C.Error

foreign import ccall safe "libdiskfree.h diskfree" c_diskfree
	:: CString -> IO CULLong

foreign import ccall safe "libdiskfree.h disksize" c_disksize
	:: CString -> IO CULLong

getVal :: (CString -> IO CULLong) -> FilePath -> IO (Maybe Integer)
getVal getter path = withFilePath path $ \c_path -> do
	free <- getter c_path
	ifM (safeErrno <$> getErrno)
		( return $ Just $ toInteger free
		, return Nothing
		)
  where
	safeErrno (Errno v) = v == 0

getDiskFree :: FilePath -> IO (Maybe Integer)
getDiskFree = getVal c_diskfree

getDiskSize :: FilePath -> IO (Maybe Integer)
getDiskSize = getVal c_disksize

#else
#ifdef mingw32_HOST_OS

import Common

import System.Win32.File

getDiskFree :: FilePath -> IO (Maybe Integer)
getDiskFree path = catchMaybeIO $ do
	(sectors, bytes, nfree, _ntotal) <- getDiskFreeSpace (Just path)
	return $ toInteger sectors * toInteger bytes * toInteger nfree

getDiskSize :: FilePath -> IO (Maybe Integer)
getDiskSize _ = return Nothing
#else

#warning Building without disk free space checking support

getDiskFree :: FilePath -> IO (Maybe Integer)
getDiskFree _ = return Nothing

getDiskSize :: FilePath -> IO (Maybe Integer)
getDiskSize _ = return Nothing

#endif
#endif
