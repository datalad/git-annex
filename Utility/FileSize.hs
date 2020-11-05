{- File size.
 -
 - Copyright 2015-2020 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module Utility.FileSize (
	FileSize,
	getFileSize,
	getFileSize',
) where

import System.PosixCompat.Files
#ifdef mingw32_HOST_OS
import Control.Exception (bracket)
import System.IO
#endif

import qualified Utility.RawFilePath as R

type FileSize = Integer

{- Gets the size of a file.
 -
 - This is better than using fileSize, because on Windows that returns a
 - FileOffset which maxes out at 2 gb.
 - See https://github.com/jystic/unix-compat/issues/16
 -}
getFileSize :: R.RawFilePath -> IO FileSize
#ifndef mingw32_HOST_OS
getFileSize f = fmap (fromIntegral . fileSize) (R.getFileStatus f)
#else
getFileSize f = bracket (openFile (fromRawFilePath f) ReadMode) hClose hFileSize
#endif

{- Gets the size of the file, when its FileStatus is already known.
 -
 - On windows, uses getFileSize. Otherwise, the FileStatus contains the
 - size, so this does not do any work. -}
getFileSize' :: R.RawFilePath -> FileStatus -> IO FileSize
#ifndef mingw32_HOST_OS
getFileSize' _ s = return $ fromIntegral $ fileSize s
#else
getFileSize' f _ = getFileSize f
#endif
