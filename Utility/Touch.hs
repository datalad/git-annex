{- Portability shim for touching a file.
 -
 - Copyright 2011-2018 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE CPP #-}

module Utility.Touch (
	touchBoth,
	touch
) where

#if ! defined(mingw32_HOST_OS)

import System.FilePath.ByteString (RawFilePath)
import System.Posix.Files.ByteString
import Data.Time.Clock.POSIX

{- Changes the access and modification times of an existing file.
   Can follow symlinks, or not. -}
touchBoth :: RawFilePath -> POSIXTime -> POSIXTime -> Bool -> IO ()
touchBoth file atime mtime follow
	| follow = setFileTimesHiRes file atime mtime
	| otherwise = setSymbolicLinkTimesHiRes file atime mtime

{- Changes the access and modification times of an existing file
 - to the same value. Can follow symlinks, or not. -}
touch :: RawFilePath -> POSIXTime -> Bool -> IO ()
touch file mtime = touchBoth file mtime mtime

#else

import Data.Time.Clock.POSIX
import Utility.RawFilePath

{- Noop for Windows -}
touchBoth :: RawFilePath -> POSIXTime -> POSIXTime -> Bool -> IO ()
touchBoth _ _ _ _ = return ()

touch :: RawFilePath -> POSIXTime -> Bool -> IO ()
touch _ _ _ = return ()

#endif
