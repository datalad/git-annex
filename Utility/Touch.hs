{- More control over touching a file.
 -
 - Copyright 2011 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE CPP #-}

module Utility.Touch (
	TimeSpec(..),
	touchBoth,
	touch
) where

#if ! defined(mingw32_HOST_OS) && ! defined(__ANDROID__)

#if MIN_VERSION_unix(2,7,0)

import System.Posix.Files
import System.Posix.Types

newtype TimeSpec = TimeSpec EpochTime

{- Changes the access and modification times of an existing file.
   Can follow symlinks, or not. Throws IO error on failure. -}
touchBoth :: FilePath -> TimeSpec -> TimeSpec -> Bool -> IO ()
touchBoth file (TimeSpec atime) (TimeSpec mtime) follow
	| follow = setFileTimes file atime mtime
	| otherwise = setSymbolicLinkTimesHiRes file (realToFrac atime) (realToFrac mtime)

touch :: FilePath -> TimeSpec -> Bool -> IO ()
touch file mtime = touchBoth file mtime mtime

#else
import Utility.Touch.Old
#endif

#else

import System.PosixCompat

newtype TimeSpec = TimeSpec EpochTime

{- Noop for Windows -}
touchBoth FilePath -> TimeSpec -> TimeSpec -> Bool -> IO ()
touchBoth _ _ _ _ = return ()

touch :: FilePath -> TimeSpec -> Bool -> IO ()
touch _ _ = return ()

#endif
