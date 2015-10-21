{- More control over touching a file.
 -
 - Copyright 2011 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE ForeignFunctionInterface, CPP #-}

module Utility.Touch (
	TimeSpec(..),
	touchBoth,
	touch
) where

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
{- Compatability interface for old version of unix, to be removed eventally. -}

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/time.h>

#ifndef _BSD_SOURCE
#define _BSD_SOURCE
#endif

#if (defined UTIME_OMIT && defined UTIME_NOW && defined AT_FDCWD && defined AT_SYMLINK_NOFOLLOW)
#define use_utimensat 1

import Utility.FileSystemEncoding

import Control.Monad (when)
import Foreign
#endif

import Foreign.C

newtype TimeSpec = TimeSpec CTime

touchBoth :: FilePath -> TimeSpec -> TimeSpec -> Bool -> IO ()

touch :: FilePath -> TimeSpec -> Bool -> IO ()
touch file mtime = touchBoth file mtime mtime

#ifdef use_utimensat

at_fdcwd :: CInt
at_fdcwd = #const AT_FDCWD

at_symlink_nofollow :: CInt
at_symlink_nofollow = #const AT_SYMLINK_NOFOLLOW

instance Storable TimeSpec where
	-- use the larger alignment of the two types in the struct
	alignment _ = max sec_alignment nsec_alignment
	  where
		sec_alignment = alignment (1::CTime)
		nsec_alignment = alignment (1::CLong)
	sizeOf _ = #{size struct timespec}
	peek ptr = do
		sec <- #{peek struct timespec, tv_sec} ptr
		return $ TimeSpec sec
	poke ptr (TimeSpec sec) = do
		#{poke struct timespec, tv_sec} ptr sec
		#{poke struct timespec, tv_nsec} ptr (0 :: CLong)

{- While its interface is beastly, utimensat is in recent
   POSIX standards, unlike lutimes. -}
foreign import ccall "utimensat" 
	c_utimensat :: CInt -> CString -> Ptr TimeSpec -> CInt -> IO CInt

touchBoth file atime mtime follow = 
	allocaArray 2 $ \ptr ->
	withFilePath file $ \f -> do
		pokeArray ptr [atime, mtime]
		r <- c_utimensat at_fdcwd f ptr flags
		when (r /= 0) $ throwErrno "touchBoth"
  where
	flags
       		| follow = 0
		| otherwise = at_symlink_nofollow 

#else
#if 0
{- Using lutimes is needed for BSD.
 - 
 - TODO: test if lutimes is available. May have to do it in configure.
 - TODO: TimeSpec uses a CTime, while tv_sec is a CLong. It is implementation
 - dependent whether these are the same; need to find a cast that works.
 - (Without the cast it works on linux i386, but
 - maybe not elsewhere.)
 -}

instance Storable TimeSpec where
	alignment _ = alignment (1::CLong)
	sizeOf _ = #{size struct timeval}
	peek ptr = do
		sec <- #{peek struct timeval, tv_sec} ptr
		return $ TimeSpec sec
	poke ptr (TimeSpec sec) = do
		#{poke struct timeval, tv_sec} ptr sec
		#{poke struct timeval, tv_usec} ptr (0 :: CLong) 

foreign import ccall "utimes" 
	c_utimes :: CString -> Ptr TimeSpec -> IO CInt
foreign import ccall "lutimes" 
	c_lutimes :: CString -> Ptr TimeSpec -> IO CInt

touchBoth file atime mtime follow = 
	allocaArray 2 $ \ptr ->
	withFilePath file $ \f -> do
		pokeArray ptr [atime, mtime]
		r <- syscall f ptr
		when (r /= 0) $
			throwErrno "touchBoth"
  where
	syscall
       		| follow = c_lutimes
		| otherwise = c_utimes

#else
#warning "utimensat and lutimes not available; building without symlink timestamp preservation support"
touchBoth _ _ _ _ = return ()
#endif
#endif

#endif
