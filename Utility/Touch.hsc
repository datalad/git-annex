{- More control over touching a file.
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE ForeignFunctionInterface #-}

module Utility.Touch (
	TimeSpec(..),
	touchBoth,
	touch
) where

import Utility.FileSystemEncoding

import Foreign
import Foreign.C
import Control.Monad (when)

newtype TimeSpec = TimeSpec CTime

{- Changes the access and modification times of an existing file.
   Can follow symlinks, or not. Throws IO error on failure. -}
touchBoth :: FilePath -> TimeSpec -> TimeSpec -> Bool -> IO ()

touch :: FilePath -> TimeSpec -> Bool -> IO ()
touch file mtime = touchBoth file mtime mtime

#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/time.h>

#ifndef _BSD_SOURCE
#define _BSD_SOURCE
#endif

#if (defined UTIME_OMIT && defined UTIME_NOW && defined AT_FDCWD && defined AT_SYMLINK_NOFOLLOW)

at_fdcwd :: CInt
at_fdcwd = #const AT_FDCWD

at_symlink_nofollow :: CInt
at_symlink_nofollow = #const AT_SYMLINK_NOFOLLOW

instance Storable TimeSpec where
	-- use the larger alignment of the two types in the struct
	alignment _ = max sec_alignment nsec_alignment
		where
			sec_alignment = alignment (undefined::CTime)
			nsec_alignment = alignment (undefined::CLong)
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
		flags = if follow
			then 0
			else at_symlink_nofollow 

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
	alignment _ = alignment (undefined::CLong)
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
		syscall = if follow
			then c_lutimes
			else c_utimes

#else
#warning "utimensat and lutimes not available; building without symlink timestamp preservation support"
touchBoth _ _ _ _ = return ()
#endif
#endif
