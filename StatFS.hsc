-----------------------------------------------------------------------------
-- |
--
-- (This code originally comes from xmobar)
-- 
-- Module      :  StatFS
-- Copyright   :  (c) Jose A Ortega Ruiz
-- License     :  BSD-3-clause
--
-- All rights reserved.
-- 
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions
-- are met:
-- 
-- 1. Redistributions of source code must retain the above copyright
--    notice, this list of conditions and the following disclaimer.
-- 2. Redistributions in binary form must reproduce the above copyright
--    notice, this list of conditions and the following disclaimer in the
--    documentation and/or other materials provided with the distribution.
-- 3. Neither the name of the author nor the names of his contributors
--    may be used to endorse or promote products derived from this software
--    without specific prior written permission.
-- 
-- THIS SOFTWARE IS PROVIDED BY THE REGENTS AND CONTRIBUTORS ``AS IS'' AND
-- ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
-- IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
-- ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHORS OR CONTRIBUTORS BE LIABLE
-- FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
-- OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
-- HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
-- LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
-- OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
-- SUCH DAMAGE.
--
-- Maintainer  :  Jose A Ortega Ruiz <jao@gnu.org>
-- Stability   :  unstable
-- Portability :  unportable
--
--  A binding to C's statvfs(2)
--
-----------------------------------------------------------------------------

{-# LANGUAGE CPP, ForeignFunctionInterface, EmptyDataDecls #-}


module StatFS ( FileSystemStats(..), getFileSystemStats ) where

import Foreign
import Foreign.C.Types
import Foreign.C.String
import Data.ByteString (useAsCString)
import Data.ByteString.Char8 (pack)

#if defined (__FreeBSD__) || defined (__FreeBSD_kernel__) || defined (__APPLE__)
# include <sys/param.h>
# include <sys/mount.h>
#else
#if defined (__linux__)
#include <sys/vfs.h>
#else
#define UNKNOWN
#endif
#endif

data FileSystemStats = FileSystemStats {
  fsStatBlockSize :: Integer
  -- ^ Optimal transfer block size.
  , fsStatBlockCount :: Integer
  -- ^ Total data blocks in file system.
  , fsStatByteCount :: Integer
  -- ^ Total bytes in file system.
  , fsStatBytesFree :: Integer
  -- ^ Free bytes in file system.
  , fsStatBytesAvailable :: Integer
  -- ^ Free bytes available to non-superusers.
  , fsStatBytesUsed :: Integer
  -- ^ Bytes used.
  } deriving (Show, Eq)

data CStatfs

#ifdef UNKNOWN
#warning free space checking code not available for this OS
#else
#if defined(__APPLE__)
foreign import ccall unsafe "sys/mount.h statfs64"
#else
#if defined(__FreeBSD__) || defined (__FreeBSD_kernel__)
foreign import ccall unsafe "sys/mount.h statfs"
#else
foreign import ccall unsafe "sys/vfs.h statfs64"
#endif
#endif
  c_statfs :: CString -> Ptr CStatfs -> IO CInt
#endif

toI :: CULong -> Integer
toI = toInteger

getFileSystemStats :: String -> IO (Maybe FileSystemStats)
getFileSystemStats path =
#ifdef UNKNOWN
  return Nothing
#else
  allocaBytes (#size struct statfs) $ \vfs ->
  useAsCString (pack path) $ \cpath -> do
    res <- c_statfs cpath vfs
    if res == -1 then return Nothing
      else do
        bsize <- (#peek struct statfs, f_bsize) vfs
        bcount <- (#peek struct statfs, f_blocks) vfs
        bfree <- (#peek struct statfs, f_bfree) vfs
        bavail <- (#peek struct statfs, f_bavail) vfs
        let bpb = toI bsize
        return $ Just FileSystemStats
                       { fsStatBlockSize = bpb
                       , fsStatBlockCount = toI bcount
                       , fsStatByteCount = toI bcount * bpb
                       , fsStatBytesFree = toI bfree * bpb
                       , fsStatBytesAvailable = toI bavail * bpb
                       , fsStatBytesUsed = toI (bcount - bfree) * bpb
                       }
#endif
