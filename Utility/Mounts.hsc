{- Interface to mtab (and fstab)
 - 
 - Deprecated; moving to mountpoints library on hackage.
 - 
 - Derived from hsshellscript, originally written by
 - Volker Wysk <hsss@volker-wysk.de>
 - 
 - Modified to support BSD, Mac OS X, and Android by
 - Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU LGPL version 2.1 or higher.
 -
 -}

{-# LANGUAGE ForeignFunctionInterface #-}

module Utility.Mounts (
	Mntent(..),
	getMounts
) where

#ifndef __ANDROID__
import Control.Monad
import Foreign
import Foreign.C
#include "libmounts.h"
#else
import Utility.Exception
import Data.Maybe
import Control.Applicative
#endif
import Prelude

{- This is a stripped down mntent, containing only
 - fields available everywhere. -}
data Mntent = Mntent
	{ mnt_fsname :: String
	, mnt_dir :: FilePath
	, mnt_type :: String
	} deriving (Show, Eq, Ord)

#ifndef __ANDROID__

getMounts :: IO [Mntent]
getMounts = do
	h <- c_mounts_start
	when (h == nullPtr) $
		throwErrno "getMounts"
	mntent <- getmntent h []
	_ <- c_mounts_end h
	return mntent

  where
	getmntent h c = do
		ptr <- c_mounts_next h
		if (ptr == nullPtr)
			then return $ reverse c
			else do
				mnt_fsname_str <- #{peek struct mntent, mnt_fsname} ptr >>= peekCString
				mnt_dir_str <- #{peek struct mntent, mnt_dir} ptr >>= peekCString
				mnt_type_str <- #{peek struct mntent, mnt_type} ptr >>= peekCString
				let ent = Mntent
					{ mnt_fsname = mnt_fsname_str
					, mnt_dir = mnt_dir_str
					, mnt_type = mnt_type_str
					}
				getmntent h (ent:c)

{- Using unsafe imports because the C functions are belived to never block.
 - Note that getmntinfo is called with MNT_NOWAIT to avoid possibly blocking;
 - while getmntent only accesses a file in /etc (or /proc) that should not
 - block. -}
foreign import ccall unsafe "libmounts.h mounts_start" c_mounts_start
        :: IO (Ptr ())
foreign import ccall unsafe "libmounts.h mounts_next" c_mounts_next
        :: Ptr () -> IO (Ptr ())
foreign import ccall unsafe "libmounts.h mounts_end" c_mounts_end
        :: Ptr () -> IO CInt

#else

{- Android does not support getmntent (well, it's a no-op stub in Bionic).
 - 
 - But, the linux kernel's /proc/mounts is available to be parsed.
 -}
getMounts :: IO [Mntent]
getMounts = catchDefaultIO [] $
	mapMaybe (parse . words) . lines <$> readFile "/proc/mounts"
  where
  	parse (device:mountpoint:fstype:_rest) = Just $ Mntent
		{ mnt_fsname = device
		, mnt_dir = mountpoint
		, mnt_type = fstype
		}
	parse _ = Nothing

#endif
