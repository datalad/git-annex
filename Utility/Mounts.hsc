{- Interface to mtab (and fstab)
 - 
 - Derived from hsshellscript, originally written by
 - Volker Wysk <hsss@volker-wysk.de>
 - 
 - Modified to support BSD and Mac OS X by
 - Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU LGPL version 2.1 or higher.
 -}

{-# LANGUAGE ForeignFunctionInterface #-}

module Utility.Mounts (
	Mntent(..),
	getMounts
) where

import Control.Monad
import Foreign
import Foreign.C
import GHC.IO hiding (finally, bracket)
import Prelude hiding (catch)

#include "libmounts.h"

{- This is a stripped down mntent, containing only
 - fields available everywhere. -}
data Mntent = Mntent
	{ mnt_fsname :: String
	, mnt_dir :: String
	, mnt_type :: String
	} deriving (Read, Show, Eq, Ord)

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

foreign import ccall unsafe "libmounts.h mounts_start" c_mounts_start
        :: IO (Ptr ())

foreign import ccall unsafe "libmounts.h mounts_next" c_mounts_next
        :: Ptr () -> IO (Ptr ())

foreign import ccall unsafe "libmounts.h mounts_end" c_mounts_end
        :: Ptr () -> IO CInt
