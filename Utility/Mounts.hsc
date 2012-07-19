{- Interface to mtab (and fstab)
 - 
 - Derived from hsshellscript, originally written by
 - Volker Wysk <hsss@volker-wysk.de>
 -
 - Licensed under the GNU LGPL version 2.1 or higher.
 -}

{-# LANGUAGE ForeignFunctionInterface #-}

module Utility.Mounts (
	Mntent(..),
	read_mtab,
	read_fstab,
) where

import Control.Monad
import Foreign
import Foreign.C
import GHC.IO hiding (finally, bracket)
import Prelude hiding (catch)

#include <stdio.h>
#include <mntent.h>

data Mntent = Mntent
	{ mnt_fsname :: String
	, mnt_dir :: String
	, mnt_type :: String
	, mnt_opts :: String
	, mnt_freq :: Int
	, mnt_passno :: Int
	} deriving (Read, Show, Eq)

read_mounts :: String -> IO [Mntent]
read_mounts path = do
	h <- withCString path $ \cpath ->
		withCString "r" $ \r ->
			c_setmntent cpath r
	when (h == nullPtr) $
		throwErrno "setmntent"
	mntent <- getmntent h []
	_ <- c_endmntent h
	return mntent

	where
		getmntent h l = do
			ptr <- c_getmntent h
			if (ptr == nullPtr)
				then return $ reverse l
				else do
					mnt_fsname_str <- #{peek struct mntent, mnt_fsname} ptr >>= peekCString
					mnt_dir_str <- #{peek struct mntent, mnt_dir} ptr >>= peekCString
					mnt_type_str <- #{peek struct mntent, mnt_type} ptr >>= peekCString
					mnt_opts_str <- #{peek struct mntent, mnt_opts} ptr >>= peekCString
					mnt_freq_int <- #{peek struct mntent, mnt_freq} ptr
					mnt_passno_int <- #{peek struct mntent, mnt_passno} ptr
					let ent = Mntent
						{ mnt_fsname = mnt_fsname_str
						, mnt_dir = mnt_dir_str
						, mnt_type = mnt_type_str
						, mnt_opts = mnt_opts_str
						, mnt_freq = mnt_freq_int
						, mnt_passno = mnt_passno_int
						}
					getmntent h (ent:l)

read_mtab :: IO [Mntent]
read_mtab = read_mounts "/etc/mtab"

read_fstab :: IO [Mntent]
read_fstab = read_mounts "/etc/fstab"

foreign import ccall safe "setmntent"
	c_setmntent :: ((Ptr CChar) -> ((Ptr CChar) -> (IO (Ptr ()))))

foreign import ccall safe "endmntent"
	c_endmntent :: ((Ptr ()) -> (IO CInt))

foreign import ccall safe "getmntent"
	c_getmntent :: ((Ptr ()) -> (IO (Ptr ())))
