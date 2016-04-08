{- POSIX files (and compatablity wrappers).
 -
 - This is like System.PosixCompat.Files, but with a few fixes.
 -
 - Copyright 2014 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-tabs #-}

module Utility.PosixFiles (
	module X,
	rename
) where

import System.PosixCompat.Files as X hiding (rename)

#ifndef mingw32_HOST_OS
import System.Posix.Files (rename)
#else
import qualified System.Win32.File as Win32
import qualified System.Win32.HardLink as Win32
#endif

{- System.PosixCompat.Files.rename on Windows calls renameFile,
 - so cannot rename directories. 
 -
 - Instead, use Win32 moveFile, which can. It needs to be told to overwrite
 - any existing file. -}
#ifdef mingw32_HOST_OS
rename :: FilePath -> FilePath -> IO ()
rename src dest = Win32.moveFileEx src dest Win32.mOVEFILE_REPLACE_EXISTING
#endif

{- System.PosixCompat.Files.createLink throws an error, but windows
 - does support hard links. -}
#ifdef mingw32_HOST_OS
createLink :: FilePath -> FilePath -> IO ()
createLink = Win32.createHardLink
#endif
