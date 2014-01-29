{- POSIX files (and compatablity wrappers).
 -
 - This is like System.PosixCompat.Files, except with a fixed rename.
 -
 - Copyright 2014 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Utility.PosixFiles (
	module X,
	rename
) where

import System.PosixCompat.Files as X hiding (rename)

#ifndef mingw32_HOST_OS
import System.Posix.Files (rename)
#else
import System.Win32.File (moveFile)
#endif

{- System.PosixCompat.Files.rename on Windows calls renameFile,
 - so cannot rename directories. Instead, use Win32 moveFile, which can. -}
#ifdef mingw32_HOST_OS
rename :: FilePath -> FilePath -> IO ()
rename = moveFile
#endif
