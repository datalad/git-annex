{- /bin/sh handling
 -
 - Copyright 2013 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE CPP #-}

module Utility.Shell where

import Utility.SafeCommand
#ifdef mingw32_HOST_OS
import Utility.Path
import Utility.FileSystemEncoding
import Utility.Exception
import Utility.PartialPrelude
import Utility.Applicative
#endif

#ifdef mingw32_HOST_OS
import System.IO
import System.FilePath
#endif

shellPath_portable :: FilePath
shellPath_portable = "/bin/sh"

shellPath_local :: FilePath
#ifndef __ANDROID__
shellPath_local = shellPath_portable
#else
shellPath_local = "/system/bin/sh"
#endif

shebang_portable :: String
shebang_portable = "#!" ++ shellPath_portable

shebang_local :: String
shebang_local = "#!" ++ shellPath_local

-- | On Windows, shebang is not handled by the kernel, so to support
-- shell scripts etc, have to look at the program being run and
-- parse it for shebang.
--
-- This has no effect on Unix.
findShellCommand :: FilePath -> IO (FilePath, [CommandParam])
findShellCommand f = do
#ifndef mingw32_HOST_OS
	defcmd
#else
	l <- catchDefaultIO Nothing $ withFile f ReadMode $
		headMaybe . lines <$$> hGetContents
	case l of
		Just ('#':'!':rest) -> case words rest of
			[] -> defcmd
			(c:ps) -> do
				let ps' = map Param ps ++ [File f]
				-- If the command is not inPath,
				-- take the base of it, and run eg "sh"
				-- which in some cases on windows will work
				-- despite it not being inPath.
				ok <- inPath c
				return (if ok then c else takeFileName c, ps')
		_ -> defcmd
#endif
  where
	defcmd = return (f, [])
