{- /bin/sh handling
 -
 - Copyright 2013 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE CPP #-}

module Utility.Shell (
	shellPath,
	shebang,
	findShellCommand,
) where

import Utility.SafeCommand
#ifdef mingw32_HOST_OS
import Utility.Path
import Utility.Exception
import Utility.PartialPrelude
#endif

#ifdef mingw32_HOST_OS
import System.FilePath
#endif

shellPath :: FilePath
shellPath = "/bin/sh"

shebang :: String
shebang = "#!" ++ shellPath

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
	l <- catchDefaultIO Nothing $ headMaybe . lines <$> readFile f
	case l of
		Just ('#':'!':rest) -> case words rest of
			[] -> defcmd
			(c:ps) -> do
				let ps' = map Param ps ++ [File f]
				-- If the command is not inSearchPath,
				-- take the base of it, and run eg "sh"
				-- which in some cases on windows will work
				-- despite it not being inSearchPath.
				ok <- inSearchPath c
				return (if ok then c else takeFileName c, ps')
		_ -> defcmd
#endif
  where
	defcmd = return (f, [])
