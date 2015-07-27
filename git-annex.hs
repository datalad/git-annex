{- git-annex main program dispatch
 -
 - Copyright 2010-2014 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

import System.Environment (getArgs, getProgName)
import System.FilePath
import Network.Socket (withSocketsDo)

import qualified CmdLine.GitAnnex
import qualified CmdLine.GitAnnexShell
import qualified Test

#ifdef mingw32_HOST_OS
import Utility.UserInfo
import Utility.Env
#endif

main :: IO ()
main = withSocketsDo $ do
	ps <- getArgs
	run ps =<< getProgName
  where
	run ps n
		| isshell n = CmdLine.GitAnnexShell.run ps
		| otherwise =
#ifdef mingw32_HOST_OS
			do
				winEnv
				gitannex ps
#else
			gitannex ps
#endif
	gitannex = CmdLine.GitAnnex.run Test.optParser Test.runner
	isshell n = takeFileName n == "git-annex-shell"

#ifdef mingw32_HOST_OS
{- On Windows, if HOME is not set, probe it and set it.
 - This is a workaround for some Cygwin commands needing HOME to be set.
 -
 - If TZ is set, unset it.
 - TZ being set can interfere with workarounds for Windows timezone
 - horribleness, and prevents getCurrentTimeZone from seeing the system
 - time zone.
 -}
winEnv :: IO ()
winEnv = do
	home <- myHomeDir
	setEnv "HOME" home False
	setEnv "CYGWIN" "nodosfilewarning" True
	unsetEnv "TZ"
#endif
