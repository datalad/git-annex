{- git-annex main program dispatch
 -
 - Copyright 2010-2014 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

import System.Environment (getArgs, getProgName)
import System.FilePath

import qualified CmdLine.GitAnnex
import qualified CmdLine.GitAnnexShell
#ifdef WITH_TESTSUITE
import qualified Test
#endif

#ifdef mingw32_HOST_OS
import Utility.UserInfo
import Utility.Env
import Config.Files
import System.Process
import System.Exit
#endif

main :: IO ()
main = do
	ps <- getArgs
	run ps =<< getProgName
  where
	run ps n
		| isshell n = CmdLine.GitAnnexShell.run ps
		| otherwise =
#ifdef mingw32_HOST_OS
			winEnv gitannex ps
#else
			gitannex ps
#endif
	gitannex ps = 
#ifdef WITH_TESTSUITE
		case ps of
			("test":ps') -> Test.main ps'
			_ -> CmdLine.GitAnnex.run ps
#else
		CmdLine.GitAnnex.run ps
#endif
	isshell n = takeFileName n == "git-annex-shell"

#ifdef mingw32_HOST_OS
{- On Windows, if HOME is not set, probe it and set it.
 - This is a workaround for some Cygwin commands needing HOME to be set,
 - and for there being no known way to set environment variables on
 - Windows, except by passing an environment in each call to a program.
 - While ugly, this workaround is easier than trying to ensure HOME is set
 - in all calls to the affected programs.
 -
 - If TZ is set, unset it.
 - TZ being set can interfere with workarounds for Windows timezone
 - horribleness, and prevents getCurrentTimeZone from seeing the system
 - time zone.
 -
 - Due to Windows limitations, have to re-exec git-annex with the new
 - environment.
 -}
winEnv :: ([String] -> IO ()) -> [String] -> IO ()
winEnv a ps = do
	e <- getEnvironment
	home <- myHomeDir
	let e' = wantedenv e home
	if (e' /= e)
		then do
			cmd <- readProgramFile
			(_, _, _, pid) <- createProcess (proc cmd ps)
				{ env = Just e' }
			exitWith =<< waitForProcess pid		
		else a ps
  where
	wantedenv e home = delEntry "TZ" $ case lookup "HOME" e of
		Nothing -> e 
		Just _ -> addEntries
			[ ("HOME", home)
			, ("CYGWIN", "nodosfilewarning")
			] e
#endif
