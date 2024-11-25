{- git-annex main program dispatch
 -
 - Copyright 2010-2024 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

import System.Environment (getArgs, getProgName)
import System.FilePath
import Network.Socket (withSocketsDo)
import qualified Data.Map as M

import CmdLine.Multicall
import qualified CmdLine.GitAnnex
import qualified CmdLine.GitAnnexShell
import qualified CmdLine.GitRemoteAnnex
import qualified CmdLine.GitRemoteTorAnnex
import qualified Test
import qualified Benchmark
import Messages
import Utility.FileSystemEncoding

#ifdef mingw32_HOST_OS
import Utility.UserInfo
import Utility.Env.Set
#endif

main :: IO ()
main = sanitizeTopLevelExceptionMessages $ withSocketsDo $ do
	useFileSystemEncoding
	ps <- getArgs
#ifdef mingw32_HOST_OS
	winEnv
#endif
	run ps =<< getProgName
  where
	run ps n = case M.lookup (takeFileName n) otherMulticallCommands of
		Just GitAnnexShell -> CmdLine.GitAnnexShell.run ps
		Just GitRemoteAnnex -> CmdLine.GitRemoteAnnex.run ps
		Just GitRemoteTorAnnex -> CmdLine.GitRemoteTorAnnex.run ps
		Nothing -> CmdLine.GitAnnex.run
			Test.optParser
			Test.runner
			Benchmark.mkGenerator
			ps

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
