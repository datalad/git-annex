{- git-annex main program stub
 -
 - Copyright 2010-2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

import System.Environment
import System.FilePath

import qualified CmdLine.GitAnnex
import qualified CmdLine.GitAnnexShell
#ifdef WITH_TESTSUITE
import qualified Test
#endif

main :: IO ()
main = do
	ps <- getArgs
	run ps =<< getProgName
  where
	run ps n
		| isshell n = CmdLine.GitAnnexShell.run ps
		| otherwise =
#ifdef WITH_TESTSUITE
			case ps of
				("test":ps') -> Test.main ps'
				_ -> CmdLine.GitAnnex.run ps
#else
			CmdLine.GitAnnex.run ps
#endif
	isshell n = takeFileName n == "git-annex-shell"
