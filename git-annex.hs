{- git-annex main program stub
 -
 - Copyright 2010-2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

import System.Environment
import System.FilePath

import qualified GitAnnex
import qualified GitAnnexShell
#ifdef WITH_TESTSUITE
import qualified Test
#endif

main :: IO ()
main = run =<< getProgName
  where
	run n
		| isshell n = go GitAnnexShell.run
		| otherwise = go GitAnnex.run
	isshell n = takeFileName n == "git-annex-shell"
	go a = do
		ps <- getArgs
#ifdef WITH_TESTSUITE
		case ps of
			("test":ps') -> Test.main ps'
			_ -> a ps
#else
		a ps
#endif
