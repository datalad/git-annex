{- git-annex main program stub
 -
 - Copyright 2010,2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

import System.Environment
import System.FilePath

import qualified GitAnnex
import qualified GitAnnexShell

main :: IO ()
main = run =<< getProgName
	where
		run n
			| isshell n = go GitAnnexShell.run
			| otherwise = go GitAnnex.run
		isshell n = takeFileName n == "git-annex-shell"
		go a = a =<< getArgs
