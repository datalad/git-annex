{- git-annex-shell main program
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

import System.Environment

import GitAnnexShell

main :: IO ()
main = run =<< getArgs
