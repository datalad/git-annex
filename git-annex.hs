{- git-annex main program stub
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

import System.Environment

import GitAnnex

main :: IO ()
main = run =<< getArgs
