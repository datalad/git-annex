{- Common infrastructure for the git-annex assistant threads.
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.Common (
	module X,
	ThreadName,
	debug
) where

import Common.Annex as X

import System.Log.Logger

type ThreadName = String

debug :: ThreadName -> [String] -> IO ()
debug threadname ws = debugM threadname $ unwords $ (threadname ++ ":") : ws
