{- git-annex assistant named threads.
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.Types.NamedThread where

import Common.Annex
import Assistant.Monad

import System.Log.Logger

type ThreadName = String
data NamedThread = NamedThread ThreadName (Assistant ())

debug :: [String] -> Assistant ()
debug ws = do
	name <- getAssistant threadName
	liftIO $ debugM name $ unwords $ (name ++ ":") : ws
