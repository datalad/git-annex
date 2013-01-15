{- git-annex assistant named threads.
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.Types.NamedThread (
	ThreadName,
	NamedThread(..),
	debug,
	notice,
) where

import Common.Annex
import Assistant.Monad

import System.Log.Logger

type ThreadName = String
data NamedThread = NamedThread ThreadName (Assistant ())

debug :: [String] -> Assistant ()
debug = logaction debugM

notice :: [String] -> Assistant ()
notice = logaction noticeM

logaction :: (String -> String -> IO ()) -> [String] -> Assistant ()
logaction a ws = do
	name <- getAssistant threadName
	liftIO $ a name $ unwords $ (name ++ ":") : ws
