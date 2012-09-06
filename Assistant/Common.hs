{- Common infrastructure for the git-annex assistant threads.
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.Common (
	module X,
	ThreadName,
	NamedThread(..),
	runNamedThread,
	debug
) where

import Common.Annex as X
import Assistant.DaemonStatus
import Assistant.Alert

import System.Log.Logger
import qualified Control.Exception as E

type ThreadName = String
data NamedThread = NamedThread ThreadName (IO ())

debug :: ThreadName -> [String] -> IO ()
debug threadname ws = debugM threadname $ unwords $ (threadname ++ ":") : ws

runNamedThread :: DaemonStatusHandle -> NamedThread -> IO ()
runNamedThread dstatus (NamedThread name a) = go
	where
		go = do
			r <- E.try a :: IO (Either E.SomeException ())
			case r of
				Right _ -> noop
				Left e -> do
					let msg = unwords
						[ name
						, "crashed:"
						, show e
						]
					hPutStrLn stderr msg
					-- TODO click to restart
					void $ addAlert dstatus $
						warningAlert name msg
