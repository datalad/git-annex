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
	debug,
	brokendebug
) where

import Common.Annex as X
import Assistant.Monad as X
import Assistant.Alert
import Assistant.DaemonStatus

import System.Log.Logger
import qualified Control.Exception as E

type ThreadName = String
data NamedThread = NamedThread ThreadName (Assistant ())

brokendebug :: ThreadName -> [String] -> IO ()
brokendebug _ _ = noop -- TODO remove this

debug :: [String] -> Assistant ()
debug ws = do
	name <- getAssistant threadName
	liftIO $ debugM name $ unwords $ (name ++ ":") : ws

runNamedThread :: NamedThread -> Assistant ()
runNamedThread (NamedThread name a) = do
	d <- getAssistant id
	liftIO . go $ d { threadName = name }
  where
	go d = do
		r <- E.try (runAssistant a d) :: IO (Either E.SomeException ())
		case r of
			Right _ -> noop
			Left e -> do
				let msg = unwords [name, "crashed:", show e]
				hPutStrLn stderr msg
				-- TODO click to restart
				void $ addAlert (daemonStatusHandle d) $
					warningAlert name msg
