{- git-annex assistant named threads.
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.NamedThread where

import Assistant.Common
import Assistant.DaemonStatus
import Assistant.Alert

import qualified Control.Exception as E

runNamedThread :: NamedThread -> Assistant ()
runNamedThread (NamedThread name a) = do
	d <- getAssistant id
	liftIO . go $ d { threadName = name }
  where
	go d = do
		r <- E.try (runAssistant d a) :: IO (Either E.SomeException ())
		case r of
			Right _ -> noop
			Left e -> do
				let msg = unwords [name, "crashed:", show e]
				hPutStrLn stderr msg
				-- TODO click to restart
				runAssistant d $ void $
					addAlert $ warningAlert name msg
