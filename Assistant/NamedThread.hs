{- git-annex assistant named threads.
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.NamedThread where

import Common.Annex
import Assistant.Types.DaemonStatus
import Assistant.DaemonStatus
import Assistant.Alert
import Assistant.Monad

import Control.Concurrent
import Control.Concurrent.Async
import qualified Data.Map as M

{- Starts a named thread, if it's not already running.
 -
 - Named threads are run by a management thread, so if they crash
 - an alert is displayed, allowing the thread to be restarted. -}
startNamedThread :: NamedThread -> Assistant ()
startNamedThread namedthread@(NamedThread name a) = do
	m <- startedThreads <$> getDaemonStatus
	case M.lookup name m of
		Nothing -> start
		Just aid ->
			maybe noop (const start) =<< liftIO (poll aid)
  where
	start = do
		d <- getAssistant id
		aid <- liftIO $ runmanaged $ d { threadName = name }
		modifyDaemonStatus_ $ \s -> s
			{ startedThreads = M.insertWith' const name aid (startedThreads s) }
	runmanaged d = do
		aid <- async $ runAssistant d a
		void $ forkIO $ manager d aid
		return aid
	manager d aid = do
		r <- waitCatch aid
		case r of
			Right _ -> noop
			Left e -> do
				let msg = unwords [name, "crashed:", show e]
				hPutStrLn stderr msg
				-- TODO click to restart
				runAssistant d $ void $
					addAlert $ warningAlert name msg

{- Waits for all named threads that have been started to finish. -}
waitNamedThreads :: Assistant ()
waitNamedThreads = do
	m <- startedThreads <$> getDaemonStatus
	liftIO $ mapM_ wait $ M.elems m

