{- git-annex assistant named threads.
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.NamedThread where

import Common.Annex
import Assistant.Types.NamedThread
import Assistant.Types.ThreadName
import Assistant.Types.DaemonStatus
import Assistant.DaemonStatus
import Assistant.Alert
import Assistant.Monad
import Assistant.WebApp
import Assistant.WebApp.Types

import Control.Concurrent
import Control.Concurrent.Async
import qualified Data.Map as M
import qualified Data.Text as T
import qualified Control.Exception as E

{- Starts a named thread, if it's not already running.
 -
 - Named threads are run by a management thread, so if they crash
 - an alert is displayed, allowing the thread to be restarted. -}
startNamedThread :: Maybe UrlRenderer -> NamedThread -> Assistant ()
startNamedThread urlrenderer namedthread@(NamedThread name a) = do
	m <- startedThreads <$> getDaemonStatus
	case M.lookup name m of
		Nothing -> start
		Just (aid, _) -> do
			r <- liftIO (E.try (poll aid) :: IO (Either E.SomeException (Maybe (Either E.SomeException ()))))
			case r of
				Right Nothing -> noop
				_ -> start
  where
	start = do
		d <- getAssistant id
		aid <- liftIO $ runmanaged $ d { threadName = name }
		restart <- asIO $ startNamedThread urlrenderer namedthread
		modifyDaemonStatus_ $ \s -> s
			{ startedThreads = M.insertWith' const name (aid, restart) (startedThreads s) }
	runmanaged d = do
		aid <- async $ runAssistant d a
		void $ forkIO $ manager d aid
		return aid
	manager d aid = do
		r <- E.try (wait aid) :: IO (Either E.SomeException ())
		case r of
			Right _ -> noop
			Left e -> do
				let msg = unwords
					[ fromThreadName name
					, "crashed:", show e
					]
				hPutStrLn stderr msg
				button <- runAssistant d mkbutton
				runAssistant d $ void $
					addAlert $ (warningAlert (fromThreadName name) msg)
						{ alertButton = button }
	mkbutton = case urlrenderer of
		Nothing -> return Nothing
		Just renderer -> do
			close <- asIO1 removeAlert
			url <- liftIO $ renderUrl renderer (RestartThreadR name) []
			return $ Just $ AlertButton
				{ buttonLabel = T.pack "Restart Thread"
				, buttonUrl = url
				, buttonAction = Just close
				}

{- Waits for all named threads that have been started to finish.
 -
 - Note that if a named thread crashes, it will probably
 - cause this to crash as well. Also, named threads that are started
 - after this is called will not be waited on. -}
waitNamedThreads :: Assistant ()
waitNamedThreads = do
	m <- startedThreads <$> getDaemonStatus
	liftIO $ mapM_ (wait . fst) $ M.elems m

