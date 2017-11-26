{- git-annex assistant named threads.
 -
 - Copyright 2012 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Assistant.NamedThread where

import Annex.Common
import Assistant.Types.NamedThread
import Assistant.Types.ThreadName
import Assistant.Types.DaemonStatus
import Assistant.Types.UrlRenderer
import Assistant.DaemonStatus
import Assistant.Monad
import Utility.NotificationBroadcaster

import Control.Concurrent
import Control.Concurrent.Async
import qualified Data.Map as M
import qualified Control.Exception as E

#ifdef WITH_WEBAPP
import Assistant.WebApp.Types
import Assistant.Types.Alert
import Assistant.Alert
import qualified Data.Text as T
#endif

{- Starts a named thread, if it's not already running.
 -
 - Named threads are run by a management thread, so if they crash
 - an alert is displayed, allowing the thread to be restarted. -}
startNamedThread :: UrlRenderer -> NamedThread -> Assistant ()
startNamedThread urlrenderer (NamedThread afterstartupsanitycheck name a) = do
	m <- startedThreads <$> getDaemonStatus
	case M.lookup name m of
		Nothing -> start
		Just (aid, _) -> do
			r <- liftIO (E.try (poll aid) :: IO (Either E.SomeException (Maybe (Either E.SomeException ()))))
			case r of
				Right Nothing -> noop
				_ -> start
  where
	start
		| afterstartupsanitycheck = do
			status <- getDaemonStatus
			h <- liftIO $ newNotificationHandle False $
				startupSanityCheckNotifier status
			startwith $ runmanaged $
				liftIO $ waitNotification h
		| otherwise = startwith $ runmanaged noop
	startwith runner = do
		d <- getAssistant id
		aid <- liftIO $ runner $ d { threadName = name }
		restart <- asIO $ startNamedThread urlrenderer (NamedThread False name a)
		modifyDaemonStatus_ $ \s -> s
			{ startedThreads = M.insertWith' const name (aid, restart) (startedThreads s) }
	runmanaged first d = do
		aid <- async $ runAssistant d $ do
			void first
			a
		void $ forkIO $ manager d aid
		return aid
	manager d aid = do
		r <- E.try (wait aid) :: IO (Either E.SomeException ())
		case r of
			Right _ -> noop
			Left e -> do
				let msg = unwords
					[ fromThreadName $ threadName d
					, "crashed:", show e
					]
				hPutStrLn stderr msg
#ifdef WITH_WEBAPP
				button <- runAssistant d $ mkAlertButton True
					(T.pack "Restart Thread")
					urlrenderer 
					(RestartThreadR name)
				runAssistant d $ void $ addAlert $
					(warningAlert (fromThreadName name) msg)
						{ alertButtons = [button] }
#endif

namedThreadId :: NamedThread -> Assistant (Maybe ThreadId)
namedThreadId (NamedThread _ name _) = do
	m <- startedThreads <$> getDaemonStatus
	return $ asyncThreadId . fst <$> M.lookup name m

{- Waits for all named threads that have been started to finish.
 -
 - Note that if a named thread crashes, it will probably
 - cause this to crash as well. Also, named threads that are started
 - after this is called will not be waited on. -}
waitNamedThreads :: Assistant ()
waitNamedThreads = do
	m <- startedThreads <$> getDaemonStatus
	liftIO $ mapM_ (wait . fst) $ M.elems m

