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
	addAlert,
	removeAlert,
	alertWhile,
	alertWhile',
	alertDuring,
) where

import Common.Annex as X
import Assistant.Monad as X
import Assistant.Alert
import Assistant.DaemonStatus

import System.Log.Logger
import qualified Control.Exception as E
import qualified Data.Map as M

type ThreadName = String
data NamedThread = NamedThread ThreadName (Assistant ())

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

{- Returns the alert's identifier, which can be used to remove it. -}
addAlert :: DaemonStatusHandle -> Alert -> IO AlertId
addAlert dstatus alert = notifyAlert dstatus `after` modifyDaemonStatus dstatus go
	where
		go s = (s { lastAlertId = i, alertMap = m }, i)
			where
				i = nextAlertId $ lastAlertId s
				m = mergeAlert i alert (alertMap s)

removeAlert :: DaemonStatusHandle -> AlertId -> IO ()
removeAlert dstatus i = updateAlert dstatus i (const Nothing)

updateAlert :: DaemonStatusHandle -> AlertId -> (Alert -> Maybe Alert) -> IO ()
updateAlert dstatus i a = updateAlertMap dstatus $ \m -> M.update a i m

updateAlertMap :: DaemonStatusHandle -> (AlertMap -> AlertMap) -> IO ()
updateAlertMap dstatus a = notifyAlert dstatus `after` modifyDaemonStatus_ dstatus go
	where
		go s = s { alertMap = a (alertMap s) }

{- Displays an alert while performing an activity that returns True on
 - success.
 -
 - The alert is left visible afterwards, as filler.
 - Old filler is pruned, to prevent the map growing too large. -}
alertWhile :: Alert -> Assistant Bool -> Assistant Bool
alertWhile alert a = alertWhile' alert $ do
	r <- a
	return (r, r)

{- Like alertWhile, but allows the activity to return a value too. -}
alertWhile' :: Alert -> Assistant (Bool, a) -> Assistant a
alertWhile' alert a = do
	let alert' = alert { alertClass = Activity }
	dstatus <- getAssistant daemonStatusHandle
	i <- liftIO $ addAlert dstatus alert'
	(ok, r) <- a
	liftIO $ updateAlertMap dstatus $
		mergeAlert i $ makeAlertFiller ok alert'
	return r

{- Displays an alert while performing an activity, then removes it. -}
alertDuring :: Alert -> Assistant a -> Assistant a
alertDuring alert a = do
	let alert' = alert { alertClass = Activity }
	dstatus <- getAssistant daemonStatusHandle
	i <- liftIO $ addAlert dstatus alert'
	liftIO (removeAlert dstatus i) `after` a
