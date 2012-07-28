{- notification broadcaster
 -
 - This is used to allow clients to block until there is a new notification
 - that some thing occurred. It does not communicate what the change is,
 - it only provides blocking reads to wait on notifications.
 -
 - Multiple clients are supported. Each has a unique id.
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.NotificationBroadCaster (
	NotificationBroadCaster,
	NotificationHandle,
	newNotificationBroadCaster,
	newNotificationHandle,
	notificationHandleToId,
	notificationHandleFromId,
	sendNotification,
	waitNotification,
) where

import Common

import Control.Concurrent.STM
import Control.Concurrent.SampleVar

{- One SampleVar per client. The TMVar is never empty, so never blocks. -}
type NotificationBroadCaster = TMVar [SampleVar ()]

{- Handle given out to an individual client. -}
data NotificationHandle = NotificationHandle NotificationBroadCaster Int

newNotificationBroadCaster :: IO NotificationBroadCaster
newNotificationBroadCaster = atomically (newTMVar [])

{- Allocates a notification handle for a client to use. -}
newNotificationHandle :: NotificationBroadCaster -> IO NotificationHandle
newNotificationHandle b = NotificationHandle
	<$> pure b
	<*> addclient b
	where
		addclient b = do
			s <- newEmptySampleVar
			atomically $ do
				l <- readTMVar b
				putTMVar b $ l ++ [s]
				return $ length l

{- Extracts the Int identifier from a notification handle.
 - This can be used to eg, pass the identifier through to a WebApp. -}
notificationHandleToId :: NotificationHandle -> Int
notificationHandleToId (NotificationHandle _ i) = i

{- Given a NotificationBroadCaster, and an Int identifier, recreates the
 - NotificationHandle. -}
notificationHandleFromId :: NotificationBroadCaster -> Int -> NotificationHandle
notificationHandleFromId = NotificationHandle

{- Sends a notification to all clients. -}
sendNotification :: NotificationBroadCaster -> IO ()
sendNotification b = do
	l <- atomically $ readTMVar b
	mapM_ notify l
	where
		notify s = writeSampleVar s ()

{- Used by a client to block until a new notification is available since
 - the last time it tried. -}
waitNotification :: NotificationHandle -> IO ()
waitNotification (NotificationHandle b i) = do
	l <- atomically $ readTMVar b
	readSampleVar (l !! i)
