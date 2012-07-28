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

module Utility.NotificationBroadcaster (
	NotificationBroadcaster,
	NotificationHandle,
	newNotificationBroadcaster,
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
type NotificationBroadcaster = TMVar [SampleVar ()]

{- Handle given out to an individual client. -}
data NotificationHandle = NotificationHandle NotificationBroadcaster Int

newNotificationBroadcaster :: IO NotificationBroadcaster
newNotificationBroadcaster = atomically (newTMVar [])

{- Allocates a notification handle for a client to use. -}
newNotificationHandle :: NotificationBroadcaster -> IO NotificationHandle
newNotificationHandle b = NotificationHandle
	<$> pure b
	<*> addclient
	where
		addclient = do
			s <- newEmptySampleVar
			atomically $ do
				l <- readTMVar b
				putTMVar b $ l ++ [s]
				return $ length l

{- Extracts the Int identifier from a notification handle.
 - This can be used to eg, pass the identifier through to a WebApp. -}
notificationHandleToId :: NotificationHandle -> Int
notificationHandleToId (NotificationHandle _ i) = i

{- Given a NotificationBroadcaster, and an Int identifier, recreates the
 - NotificationHandle. -}
notificationHandleFromId :: NotificationBroadcaster -> Int -> NotificationHandle
notificationHandleFromId = NotificationHandle

{- Sends a notification to all clients. -}
sendNotification :: NotificationBroadcaster -> IO ()
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
