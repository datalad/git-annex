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
	NotificationId,
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

newtype NotificationId = NotificationId Int
	deriving (Read, Show, Eq, Ord)

{- Handle given out to an individual client. -}
data NotificationHandle = NotificationHandle NotificationBroadcaster NotificationId

newNotificationBroadcaster :: IO NotificationBroadcaster
newNotificationBroadcaster = atomically $ newTMVar []

{- Allocates a notification handle for a client to use. -}
newNotificationHandle :: NotificationBroadcaster -> IO NotificationHandle
newNotificationHandle b = NotificationHandle
	<$> pure b
	<*> addclient
	where
		addclient = do
			s <- newEmptySampleVar
			atomically $ do
				l <- takeTMVar b
				putTMVar b $ l ++ [s]
				return $ NotificationId $ length l

{- Extracts the identifier from a notification handle.
 - This can be used to eg, pass the identifier through to a WebApp. -}
notificationHandleToId :: NotificationHandle -> NotificationId
notificationHandleToId (NotificationHandle _ i) = i

notificationHandleFromId :: NotificationBroadcaster -> NotificationId -> NotificationHandle
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
waitNotification (NotificationHandle b (NotificationId i)) = do
	l <- atomically $ readTMVar b
	readSampleVar (l !! i)
