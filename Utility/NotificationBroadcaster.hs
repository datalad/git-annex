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
	checkNotification,
) where

import Common

import Control.Concurrent.STM

{- One TMVar per client, which are empty when no notification is pending,
 - and full when a notification has been sent but not yet seen by the
 - client. The list TMVar is never empty, so never blocks. -}
type NotificationBroadcaster = TMVar [TMVar ()]

newtype NotificationId = NotificationId Int
	deriving (Read, Show, Eq, Ord)

{- Handle given out to an individual client. -}
data NotificationHandle = NotificationHandle NotificationBroadcaster NotificationId

newNotificationBroadcaster :: IO NotificationBroadcaster
newNotificationBroadcaster = atomically $ newTMVar []

{- Allocates a notification handle for a client to use.
 -
 - An immediate notification can be forced the first time waitNotification
 - is called on the handle. This is useful in cases where a notification
 - may be sent while the new handle is being constructed. Normally,
 - such a notification would be missed. Forcing causes extra work,
 - but ensures such notifications get seen.
 -}
newNotificationHandle :: Bool -> NotificationBroadcaster -> IO NotificationHandle
newNotificationHandle force b = NotificationHandle
	<$> pure b
	<*> addclient
  where
	addclient = atomically $ do
		s <- if force
			then newTMVar ()
			else newEmptyTMVar
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
	notify s = atomically $
		whenM (isEmptyTMVar s) $
			putTMVar s ()

{- Used by a client to block until a new notification is available since
 - the last time it tried. -}
waitNotification :: NotificationHandle -> IO ()
waitNotification (NotificationHandle b (NotificationId i)) = do
	l <- atomically $ readTMVar b
	atomically $ takeTMVar (l !! i)

{- Used by a client to check if there has been a new notification since the
 - last time it checked, without blocking. -}
checkNotification :: NotificationHandle -> IO Bool
checkNotification (NotificationHandle b (NotificationId i)) = do
	l <- atomically $ readTMVar b
	maybe False (const True) <$> atomically (tryTakeTMVar (l !! i))
