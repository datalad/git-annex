{- git-annex assistant push notification thread
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.Threads.PushNotifier where

import Assistant.Common
import Assistant.Pushes

thisThread :: ThreadName
thisThread = "PushNotifier"

pushNotifierThread :: PushNotifier -> NamedThread
pushNotifierThread pushnotifier = thread $ forever $ do
	waitPush pushnotifier
	-- TODO
	where
		thread = NamedThread thisThread
