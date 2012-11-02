{- git-annex assistant buddies
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Assistant.Types.Buddies where

import Common.Annex

import qualified Data.Map as M
import Control.Concurrent.STM
import Utility.NotificationBroadcaster

{- When XMPP is enabled, this is an XMPP buddy map.
 - Otherwise, it's an empty map, for simplicity. -}
#ifdef WITH_XMPP
import Assistant.XMPP.Buddies
#else
type Buddies = M.Map String Buddy
data Buddy
	deriving (Eq)
#endif

{- A list of buddies, and a way to notify when it changes. -}
type BuddyList = (TMVar Buddies, NotificationBroadcaster)

noBuddies :: Buddies
noBuddies = M.empty

newBuddyList :: IO BuddyList
newBuddyList = (,)
	<$> atomically (newTMVar noBuddies)
	<*> newNotificationBroadcaster

getBuddyList :: BuddyList -> IO [Buddy]
getBuddyList (v, _) = M.elems <$> atomically (readTMVar v)

{- Applies a function to modify the buddy list, and if it's changed,
 - sends notifications to any listeners. -}
updateBuddyList :: (Buddies -> Buddies) -> BuddyList -> IO ()
updateBuddyList a (v, caster) = do
	changed <- atomically $ do
		buds <- takeTMVar v
		let buds' = a buds
		putTMVar v buds'
		return $ buds /= buds'
	when changed $
		sendNotification caster

{- Allocates a notification handle for a client to use to listen for
 - changes to the buddy list. -}
newBuddyListNotificationHandle :: BuddyList -> IO NotificationHandle
newBuddyListNotificationHandle (_, caster) = newNotificationHandle caster
