{- git-annex assistant buddies
 -
 - Copyright 2012 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Assistant.Types.Buddies where

import Annex.Common

import qualified Data.Map as M
import Control.Concurrent.STM
import Utility.NotificationBroadcaster
import Data.Text as T

{- For simplicity, dummy types are defined even when XMPP is disabled. -}
#ifdef WITH_XMPP
import Network.Protocol.XMPP
import Data.Set as S
import Data.Ord

newtype Client = Client JID
	deriving (Eq, Show)

instance Ord Client where
	compare = comparing show

data Buddy = Buddy
	{ buddyPresent :: S.Set Client
	, buddyAway :: S.Set Client
	, buddyAssistants :: S.Set Client
	, buddyPairing :: Bool
	}
#else
data Buddy = Buddy
#endif
	deriving (Eq, Show)

data BuddyKey = BuddyKey T.Text
	deriving (Eq, Ord, Show, Read)

data PairKey = PairKey UUID T.Text
	deriving (Eq, Ord, Show, Read)

type Buddies = M.Map BuddyKey Buddy

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

getBuddy :: BuddyKey -> BuddyList -> IO (Maybe Buddy)
getBuddy k (v, _) = M.lookup k <$> atomically (readTMVar v)

getBuddyBroadcaster :: BuddyList -> NotificationBroadcaster
getBuddyBroadcaster (_, h) = h

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
