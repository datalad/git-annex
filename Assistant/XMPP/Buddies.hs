{- xmpp buddies
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.XMPP.Buddies where

import Assistant.XMPP
import Common.Annex

import Network.Protocol.XMPP
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Ord

newtype Client = Client JID
	deriving (Eq, Show)

instance Ord Client where
	compare = comparing show

data Buddy = Buddy
	{ buddyPresent :: S.Set Client
	, buddyAway :: S.Set Client
	, buddyAssistants :: S.Set Client
	}
	deriving (Eq, Show)

{- Note that the buddy map includes one buddy for the user's own JID,
 - so that we can track other git-annex assistant's sharing the same
 - account. -}
type Buddies = M.Map String Buddy

genKey :: JID -> String
genKey j = show $ JID (jidNode j) (jidDomain j) Nothing

{- Updates the buddies with XMPP presence info. -}
updateBuddies :: Presence -> Buddies -> Buddies
updateBuddies p@(Presence { presenceFrom = Just jid }) = M.alter update key
  where
	key = genKey jid
	update (Just b) = Just $ applyPresence p b
	update Nothing = newBuddy p
updateBuddies _ = id

{- Creates a new buddy based on XMPP presence info. -}
newBuddy :: Presence -> Maybe Buddy
newBuddy p
	| presenceType p == PresenceAvailable = go
	| presenceType p == PresenceUnavailable = go
	| otherwise = Nothing
  where
	go = make <$> presenceFrom p
	make _jid = applyPresence p $ Buddy
		{ buddyPresent = S.empty
		, buddyAway = S.empty
		, buddyAssistants = S.empty
		}

applyPresence :: Presence -> Buddy -> Buddy
applyPresence p b = fromMaybe b $! go <$> presenceFrom p
  where
	go jid
		| isGitAnnexPresence p = b
			{ buddyAssistants = addto $ buddyAssistants b }
		| presenceType p == PresenceAvailable = b
			{ buddyPresent = addto $ buddyPresent b
			, buddyAway = removefrom $ buddyAway b
			}
		| presenceType p == PresenceUnavailable = b
			{ buddyAway = addto $ buddyAway b
			, buddyPresent = removefrom $ buddyPresent b
			}
		| otherwise = b
	  where
		client = Client jid
		removefrom = S.filter (/= client)
		addto = S.insert client
