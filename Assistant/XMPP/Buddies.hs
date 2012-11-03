{- xmpp buddies
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.XMPP.Buddies where

import Assistant.XMPP
import Common.Annex
import Assistant.Types.Buddies

import Network.Protocol.XMPP
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T

genBuddyID :: JID -> BuddyID
genBuddyID j = BuddyID $ formatJID j

genKey :: JID -> BuddyKey
genKey j = BuddyKey $ formatJID $ JID (jidNode j) (jidDomain j) Nothing

{- Summary of info about a buddy.
 -
 - If the buddy has no clients at all anymore, returns Nothing. -}
buddySummary :: Buddy -> Maybe (Text, Bool, Bool, BuddyID)
buddySummary b = case clients of
	((Client j):_) -> Just (buddyname j, away, canpair, genBuddyID j)
	[] -> Nothing
  where
	buddyname j = maybe (T.pack "") strNode (jidNode j)
	away = S.null (buddyPresent b) && S.null (buddyAssistants b)
	canpair = not $ S.null (buddyAssistants b)
	clients = S.toList $ buddyPresent b `S.union` buddyAway b `S.union` buddyAssistants b

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
