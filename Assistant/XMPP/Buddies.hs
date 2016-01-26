{- xmpp buddies
 -
 - Copyright 2012 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.XMPP.Buddies where

import Assistant.XMPP
import Annex.Common
import Assistant.Types.Buddies

import Network.Protocol.XMPP
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T

genBuddyKey :: JID -> BuddyKey
genBuddyKey j = BuddyKey $ formatJID $ baseJID j

buddyName :: JID -> Text
buddyName j = maybe (T.pack "") strNode (jidNode j)

ucFirst :: Text -> Text
ucFirst s = let (first, rest) = T.splitAt 1 s
	in T.concat [T.toUpper first, rest]

{- Summary of info about a buddy.
 -
 - If the buddy has no clients at all anymore, returns Nothing. -}
buddySummary :: [JID] -> Buddy -> Maybe (Text, Bool, Bool, Bool, BuddyKey)
buddySummary pairedwith b = case clients of
	((Client j):_) -> Just (buddyName j, away, canpair, alreadypaired j, genBuddyKey j)
	[] -> Nothing
  where
	away = S.null (buddyPresent b) && S.null (buddyAssistants b)
	canpair = not $ S.null (buddyAssistants b)
	clients = S.toList $ buddyPresent b `S.union` buddyAway b `S.union` buddyAssistants b
	alreadypaired j = baseJID j `elem` pairedwith

{- Updates the buddies with XMPP presence info. -}
updateBuddies :: Presence -> Buddies -> Buddies
updateBuddies p@(Presence { presenceFrom = Just jid }) = M.alter update key
  where
	key = genBuddyKey jid
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
		, buddyPairing = False
		}

applyPresence :: Presence -> Buddy -> Buddy
applyPresence p b = fromMaybe b $! go <$> presenceFrom p
  where
	go jid
		| presenceType p == PresenceUnavailable = b
			{ buddyAway = addto $ buddyAway b
			, buddyPresent = removefrom $ buddyPresent b
			, buddyAssistants = removefrom $ buddyAssistants b
			}
		| hasGitAnnexTag p = b
			{ buddyAssistants = addto $ buddyAssistants b
			, buddyAway = removefrom $ buddyAway b }
		| presenceType p == PresenceAvailable = b
			{ buddyPresent = addto $ buddyPresent b
			, buddyAway = removefrom $ buddyAway b
			}
		| otherwise = b
	  where
		client = Client jid
		removefrom = S.filter (/= client)
		addto = S.insert client
