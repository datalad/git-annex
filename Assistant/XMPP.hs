{- core xmpp support
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.XMPP where

import Assistant.Common
import Assistant.Types.NetMessager
import Assistant.Pairing

import Network.Protocol.XMPP
import qualified Data.Text as T
import Data.XML.Types

{- A presence with a git-annex tag in it. -}
gitAnnexPresence :: Element -> Presence
gitAnnexPresence tag = (emptyPresence PresenceAvailable)
	{ presencePayloads = [extendedAway, tag] }
  where
	extendedAway = Element (Name (T.pack "show") Nothing Nothing) []
		[NodeContent $ ContentText $ T.pack "xa"]

{- Does a presence contain a git-annex tag? -}
isGitAnnexPresence :: Presence -> Bool
isGitAnnexPresence p = any isGitAnnexTag (presencePayloads p)

{- Name of a git-annex tag, in our own XML namespace.
 - (Not using a namespace URL to avoid unnecessary bloat.) -}
gitAnnexTagName :: Name
gitAnnexTagName  = Name (T.pack "git-annex") (Just $ T.pack "git-annex") Nothing

isGitAnnexTag :: Element -> Bool
isGitAnnexTag t = elementName t == gitAnnexTagName

{- A git-annex tag, to let other clients know we're a git-annex client too. -}
gitAnnexSignature :: Element
gitAnnexSignature = Element gitAnnexTagName [] []

queryAttr :: Name
queryAttr = Name (T.pack "query") Nothing Nothing

pushAttr :: Name
pushAttr = Name (T.pack "push") Nothing Nothing

isAttr :: Name -> (Name, [Content]) -> Bool
isAttr attr (k, _) = k == attr

uuidSep :: T.Text
uuidSep = T.pack ","

{- git-annex tag with one push attribute per UUID pushed to. -}
encodePushNotification :: [UUID] -> Element
encodePushNotification us = Element gitAnnexTagName
	[(pushAttr, [ContentText pushvalue])] []
  where
	pushvalue = T.intercalate uuidSep $
		map (T.pack . fromUUID) us

decodePushNotification :: Element -> Maybe [UUID]
decodePushNotification (Element name attrs _nodes)
	| name == gitAnnexTagName && not (null us) = Just us
	| otherwise = Nothing
  where
	us = map (toUUID . T.unpack) $
		concatMap (T.splitOn uuidSep . T.concat . map fromContent . snd) $
		filter ispush attrs
	ispush (k, _) = k == pushAttr
	fromContent (ContentText t) = t
	fromContent (ContentEntity t) = t

pushNotification :: [UUID] -> Presence
pushNotification = gitAnnexPresence . encodePushNotification

{- A request for other git-annex clients to send presence. -}
presenceQuery :: Presence
presenceQuery = gitAnnexPresence $ Element gitAnnexTagName
	[ (queryAttr, [ContentText T.empty]) ]
	[]

isPresenceQuery :: Presence -> Bool
isPresenceQuery p = case filter isGitAnnexTag (presencePayloads p) of
	[] -> False
	((Element _name attrs _nodes):_) -> any (isAttr queryAttr) attrs

{- A notification about a stage of pairing. Sent as an XMPP ping. 
 - The pairing info is sent using its id attribute; it also has a git-annex
 - tag to identify it as from us. -}
pairingNotification :: PairStage -> UUID -> JID -> JID -> IQ
pairingNotification pairstage u tojid fromjid = (emptyIQ IQGet)
	{ iqTo = Just tojid
	, iqFrom = Just fromjid
	, iqID = Just $ T.unwords $ map T.pack
		[ show pairstage
		, fromUUID u
		]
	, iqPayload = Just gitAnnexSignature
	}

decodePairingNotification :: IQ -> Maybe NetMessage
decodePairingNotification iq@(IQ { iqPayload = Just elt })
	| isGitAnnexTag elt = parseid =<< words . T.unpack <$> iqID iq
	| otherwise = Nothing
  where
	parseid [stage, u] = 
		PairingNotification
			<$> readish stage
			<*> (formatJID <$> iqFrom iq)
			<*> pure (toUUID u)
	parseid _ = Nothing
decodePairingNotification _ = Nothing
