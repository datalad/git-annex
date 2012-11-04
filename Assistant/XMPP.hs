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

pairAttr :: Name
pairAttr = Name (T.pack "pair") Nothing Nothing

isAttr :: Name -> (Name, [Content]) -> Bool
isAttr attr (k, _) = k == attr

getAttr :: Element -> Name -> Maybe T.Text
getAttr (Element _name attrs _nodes) name =
	T.concat . map unpack . snd <$> headMaybe (filter (isAttr name) attrs)
  where
	unpack (ContentText t) = t
	unpack (ContentEntity t) = t

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

{- A notification about a stage of pairing, sent as directed presence
 - to all clients of a jid. 
 -
 - For PairReq, the directed presence is followed by a second presence
 - without the pair notification. This is done because XMPP servers
 - resend the last directed presence periodically, which can make
 - the pair request alert be re-displayed annoyingly. For PairAck and
 - PairDone, that resending is a desirable feature, as it helps ensure
 - clients see them.
 -}
encodePairingNotification :: PairStage -> UUID -> JID -> JID -> [Presence]
encodePairingNotification pairstage u tojid fromjid
	| pairstage == PairReq = [send, clear]
	| otherwise = [send]
	where
		send = directed $ gitAnnexPresence $ Element gitAnnexTagName
			[(pairAttr, [ContentText content])] []
		clear = directed $ gitAnnexPresence gitAnnexSignature

		directed p = p
			{ presenceTo = Just $ baseJID tojid
			, presenceFrom = Just fromjid
			}	

		content = mkPairingContent pairstage u

{- A notification about a stage of pairing. Sent to self as an XMPP IQ.
 - Directed presence is not used for self-messaging presence because
 - some XMPP clients seem very confused by it. Google Talk has been
 - observed leaking self-directed presence to other friends, seeming
 - to think it sets the visible presence.
 - 
 - The pairing info is sent using its id attribute; it also has a git-annex
 - tag to identify it as from us. -}
encodeSelfPairingNotification :: PairStage -> UUID -> JID -> JID -> IQ
encodeSelfPairingNotification pairstage u tojid fromjid = (emptyIQ IQGet)
	{ iqTo = Just tojid
	, iqFrom = Just fromjid
	, iqID = Just $ mkPairingContent pairstage u
	, iqPayload = Just gitAnnexSignature
	}

decodePairingNotification :: Presence -> Maybe NetMessage
decodePairingNotification p = case filter isGitAnnexTag (presencePayloads p) of
	[] -> Nothing
	(elt:_) -> parsePairingContent (presenceFrom p) =<< getAttr elt pairAttr

decodeSelfPairingNotification :: IQ -> Maybe NetMessage
decodeSelfPairingNotification iq@(IQ { iqPayload = Just elt })
	| isGitAnnexTag elt = parsePairingContent (iqFrom iq) =<< iqID iq
	| otherwise = Nothing
decodeSelfPairingNotification _ = Nothing

mkPairingContent :: PairStage -> UUID -> T.Text
mkPairingContent pairstage u = T.unwords $ map T.pack
	[ show pairstage
	, fromUUID u
	]

parsePairingContent :: Maybe JID -> T.Text -> Maybe NetMessage
parsePairingContent jid t = parse $ words $ T.unpack t
  where
	parse [stage, u] = PairingNotification
		<$> readish stage
		<*> (formatJID <$> jid)
		<*> pure (toUUID u)
	parse _ = Nothing

{- The JID without the client part. -}
baseJID :: JID -> JID
baseJID j = JID (jidNode j) (jidDomain j) Nothing
