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
import Data.Text (Text)
import qualified Data.Text as T
import Data.XML.Types

{- Name of the git-annex tag, in our own XML namespace.
 - (Not using a namespace URL to avoid unnecessary bloat.) -}
gitAnnexTagName :: Name
gitAnnexTagName  = Name (T.pack "git-annex") (Just $ T.pack "git-annex") Nothing

{- Creates a git-annex tag containing a particular attribute and value. -}
gitAnnexTag :: Name -> Text -> Element
gitAnnexTag attr val = Element gitAnnexTagName [(attr, [ContentText val])] []

isGitAnnexTag :: Element -> Bool
isGitAnnexTag t = elementName t == gitAnnexTagName

{- Things that a git-annex tag can inserted into. -}
class GitAnnexTaggable a where
	insertGitAnnexTag :: a -> Element -> a

	extractGitAnnexTag :: a -> Maybe Element

	hasGitAnnexTag :: a -> Bool
	hasGitAnnexTag = isJust . extractGitAnnexTag

instance GitAnnexTaggable Message where
	insertGitAnnexTag m e = m { messagePayloads = e : messagePayloads m }
	extractGitAnnexTag = headMaybe . filter isGitAnnexTag . messagePayloads

instance GitAnnexTaggable Presence where
	-- always mark extended away
	insertGitAnnexTag p e = p { presencePayloads = extendedAway : e : presencePayloads p }
	extractGitAnnexTag = headMaybe . filter isGitAnnexTag . presencePayloads

{- Gets the attr and value from a git-annex tag. -}
getGitAnnexAttrValue :: GitAnnexTaggable a => a -> Maybe (Name, Text)
getGitAnnexAttrValue a = case extractGitAnnexTag a of
	Just (Element _ [(attr, content)] []) -> Just $
		(attr, T.concat $ map unpack content)
	_ -> Nothing
  where
	unpack (ContentText t) = t
	unpack (ContentEntity t) = t

{- A presence with a git-annex tag in it. -}
gitAnnexPresence :: Element -> Presence
gitAnnexPresence = insertGitAnnexTag $ emptyPresence PresenceAvailable

{- A presence with an empty git-annex tag in it, used for letting other
 - clients know we're around and are a git-annex client. -}
gitAnnexSignature :: Presence
gitAnnexSignature = gitAnnexPresence $ Element gitAnnexTagName [] []

{- A message with a git-annex tag in it. -}
gitAnnexMessage :: Element -> Message
gitAnnexMessage = insertGitAnnexTag silentMessage

{- A notification that we've pushed to some repositories, listing their
 - UUIDs. -}
pushNotification :: [UUID] -> Presence
pushNotification = gitAnnexPresence . gitAnnexTag pushAttr . encodePushNotification

pushAttr :: Name
pushAttr = Name (T.pack "push") Nothing Nothing

uuidSep :: T.Text
uuidSep = T.pack ","

encodePushNotification :: [UUID] -> Text
encodePushNotification = T.intercalate uuidSep . map (T.pack . fromUUID)

decodePushNotification :: Text -> [UUID]
decodePushNotification = map (toUUID . T.unpack) . T.splitOn uuidSep

{- A request for other git-annex clients to send presence. -}
presenceQuery :: Presence
presenceQuery = gitAnnexPresence $ gitAnnexTag queryAttr T.empty

queryAttr :: Name
queryAttr = Name (T.pack "query") Nothing Nothing

{- A notification about a stage of pairing. -}
pairingNotification :: PairStage -> UUID -> JID -> JID -> Message
pairingNotification pairstage u tojid fromjid =
	(gitAnnexMessage tag)
		{ messageTo = Just tojid
		, messageFrom = Just fromjid
		}
  where
	tag = gitAnnexTag pairAttr $
		encodePairingNotification pairstage u

pairAttr :: Name
pairAttr = Name (T.pack "pair") Nothing Nothing

encodePairingNotification :: PairStage -> UUID -> Text
encodePairingNotification pairstage u = T.unwords $ map T.pack
	[ show pairstage
	, fromUUID u
	]

decodePairingNotification :: Text -> Message -> Maybe NetMessage
decodePairingNotification t msg = parse $ words $ T.unpack t
  where
	parse [stage, u] = PairingNotification
		<$> readish stage
		<*> (formatJID <$> messageFrom msg)
		<*> pure (toUUID u)
	parse _ = Nothing

{- The JID without the client part. -}
baseJID :: JID -> JID
baseJID j = JID (jidNode j) (jidDomain j) Nothing

{- An XMPP chat message with an empty body. This should not be displayed
 - by clients, but can be used for communications. -}
silentMessage :: Message
silentMessage = (emptyMessage MessageChat)
	{ messagePayloads = [ emptybody ] }
  where
	emptybody = Element
		{ elementName = Name (T.pack "body") Nothing Nothing
		, elementAttributes = []
		, elementNodes = []
		}

{- Add to a presence to mark its client as extended away. -}
extendedAway :: Element
extendedAway = Element (Name (T.pack "show") Nothing Nothing) []
	[NodeContent $ ContentText $ T.pack "xa"]
