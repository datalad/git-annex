{- core xmpp support
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Assistant.XMPP where

import Assistant.Common
import Assistant.Types.NetMessager
import Assistant.Pairing

import Network.Protocol.XMPP hiding (Node)
import Data.Text (Text)
import qualified Data.Text as T
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import Data.XML.Types
import qualified Codec.Binary.Base64 as B64

{- Name of the git-annex tag, in our own XML namespace.
 - (Not using a namespace URL to avoid unnecessary bloat.) -}
gitAnnexTagName :: Name
gitAnnexTagName  = "{git-annex}git-annex"

{- Creates a git-annex tag containing a particular attribute and value. -}
gitAnnexTag :: Name -> Text -> Element
gitAnnexTag attr val = gitAnnexTagContent attr val []

{- Also with some content. -}
gitAnnexTagContent :: Name -> Text -> [Node] -> Element
gitAnnexTagContent attr val = Element gitAnnexTagName [(attr, [ContentText val])]

isGitAnnexTag :: Element -> Bool
isGitAnnexTag t = elementName t == gitAnnexTagName

{- Things that a git-annex tag can inserted into. -}
class GitAnnexTaggable a where
	insertGitAnnexTag :: a -> Element -> a

	extractGitAnnexTag :: a -> Maybe Element

	hasGitAnnexTag :: a -> Bool
	hasGitAnnexTag = isJust . extractGitAnnexTag

instance GitAnnexTaggable Message where
	insertGitAnnexTag m elt = m { messagePayloads = elt : messagePayloads m }
	extractGitAnnexTag = headMaybe . filter isGitAnnexTag . messagePayloads

instance GitAnnexTaggable Presence where
	-- always mark extended away
	insertGitAnnexTag p elt = p { presencePayloads = extendedAway : elt : presencePayloads p }
	extractGitAnnexTag = headMaybe . filter isGitAnnexTag . presencePayloads

data GitAnnexTagInfo = GitAnnexTagInfo
	{ tagAttr :: Name
	, tagValue :: Text
	, tagElement :: Element
	}

gitAnnexTagInfo :: GitAnnexTaggable a => a -> Maybe GitAnnexTagInfo
gitAnnexTagInfo v = case extractGitAnnexTag v of
	{- Each git-annex tag has a single attribute. -}
	Just (tag@(Element _ [(attr, _)] _)) -> GitAnnexTagInfo
		<$> pure attr
		<*> attributeText attr tag
		<*> pure tag
	_ -> Nothing

{- A presence with a git-annex tag in it. -}
gitAnnexPresence :: Element -> Presence
gitAnnexPresence = insertGitAnnexTag $ emptyPresence PresenceAvailable

{- A presence with an empty git-annex tag in it, used for letting other
 - clients know we're around and are a git-annex client. -}
gitAnnexSignature :: Presence
gitAnnexSignature = gitAnnexPresence $ Element gitAnnexTagName [] []

{- A message with a git-annex tag in it. -}
gitAnnexMessage :: Element -> JID -> JID -> Message
gitAnnexMessage elt tojid fromjid = (insertGitAnnexTag silentMessage elt)
	{ messageTo = Just tojid
	, messageFrom = Just fromjid
	}

{- A notification that we've pushed to some repositories, listing their
 - UUIDs. -}
pushNotification :: [UUID] -> Presence
pushNotification = gitAnnexPresence . gitAnnexTag pushAttr . encodePushNotification

pushAttr :: Name
pushAttr = "push"

uuidSep :: Text
uuidSep = ","

encodePushNotification :: [UUID] -> Text
encodePushNotification = T.intercalate uuidSep . map (T.pack . fromUUID)

decodePushNotification :: Text -> [UUID]
decodePushNotification = map (toUUID . T.unpack) . T.splitOn uuidSep

{- A request for other git-annex clients to send presence. -}
presenceQuery :: Presence
presenceQuery = gitAnnexPresence $ gitAnnexTag queryAttr T.empty

queryAttr :: Name
queryAttr = "query"

{- A notification about a stage of pairing. -}
pairingNotification :: PairStage -> UUID -> JID -> JID -> Message
pairingNotification pairstage u = gitAnnexMessage $ 
	gitAnnexTag pairAttr $ encodePairingNotification pairstage u

pairAttr :: Name
pairAttr = "pair"

encodePairingNotification :: PairStage -> UUID -> Text
encodePairingNotification pairstage u = T.unwords $ map T.pack
	[ show pairstage
	, fromUUID u
	]

decodePairingNotification :: Message -> GitAnnexTagInfo -> Maybe NetMessage
decodePairingNotification m = parse . words . T.unpack . tagValue
  where
	parse [stage, u] = PairingNotification
		<$> readish stage
		<*> (formatJID <$> messageFrom m)
		<*> pure (toUUID u)
	parse _ = Nothing

canPush :: JID -> JID -> Message
canPush = gitAnnexMessage $ gitAnnexTag canPushAttr T.empty

decodeCanPush :: Message -> GitAnnexTagInfo -> Maybe NetMessage
decodeCanPush m _ = Pushing <$> (formatJID <$> messageFrom m) <*> pure CanPush

canPushAttr :: Name
canPushAttr = "canpush"

pushRequest :: JID -> JID -> Message
pushRequest = gitAnnexMessage $ gitAnnexTag pushRequestAttr T.empty

decodePushRequest :: Message -> GitAnnexTagInfo -> Maybe NetMessage
decodePushRequest m _ = Pushing <$> (formatJID <$> messageFrom m) <*> pure PushRequest

pushRequestAttr :: Name
pushRequestAttr = "pushrequest"

startingPush :: JID -> JID -> Message
startingPush = gitAnnexMessage $ gitAnnexTag startingPushAttr T.empty

startingPushAttr :: Name
startingPushAttr = "startingpush"

decodeStartingPush :: Message -> GitAnnexTagInfo -> Maybe NetMessage
decodeStartingPush m _ = Pushing <$> (formatJID <$> messageFrom m) <*> pure StartingPush

receivePackOutput :: ByteString -> JID -> JID -> Message
receivePackOutput = gitAnnexMessage .
	gitAnnexTagContent receivePackAttr T.empty . encodeTagContent

receivePackAttr :: Name
receivePackAttr = "rp"

decodeReceivePackOutput :: Message -> GitAnnexTagInfo -> Maybe NetMessage
decodeReceivePackOutput m i = Pushing 
	<$> (formatJID <$> messageFrom m)
	<*> (ReceivePackOutput <$> decodeTagContent (tagElement i))

sendPackOutput :: ByteString -> JID -> JID -> Message
sendPackOutput = gitAnnexMessage .
	gitAnnexTagContent sendPackAttr T.empty . encodeTagContent

sendPackAttr :: Name
sendPackAttr = "sp"

decodeSendPackOutput :: Message -> GitAnnexTagInfo -> Maybe NetMessage
decodeSendPackOutput m i = Pushing
	<$> (formatJID <$> messageFrom m)
	<*> (SendPackOutput <$> decodeTagContent (tagElement i))

receivePackDone :: ExitCode -> JID -> JID -> Message
receivePackDone = gitAnnexMessage . gitAnnexTag receivePackDoneAttr . T.pack . show . toi
  where
	toi (ExitSuccess) = 0
	toi (ExitFailure i) = i

decodeReceivePackDone :: Message -> GitAnnexTagInfo -> Maybe NetMessage
decodeReceivePackDone m i = Pushing
	<$> (formatJID <$> messageFrom m)
	<*> (ReceivePackDone . convert <$> readish (T.unpack $ tagValue i))
  where
	convert 0 = ExitSuccess
	convert n = ExitFailure n

receivePackDoneAttr :: Name
receivePackDoneAttr = "rpdone"

{- Base 64 encoding a ByteString to use as the content of a tag. -}
encodeTagContent :: ByteString -> [Node]
encodeTagContent b = [NodeContent $ ContentText $ T.pack $ B64.encode $ B.unpack b]

decodeTagContent :: Element -> Maybe ByteString
decodeTagContent elt = B.pack <$> B64.decode s
  where
	s = T.unpack $ T.concat $ elementText elt

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
		{ elementName = "body"
		, elementAttributes = []
		, elementNodes = []
		}

{- Add to a presence to mark its client as extended away. -}
extendedAway :: Element
extendedAway = Element "show" [] [NodeContent $ ContentText "xa"]
