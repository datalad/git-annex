{- core xmpp support
 -
 - Copyright 2012-2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Assistant.XMPP where

import Assistant.Common
import Assistant.Types.NetMessager
import Assistant.Pairing
import Git.Sha (extractSha)

import Network.Protocol.XMPP hiding (Node)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Map as M
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
	-- always mark extended away and set presence priority to negative
	insertGitAnnexTag p elt = p
		{ presencePayloads = extendedAway : negativePriority : elt : presencePayloads p }
	extractGitAnnexTag = headMaybe . filter isGitAnnexTag . presencePayloads

data GitAnnexTagInfo = GitAnnexTagInfo
	{ tagAttr :: Name
	, tagValue :: Text
	, tagElement :: Element
	}

type Decoder = Message -> GitAnnexTagInfo -> Maybe NetMessage

gitAnnexTagInfo :: GitAnnexTaggable a => a -> Maybe GitAnnexTagInfo
gitAnnexTagInfo v = case extractGitAnnexTag v of
	{- Each git-annex tag has a single attribute. -}
	Just (tag@(Element _ [(attr, _)] _)) -> GitAnnexTagInfo
		<$> pure attr
		<*> attributeText attr tag
		<*> pure tag
	_ -> Nothing

{- A presence with a git-annex tag in it.
 - Also includes a status tag, which may be visible in XMPP clients. -}
gitAnnexPresence :: Element -> Presence
gitAnnexPresence = insertGitAnnexTag $ addStatusTag $ emptyPresence PresenceAvailable
  where
	addStatusTag p = p
		{ presencePayloads = status : presencePayloads p }
	status = Element "status" [] [statusMessage]
	statusMessage = NodeContent $ ContentText $ T.pack "git-annex"

{- A presence with an empty git-annex tag in it, used for letting other
 - clients know we're around and are a git-annex client. -}
gitAnnexSignature :: Presence
gitAnnexSignature = gitAnnexPresence $ Element gitAnnexTagName [] []

{- XMPP client to server ping -}
xmppPing :: JID -> IQ
xmppPing selfjid = (emptyIQ IQGet)
	{ iqID = Just "c2s1"
	, iqFrom = Just selfjid
	, iqTo = Just $ JID Nothing (jidDomain selfjid) Nothing
	, iqPayload = Just $ Element xmppPingTagName [] []
	}

xmppPingTagName :: Name
xmppPingTagName = "{urn:xmpp}ping"

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

encodePushNotification :: [UUID] -> Text
encodePushNotification = T.intercalate uuidSep . map (T.pack . fromUUID)

decodePushNotification :: Text -> [UUID]
decodePushNotification = map (toUUID . T.unpack) . T.splitOn uuidSep

uuidSep :: Text
uuidSep = ","

{- A request for other git-annex clients to send presence. -}
presenceQuery :: Presence
presenceQuery = gitAnnexPresence $ gitAnnexTag queryAttr T.empty

{- A notification about a stage of pairing. -}
pairingNotification :: PairStage -> UUID -> JID -> JID -> Message
pairingNotification pairstage u = gitAnnexMessage $ 
	gitAnnexTag pairAttr $ encodePairingNotification pairstage u

encodePairingNotification :: PairStage -> UUID -> Text
encodePairingNotification pairstage u = T.unwords $ map T.pack
	[ show pairstage
	, fromUUID u
	]

decodePairingNotification :: Decoder
decodePairingNotification m = parse . words . T.unpack . tagValue
  where
	parse [stage, u] = PairingNotification
		<$> readish stage
		<*> (formatJID <$> messageFrom m)
		<*> pure (toUUID u)
	parse _ = Nothing

pushMessage :: PushStage -> JID -> JID -> Message
pushMessage = gitAnnexMessage . encode
  where
	encode (CanPush u shas) =
		gitAnnexTag canPushAttr $ T.pack $ unwords $
			fromUUID u : map show shas
	encode (PushRequest u) =
		gitAnnexTag pushRequestAttr $ T.pack $ fromUUID u
	encode (StartingPush u) =
		gitAnnexTag startingPushAttr $ T.pack $ fromUUID u
	encode (ReceivePackOutput n b) = 
		gitAnnexTagContent receivePackAttr (val n) $ encodeTagContent b
	encode (SendPackOutput n b) =
		gitAnnexTagContent sendPackAttr (val n) $ encodeTagContent b
	encode (ReceivePackDone code) =
		gitAnnexTag receivePackDoneAttr $ val $ encodeExitCode code
	val = T.pack . show

decodeMessage :: Message -> Maybe NetMessage
decodeMessage m = decode =<< gitAnnexTagInfo m
  where
	decode i = M.lookup (tagAttr i) decoders >>= rundecoder i
	rundecoder i d = d m i
	decoders = M.fromList $ zip
		[ pairAttr
		, canPushAttr
		, pushRequestAttr
		, startingPushAttr
		, receivePackAttr
		, sendPackAttr
		, receivePackDoneAttr
		]
		[ decodePairingNotification
		, pushdecoder $ shasgen CanPush
		, pushdecoder $ gen PushRequest
		, pushdecoder $ gen StartingPush
		, pushdecoder $ seqgen ReceivePackOutput
		, pushdecoder $ seqgen SendPackOutput
		, pushdecoder $
			fmap (ReceivePackDone . decodeExitCode) . readish .
				T.unpack . tagValue
		]
	pushdecoder a m' i = Pushing
		<$> (formatJID <$> messageFrom m')
		<*> a i
	gen c i = c . toUUID <$> headMaybe (words (T.unpack (tagValue i)))
	seqgen c i = do
	  	packet <- decodeTagContent $ tagElement i
		let seqnum = fromMaybe 0 $ readish $ T.unpack $ tagValue i
		return $ c seqnum packet
	shasgen c i = do
		let (u:shas) = words $ T.unpack $ tagValue i
		return $ c (toUUID u) (mapMaybe extractSha shas)

decodeExitCode :: Int -> ExitCode
decodeExitCode 0 = ExitSuccess
decodeExitCode n = ExitFailure n
	
encodeExitCode :: ExitCode -> Int
encodeExitCode ExitSuccess = 0
encodeExitCode (ExitFailure n) = n

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

{- Add to a presence to give it a negative priority. -}
negativePriority :: Element
negativePriority = Element "priority" [] [NodeContent $ ContentText "-1"]

pushAttr :: Name
pushAttr = "push"

queryAttr :: Name
queryAttr = "query"

pairAttr :: Name
pairAttr = "pair"

canPushAttr :: Name
canPushAttr = "canpush"

pushRequestAttr :: Name
pushRequestAttr = "pushrequest"

startingPushAttr :: Name
startingPushAttr = "startingpush"

receivePackAttr :: Name
receivePackAttr = "rp"

sendPackAttr :: Name
sendPackAttr = "sp"

receivePackDoneAttr :: Name
receivePackDoneAttr = "rpdone"

shasAttr :: Name
shasAttr = "shas"
