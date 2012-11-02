{- core xmpp support
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.XMPP where

import Common.Annex

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

{- Does a presence contain a gitp-annex tag? -}
isGitAnnexPresence :: Presence -> Bool
isGitAnnexPresence p = any matchingtag (presencePayloads p)
	where
		matchingtag t = elementName t == gitAnnexTagName

{- Name of a git-annex tag, in our own XML namespace.
 - (Not using a namespace URL to avoid unnecessary bloat.) -}
gitAnnexTagName :: Name
gitAnnexTagName  = Name (T.pack "git-annex") (Just $ T.pack "git-annex") Nothing

{- A git-annex tag, to let other clients know we're a git-annex client too. -}
gitAnnexSignature :: Element
gitAnnexSignature = Element gitAnnexTagName [] []

pushAttr :: Name
pushAttr = Name (T.pack "push") Nothing Nothing

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
