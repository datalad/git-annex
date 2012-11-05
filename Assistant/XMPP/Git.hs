{- git over XMPP
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.XMPP.Git where

import Assistant.Common
import Assistant.XMPP
import Assistant.XMPP.Buddies
import Assistant.DaemonStatus
import Assistant.Alert
import Assistant.MakeRemote
import Assistant.Sync
import Annex.UUID
import Config
import qualified Types.Remote as Remote

import Network.Protocol.XMPP
import qualified Data.Text as T

finishXMPPPairing :: JID -> UUID -> Assistant ()
finishXMPPPairing jid u = void $ alertWhile alert $
	makeXMPPGitRemote buddy (baseJID jid) u
  where
	buddy = T.unpack $ buddyName jid
	alert = pairRequestAcknowledgedAlert buddy Nothing

{- A git remote for an XMPP user? This is represented as a git remote
 - that has no location set. The user's XMPP address is stored in the
 - xmppaddress setting.
 -
 - The UUID of their remote is also stored as usual.
 -}
makeXMPPGitRemote :: String -> JID -> UUID -> Assistant Bool
makeXMPPGitRemote buddyname jid u = do
	remote <- liftAnnex $ addRemote $ makeGitRemote buddyname "" -- no location
	liftAnnex $ do
		let r = Remote.repo remote
		storeUUID (remoteConfig r "uuid") u
		setConfig (remoteConfig r "xmppaddress") xmppaddress
	syncNewRemote remote
	return True
  where
	xmppaddress = T.unpack $ formatJID $ baseJID jid


