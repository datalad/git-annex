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
import Git.Types
import Locations.UserConfig
import qualified Types.Remote as Remote

import Network.Protocol.XMPP
import qualified Data.Text as T
import System.Posix.Env
import System.Posix.Types
import Control.Concurrent
import qualified Data.ByteString as B

configKey :: Remote -> ConfigKey
configKey r = remoteConfig (Remote.repo r) "xmppaddress"

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
		setConfig (configKey remote) xmppaddress
	syncNewRemote remote
	return True
  where
	xmppaddress = T.unpack $ formatJID $ baseJID jid

{- Pushes the named refs to the remote, over XMPP.
 -
 - Strategy: Set GIT_SSH to run git-annex. By setting the remote url
 - to "xmppgit:dummy", "git-annex xmppgit" will be run locally by
 - "git push". The dataflow them becomes:
 - 
 - git push <--> git-annex xmppgit <--> xmppPush <-------> xmpp
 -                                                          |
 - git receive-pack <--> xmppReceivePack <---------------> xmpp
 - 
 - The pipe between git-annex xmppgit and us is set up and communicated
 - using two file descriptors, GIT_ANNEX_XMPPGIT_IN and
 - GIT_ANNEX_XMPPGIT_OUT. It simply connects those up to its stdin
 - and stdout, respectively, which are in turn connected to "git-push".
 - There is also a GIT_ANNEX_XMPPGIT_CONTROL descriptor, to which an
 - exit status is sent for xmppgit to propigate.
 -
 - We listen at the other end of the pipe and relay to and from XMPP.
 -}
xmppPush :: Remote -> [Ref] -> Assistant Bool
xmppPush _remote _refs = do
	_program <- liftIO readProgramFile
	
	-- GIT_SSH=program git -c remote.xmppremote.url=xmppgit:dummy push xmppremote refs
	error "TODO"

relayIn :: String
relayIn = "GIT_ANNEX_XMPPGIT_IN"

relayOut :: String
relayOut = "GIT_ANNEX_XMPPGIT_OUT"

relayControl :: String
relayControl = "GIT_ANNEX_XMPPGIT_CONTROL"

relayHandle :: String -> IO Handle
relayHandle var = do
	v <- getEnv var
	case readish =<< v of
		Nothing -> error $ var ++ " not set"
		Just n -> fdToHandle $ Fd n

{- Called by git-annex xmppgit. -}
xmppGitRelay :: IO ()
xmppGitRelay = do
	inh <- relayHandle relayIn
	outh <- relayHandle relayOut
	
	hSetBuffering stdout NoBuffering
	hSetBuffering outh NoBuffering

	{- Is it possible to set up pipes and not need to copy the data
	 - ourselves? -}
	void $ forkIO $ forever $ do
		b <- B.hGetSome inh 1024
		when (B.null b) $
			killThread =<< myThreadId
		B.hPut stdout b
	void $ forkIO $ forever $ B.hGetSome stdin 1024 >>= B.hPut outh

	controlh <- relayHandle relayControl
	s <- hGetLine controlh
	exitWith $ case readish s of
		Just n
			| n == 0 -> ExitSuccess
			| otherwise -> ExitFailure n
		Nothing -> ExitFailure 1

{- Relays git receive-pack to and from XMPP, and propigates its exit status. -}
xmppReceivePack :: Assistant Bool
xmppReceivePack = error "TODO"
