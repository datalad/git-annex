{- git over XMPP
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.XMPP.Git where

import Assistant.Common
import Assistant.NetMessager
import Assistant.Types.NetMessager
import Assistant.XMPP
import Assistant.XMPP.Buddies
import Assistant.DaemonStatus
import Assistant.Alert
import Assistant.MakeRemote
import Assistant.Sync
import Annex.UUID
import Config
import Git
import Git.Command
import Locations.UserConfig
import qualified Types.Remote as Remote

import Network.Protocol.XMPP
import qualified Data.Text as T
import System.Posix.Env
import System.Posix.Types
import System.Process (std_in, std_out, std_err)
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
xmppPush remote refs = error "TODO"

xmppPush' :: ClientID -> Remote -> [Ref] -> Assistant Bool
xmppPush' cid remote refs = do
	program <- liftIO readProgramFile

	(Fd inf, writepush) <- liftIO createPipe
	(readpush, Fd outf) <- liftIO createPipe
	(Fd controlf, writecontrol) <- liftIO createPipe

	env <- liftIO getEnvironment
	let myenv =
		[ ("GIT_SSH", program)
		, (relayIn, show inf)
		, (relayOut, show outf)
		, (relayControl, show controlf)
		]
	g <- liftAnnex gitRepo
	let name = Remote.name remote
	let mainparams = [Param "-c", Param $ "remote."++name++".url=xmpp:client"]
	let params = Param "push" : Param name : map (Param . show) refs

	inh <- liftIO $ fdToHandle readpush
	outh <- liftIO $ fdToHandle writepush
	controlh <- liftIO $ fdToHandle writecontrol
	liftIO $ hSetBuffering outh NoBuffering
	
	t1 <- forkIO <~> toxmpp inh
	t2 <- forkIO <~> fromxmpp outh
	t3 <- forkIO <~> controlxmpp controlh

	ok <- liftIO $ boolSystemEnv "git"
		(mainparams ++ gitCommandLine params g)
		(Just $ env ++ myenv)
	liftIO $ mapM_ killThread [t1, t2, t3]
	return ok
  where
	toxmpp inh = forever $ do
		b <- liftIO $ B.hGetSome inh 1024
		when (B.null b) $
			liftIO $ killThread =<< myThreadId
		sendNetMessage $ SendPackOutput cid b
		error "TODO"
	fromxmpp outh = forever $ do
		-- TODO get b from xmpp
		let b = undefined
		liftIO $ B.hPut outh b
	controlxmpp controlh = do
		-- TODO wait for control message from xmpp
		let exitcode = undefined :: Int
		liftIO $ hPutStrLn controlh (show exitcode)
		

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
	
	hSetBuffering outh NoBuffering

	{- Is it possible to set up pipes and not need to copy the data
	 - ourselves? See splice(2) -}
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

{- Relays git receive-pack stdin and stdout via XMPP, as well as propigating
 - its exit status to XMPP. -}
xmppReceivePack :: ClientID -> Assistant Bool
xmppReceivePack cid = do
	feeder <- asIO1 toxmpp
	reader <- asIO1 fromxmpp
	sendexitcode <- asIO1 $ sendNetMessage . ReceivePackDone cid
	repodir <- liftAnnex $ fromRepo repoPath
	let p = (proc "git" ["receive-pack", repodir])
		{ std_in = CreatePipe
		, std_out = CreatePipe
		, std_err = Inherit
		}
	liftIO $ do
		(Just inh, Just outh, _, pid) <- createProcess p
		feedertid <- forkIO $ feeder outh
		void $ reader inh
		code <- waitForProcess pid
		void $ sendexitcode code
		killThread feedertid
		return $ code == ExitSuccess
  where
	toxmpp outh = do
		b <- liftIO $ B.hGetSome outh 1024
		if B.null b
			then return () -- EOF
			else do
				sendNetMessage $ ReceivePackOutput cid b
				toxmpp outh
	fromxmpp _inh = error "TODO feed xmpp to inh"
