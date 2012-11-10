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
import qualified Git.Command
import qualified Git.Branch
import qualified Annex.Branch
import Locations.UserConfig
import qualified Types.Remote as Remote
import Utility.FileMode

import Network.Protocol.XMPP
import qualified Data.Text as T
import System.Posix.Env
import System.Posix.Types
import System.Process (std_in, std_out, std_err)
import Control.Concurrent
import qualified Data.ByteString as B
import qualified Data.Map as M

finishXMPPPairing :: JID -> UUID -> Assistant ()
finishXMPPPairing jid u = void $ alertWhile alert $
	makeXMPPGitRemote buddy (baseJID jid) u
  where
	buddy = T.unpack $ buddyName jid
	alert = pairRequestAcknowledgedAlert buddy Nothing

makeXMPPGitRemote :: String -> JID -> UUID -> Assistant Bool
makeXMPPGitRemote buddyname jid u = do
	remote <- liftAnnex $ addRemote $ makeGitRemote buddyname xmppaddress
	liftAnnex $ storeUUID (remoteConfig (Remote.repo remote) "uuid") u
	syncNewRemote remote
	return True
  where
	xmppaddress = "xmpp::" ++ T.unpack (formatJID $ baseJID jid)

{- Pushes the named refs to the remote, over XMPP, communicating with a
 - specific client that either requested the push, or responded to our
 - message.
 -
 - To handle xmpp:: urls, git push will run git-remote-xmpp, which is
 - injected into its PATH, and in turn runs git-annex xmppgit. The
 - dataflow them becomes:
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
xmppPush :: ClientID -> Remote -> [Ref] -> Assistant Bool
xmppPush cid remote refs = runPush (SendPushRunning cid) handleDeferred $ do
	sendNetMessage $ StartingPush cid

	(Fd inf, writepush) <- liftIO createPipe
	(readpush, Fd outf) <- liftIO createPipe
	(Fd controlf, writecontrol) <- liftIO createPipe

	tmp <- liftAnnex $ fromRepo gitAnnexTmpDir
	let tmpdir = tmp </> "xmppgit"
	installwrapper tmpdir

	env <- liftIO getEnvironment
	path <- liftIO getSearchPath
	let myenv = M.fromList
		[ ("PATH", join [searchPathSeparator] $ tmpdir:path)
		, (relayIn, show inf)
		, (relayOut, show outf)
		, (relayControl, show controlf)
		]
		`M.union` M.fromList env

	inh <- liftIO $ fdToHandle readpush
	outh <- liftIO $ fdToHandle writepush
	controlh <- liftIO $ fdToHandle writecontrol
	
	t1 <- forkIO <~> toxmpp inh
	t2 <- forkIO <~> fromxmpp outh controlh

	{- This can take a long time to run, so avoid running it in the
	 - Annex monad. Also, override environment. -}
	g <- liftAnnex gitRepo
	let g' = g { gitEnv = Just $ M.toList myenv }
	let name = Remote.name remote
	let params = Param name : map (Param . show) refs
	ok <- liftIO $ Git.Command.runBool "push" params g'

	liftIO $ mapM_ killThread [t1, t2]
	return ok
  where
	toxmpp inh = forever $ do
		b <- liftIO $ B.hGetSome inh chunkSize
		if B.null b
			then liftIO $ killThread =<< myThreadId
			else sendNetMessage $ SendPackOutput cid b
	fromxmpp outh controlh = forever $ do
		m <- waitNetPushMessage
		case m of
			(ReceivePackOutput _ b) -> liftIO $ do
				B.hPut outh b
				hFlush outh
			(ReceivePackDone _ exitcode) -> liftIO $ do
				hPutStrLn controlh (show exitcode)
				hFlush controlh
			_ -> noop
	installwrapper tmpdir = liftIO $ do
		createDirectoryIfMissing True tmpdir
		let wrapper = tmpdir </> "git-remote-xmpp"
		program <- readProgramFile
		writeFile wrapper $ unlines
			[ "#!/bin/sh"
			, "exec " ++ program ++ " xmppgit"
			]
		modifyFileMode wrapper $ addModes executeModes

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

{- Called by git-annex xmppgit.
 -
 - git-push is talking to us on stdin
 - we're talking to git-push on stdout
 - git-receive-pack is talking to us on relayIn (via XMPP)
 - we're talking to git-receive-pack on relayOut (via XMPP)
 -}
xmppGitRelay :: IO ()
xmppGitRelay = do
	flip relay stdout =<< relayHandle relayIn
	relay stdin =<< relayHandle relayOut

	controlh <- relayHandle relayControl
	s <- hGetLine controlh
	exitWith $ case readish s of
		Just n
			| n == 0 -> ExitSuccess
			| otherwise -> ExitFailure n
		Nothing -> ExitFailure 1
  where
	{- Is it possible to set up pipes and not need to copy the data
	 - ourselves? See splice(2) -}
	relay fromh toh = void $ forkIO $ forever $ do
		b <- B.hGetSome fromh chunkSize
		when (B.null b) $ do
			hClose fromh
			hClose toh
			killThread =<< myThreadId
		B.hPut toh b
		hFlush toh

{- Relays git receive-pack stdin and stdout via XMPP, as well as propigating
 - its exit status to XMPP. -}
xmppReceivePack :: ClientID -> Assistant Bool
xmppReceivePack cid = runPush (ReceivePushRunning cid) handleDeferred $ do
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
		readertid <- forkIO $ reader inh
		void $ feeder outh
		code <- waitForProcess pid
		void $ sendexitcode code
		killThread readertid
		return $ code == ExitSuccess
  where
	toxmpp outh = do
		b <- liftIO $ B.hGetSome outh chunkSize
		if B.null b
			then return () -- EOF
			else do
				sendNetMessage $ ReceivePackOutput cid b
				toxmpp outh
	fromxmpp inh = forever $ do
		m <- waitNetPushMessage
		case m of
			(SendPackOutput _ b) -> liftIO $ do
				B.hPut inh b
				hFlush inh
			_ -> noop

xmppRemotes :: ClientID -> Assistant [Remote]
xmppRemotes cid = case baseJID <$> parseJID cid of
	Nothing -> return []
	Just jid -> do
		rs <- syncRemotes <$> getDaemonStatus
		let want = T.unpack $ formatJID jid
		liftAnnex $ filterM (matching want) rs
  where
	matching want remote = do
		let r = Remote.repo remote
		return $ repoIsUrl r && repoLocation r == "xmpp::" ++ want

whenXMPPRemote :: ClientID -> Assistant () -> Assistant ()
whenXMPPRemote cid = unlessM (null <$> xmppRemotes cid)

handlePushMessage :: NetMessage -> Assistant ()
handlePushMessage (CanPush cid) = whenXMPPRemote cid $ 
	sendNetMessage $ PushRequest cid
handlePushMessage (PushRequest cid) = do
	rs <- xmppRemotes cid
	current <- liftAnnex $ inRepo Git.Branch.current
	--let refs = catMaybes [current, Just Annex.Branch.fullname] -- TODO
	let refs = [Ref "master:refs/xmpp/newmaster"]
	forM_ rs $ \r -> xmppPush cid r refs
handlePushMessage (StartingPush cid) = whenXMPPRemote cid $
	void $ xmppReceivePack cid
handlePushMessage _ = noop

handleDeferred :: NetMessage -> Assistant ()
handleDeferred = handlePushMessage

chunkSize :: Int
chunkSize = 1024
