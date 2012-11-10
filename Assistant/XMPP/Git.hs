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

gitXMPPLocation :: JID -> String
gitXMPPLocation jid = "xmpp::" ++ T.unpack (formatJID $ baseJID jid)

makeXMPPGitRemote :: String -> JID -> UUID -> Assistant Bool
makeXMPPGitRemote buddyname jid u = do
	remote <- liftAnnex $ addRemote $
		makeGitRemote buddyname $ gitXMPPLocation jid
	liftAnnex $ storeUUID (remoteConfig (Remote.repo remote) "uuid") u
	syncNewRemote remote
	return True

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
 - using two environment variables, relayIn and relayOut, that are set
 - to the file descriptors to use. Another, relayControl, is used to
 - propigate the exit status of git receive-pack.
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
	let params = Param (Remote.name remote) : map (Param . show) refs
	r <- liftIO $ Git.Command.runBool "push" params $
		g { gitEnv = Just $ M.toList myenv }

	liftIO $ do
		mapM_ killThread [t1, t2]
		mapM_ hClose [inh, outh, controlh]

	return r
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
				hPrint controlh exitcode
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

type EnvVar = String

envVar :: String -> EnvVar
envVar s = "GIT_ANNEX_XMPPGIT_" ++ s

relayIn :: EnvVar
relayIn = envVar "IN"

relayOut :: EnvVar
relayOut = envVar "OUT"

relayControl :: EnvVar
relayControl = envVar "CONTROL"

relayHandle :: EnvVar -> IO Handle
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
 - git-receive-pack's exit code will be passed to us on relayControl
 -}
xmppGitRelay :: IO ()
xmppGitRelay = do
	flip relay stdout =<< relayHandle relayIn
	relay stdin =<< relayHandle relayOut
	code <- hGetLine =<< relayHandle relayControl
	exitWith $ fromMaybe (ExitFailure 1) $ readish code
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
	repodir <- liftAnnex $ fromRepo repoPath
	let p = (proc "git" ["receive-pack", repodir])
		{ std_in = CreatePipe
		, std_out = CreatePipe
		, std_err = Inherit
		}
	(Just inh, Just outh, _, pid) <- liftIO $ createProcess p
	readertid <- forkIO <~> relayfromxmpp inh
	relaytoxmpp outh
	code <- liftIO $ waitForProcess pid
	void $ sendNetMessage $ ReceivePackDone cid code
	liftIO $ do
		killThread readertid
		hClose inh
		hClose outh
		return $ code == ExitSuccess
  where
	relaytoxmpp outh = do
		b <- liftIO $ B.hGetSome outh chunkSize
		-- empty is EOF, so exit
		unless (B.null b) $ do
			sendNetMessage $ ReceivePackOutput cid b
			relaytoxmpp outh
	relayfromxmpp inh = forever $ do
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
		let loc = gitXMPPLocation jid
		filter (matching loc . Remote.repo) . syncRemotes 
			<$> getDaemonStatus
  where
	matching loc r = repoIsUrl r && repoLocation r == loc

whenXMPPRemote :: ClientID -> Assistant () -> Assistant ()
whenXMPPRemote cid = unlessM (null <$> xmppRemotes cid)

handlePushMessage :: NetMessage -> Assistant ()
handlePushMessage (CanPush cid) = whenXMPPRemote cid $ 
	sendNetMessage $ PushRequest cid
handlePushMessage (PushRequest cid) = do
	rs <- xmppRemotes cid
	current <- liftAnnex $ inRepo Git.Branch.current
	--let refs = catMaybes [current, Just Annex.Branch.fullname] -- TODO
	let refs = [Ref "master:refs/remotes/xmpp/newmaster"]
	forM_ rs $ \r -> xmppPush cid r refs
handlePushMessage (StartingPush cid) = whenXMPPRemote cid $
	void $ xmppReceivePack cid
handlePushMessage _ = noop

handleDeferred :: NetMessage -> Assistant ()
handleDeferred = handlePushMessage

chunkSize :: Int
chunkSize = 4096
