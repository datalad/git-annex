{- git over XMPP
 -
 - Copyright 2012 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

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
import qualified Command.Sync
import qualified Annex.Branch
import Annex.Path
import Annex.UUID
import Logs.UUID
import Annex.TaggedPush
import Annex.CatFile
import Config
import Git
import qualified Git.Branch
import qualified Types.Remote as Remote
import qualified Remote as Remote
import Remote.List
import Utility.FileMode
import Utility.Shell
import Utility.Env

import Network.Protocol.XMPP
import qualified Data.Text as T
import System.Posix.Types
import qualified System.Posix.IO
import Control.Concurrent
import System.Timeout
import qualified Data.ByteString as B
import qualified Data.Map as M

{- Largest chunk of data to send in a single XMPP message. -}
chunkSize :: Int
chunkSize = 4096

{- How long to wait for an expected message before assuming the other side
 - has gone away and canceling a push. 
 -
 - This needs to be long enough to allow a message of up to 2+ times
 - chunkSize to propigate up to a XMPP server, perhaps across to another
 - server, and back down to us. On the other hand, other XMPP pushes can be
 - delayed for running until the timeout is reached, so it should not be
 - excessive.
 -}
xmppTimeout :: Int
xmppTimeout = 120000000 -- 120 seconds

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
	liftAnnex $ storeUUIDIn (remoteConfig (Remote.repo remote) "uuid") u
	liftAnnex $ void remoteListRefresh
	remote' <- liftAnnex $ fromMaybe (error "failed to add remote")
		<$> Remote.byName (Just buddyname)
	syncRemote remote'
	return True

{- Pushes over XMPP, communicating with a specific client.
 - Runs an arbitrary IO action to push, which should run git-push with
 - an xmpp:: url.
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
xmppPush :: ClientID -> (Git.Repo -> IO Bool) -> Assistant Bool
xmppPush cid gitpush = do
	u <- liftAnnex getUUID
	sendNetMessage $ Pushing cid (StartingPush u)

	(Fd inf, writepush) <- liftIO System.Posix.IO.createPipe
	(readpush, Fd outf) <- liftIO System.Posix.IO.createPipe
	(Fd controlf, writecontrol) <- liftIO System.Posix.IO.createPipe

	tmpdir <- gettmpdir
	installwrapper tmpdir

	environ <- liftIO getEnvironment
	path <- liftIO getSearchPath
	let myenviron = addEntries
		[ ("PATH", intercalate [searchPathSeparator] $ tmpdir:path)
		, (relayIn, show inf)
		, (relayOut, show outf)
		, (relayControl, show controlf)
		]
		environ

	inh <- liftIO $ fdToHandle readpush
	outh <- liftIO $ fdToHandle writepush
	controlh <- liftIO $ fdToHandle writecontrol
	
	t1 <- forkIO <~> toxmpp 0 inh
	t2 <- forkIO <~> fromxmpp outh controlh

	{- This can take a long time to run, so avoid running it in the
	 - Annex monad. Also, override environment. -}
	g <- liftAnnex gitRepo
	r <- liftIO $ gitpush $ g { gitEnv = Just myenviron }

	liftIO $ do
		mapM_ killThread [t1, t2]
		mapM_ hClose [inh, outh, controlh]
		mapM_ closeFd [Fd inf, Fd outf, Fd controlf]
	
	return r
  where
	toxmpp seqnum inh = do
		b <- liftIO $ B.hGetSome inh chunkSize
		if B.null b
			then liftIO $ killThread =<< myThreadId
			else do
				let seqnum' = succ seqnum
				sendNetMessage $ Pushing cid $
					SendPackOutput seqnum' b
				toxmpp seqnum' inh
	
	fromxmpp outh controlh = withPushMessagesInSequence cid SendPack handlemsg
	  where
		handlemsg (Just (Pushing _ (ReceivePackOutput _ b))) = 
			liftIO $ writeChunk outh b
		handlemsg (Just (Pushing _ (ReceivePackDone exitcode))) =
			liftIO $ do
				hPrint controlh exitcode
				hFlush controlh
		handlemsg (Just _) = noop
		handlemsg Nothing = do
			debug ["timeout waiting for git receive-pack output via XMPP"]
			-- Send a synthetic exit code to git-annex
			-- xmppgit, which will exit and cause git push
			-- to die.
			liftIO $ do
				hPrint controlh (ExitFailure 1)
				hFlush controlh
				killThread =<< myThreadId
	
	installwrapper tmpdir = liftIO $ do
		createDirectoryIfMissing True tmpdir
		let wrapper = tmpdir </> "git-remote-xmpp"
		program <- programPath
		writeFile wrapper $ unlines
			[ shebang_local
			, "exec " ++ program ++ " xmppgit"
			]
		modifyFileMode wrapper $ addModes executeModes
	
	{- Use GIT_ANNEX_TMP_DIR if set, since that may be a better temp
	 - dir (ie, not on a crippled filesystem where we can't make
	 - the wrapper executable). -}
	gettmpdir = do
		v <- liftIO $ getEnv "GIT_ANNEX_TMP_DIR"
		case v of
			Nothing -> do
				tmp <- liftAnnex $ fromRepo gitAnnexTmpMiscDir
				return $ tmp </> "xmppgit"
			Just d -> return $ d </> "xmppgit"

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
		writeChunk toh b

{- Relays git receive-pack stdin and stdout via XMPP, as well as propigating
 - its exit status to XMPP. -}
xmppReceivePack :: ClientID -> Assistant Bool
xmppReceivePack cid = do
	repodir <- liftAnnex $ fromRepo repoPath
	let p = (proc "git" ["receive-pack", repodir])
		{ std_in = CreatePipe
		, std_out = CreatePipe
		, std_err = Inherit
		}
	(Just inh, Just outh, _, pid) <- liftIO $ createProcess p
	readertid <- forkIO <~> relayfromxmpp inh
	relaytoxmpp 0 outh
	code <- liftIO $ waitForProcess pid
	void $ sendNetMessage $ Pushing cid $ ReceivePackDone code
	liftIO $ do
		killThread readertid
		hClose inh
		hClose outh
		return $ code == ExitSuccess
  where
	relaytoxmpp seqnum outh = do
		b <- liftIO $ B.hGetSome outh chunkSize
		-- empty is EOF, so exit
		unless (B.null b) $ do
			let seqnum' = succ seqnum
			sendNetMessage $ Pushing cid $ ReceivePackOutput seqnum' b
			relaytoxmpp seqnum' outh
	relayfromxmpp inh = withPushMessagesInSequence cid ReceivePack handlemsg
	  where
		handlemsg (Just (Pushing _ (SendPackOutput _ b))) =
			liftIO $ writeChunk inh b
		handlemsg (Just _) = noop
		handlemsg Nothing = do
			debug ["timeout waiting for git send-pack output via XMPP"]
			-- closing the handle will make git receive-pack exit
			liftIO $ do
				hClose inh
				killThread =<< myThreadId

xmppRemotes :: ClientID -> UUID -> Assistant [Remote]
xmppRemotes cid theiruuid = case baseJID <$> parseJID cid of
	Nothing -> return []
	Just jid -> do
		let loc = gitXMPPLocation jid
		um <- liftAnnex uuidMap
		filter (matching loc . Remote.repo) . filter (knownuuid um) . syncGitRemotes 
			<$> getDaemonStatus
  where
	matching loc r = repoIsUrl r && repoLocation r == loc
	knownuuid um r = Remote.uuid r == theiruuid || M.member theiruuid um

{- Returns the ClientID that it pushed to. -}
runPush :: (Remote -> Assistant ()) -> NetMessage -> Assistant (Maybe ClientID)
runPush checkcloudrepos (Pushing cid (PushRequest theiruuid)) =
	go =<< liftAnnex (join Command.Sync.getCurrBranch)
  where
	go (Just branch, _) = do
		rs <- xmppRemotes cid theiruuid
		liftAnnex $ Annex.Branch.commit "update"
		(g, u) <- liftAnnex $ (,)
			<$> gitRepo
			<*> getUUID
		liftIO $ Command.Sync.updateBranch (Command.Sync.syncBranch branch) g
		selfjid <- ((T.unpack <$>) . xmppClientID) <$> getDaemonStatus
		if null rs
			then return Nothing
			else do
				forM_ rs $ \r -> do
					void $ alertWhile (syncAlert [r]) $
						xmppPush cid (taggedPush u selfjid branch r)
					checkcloudrepos r
				return $ Just cid
	go _ = return Nothing
runPush checkcloudrepos (Pushing cid (StartingPush theiruuid)) = do
	rs <- xmppRemotes cid theiruuid
	if null rs
		then return Nothing
		else do
			void $ alertWhile (syncAlert rs) $
				xmppReceivePack cid
			mapM_ checkcloudrepos rs
			return $ Just cid
runPush _ _ = return Nothing

{- Check if any of the shas that can be pushed are ones we do not
 - have.
 -
 - (Older clients send no shas, so when there are none, always
 - request a push.)
 -}
handlePushNotice :: NetMessage -> Assistant ()
handlePushNotice (Pushing cid (CanPush theiruuid shas)) =
	unlessM (null <$> xmppRemotes cid theiruuid) $
		if null shas
			then go
			else ifM (haveall shas)
				( debug ["ignoring CanPush with known shas"]
				, go
				)
  where
	go = do
		u <- liftAnnex getUUID
		sendNetMessage $ Pushing cid (PushRequest u)
	haveall l = liftAnnex $ not <$> anyM donthave l
	donthave sha = isNothing <$> catObjectDetails sha
handlePushNotice _ = noop

writeChunk :: Handle -> B.ByteString -> IO ()
writeChunk h b = do
	B.hPut h b
	hFlush h

{- Gets NetMessages for a PushSide, ensures they are in order,
 - and runs an action to handle each in turn. The action will be passed
 - Nothing on timeout.
 -
 - Does not currently reorder messages, but does ensure that any 
 - duplicate messages, or messages not in the sequence, are discarded.
 -}
withPushMessagesInSequence :: ClientID -> PushSide -> (Maybe NetMessage -> Assistant ()) -> Assistant ()
withPushMessagesInSequence cid side a = loop 0
  where
	loop seqnum = do
		m <- timeout xmppTimeout <~> waitInbox cid side
		let go s = a m >> loop s
		let next = seqnum + 1
		case extractSequence =<< m of
			Just seqnum'
				| seqnum' == next -> go next
				| seqnum' == 0 -> go seqnum
				| seqnum' == seqnum -> do
					debug ["ignoring duplicate sequence number", show seqnum]
					loop seqnum
				| otherwise -> do
					debug ["ignoring out of order sequence number", show seqnum', "expected", show next]
					loop seqnum
			Nothing -> go seqnum

extractSequence :: NetMessage -> Maybe Int
extractSequence (Pushing _ (ReceivePackOutput seqnum _)) = Just seqnum
extractSequence (Pushing _ (SendPackOutput seqnum _)) = Just seqnum
extractSequence _ = Nothing
