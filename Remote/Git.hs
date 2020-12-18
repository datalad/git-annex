{- Standard git remotes.
 -
 - Copyright 2011-2020 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Remote.Git (
	remote,
	configRead,
	repoAvail,
	onLocalRepo,
) where

import Annex.Common
import Annex.Ssh
import Types.Remote
import Types.GitConfig
import qualified Git
import qualified Git.Config
import qualified Git.Construct
import qualified Git.Command
import qualified Git.GCrypt
import qualified Git.Types as Git
import qualified Annex
import Logs.Presence
import Annex.Transfer
import Annex.UUID
import qualified Annex.Content
import qualified Annex.BranchState
import qualified Annex.Branch
import qualified Annex.Url as Url
import qualified Annex.SpecialRemote.Config as SpecialRemote
import Utility.Tmp
import Config
import Config.Cost
import Annex.SpecialRemote.Config
import Config.DynamicConfig
import Annex.Init
import Types.CleanupActions
import qualified CmdLine.GitAnnexShell.Fields as Fields
import Logs.Location
import Utility.Metered
import Utility.CopyFile
import Utility.Env
import Utility.Batch
import Utility.SimpleProtocol
import Remote.Helper.Git
import Remote.Helper.Messages
import Remote.Helper.ExportImport
import qualified Remote.Helper.Ssh as Ssh
import qualified Remote.GCrypt
import qualified Remote.GitLFS
import qualified Remote.P2P
import qualified Remote.Helper.P2P as P2PHelper
import P2P.Address
import Annex.Path
import Creds
import Types.NumCopies
import Types.ProposedAccepted
import Annex.Action
import Messages.Progress

#ifndef mingw32_HOST_OS
import qualified Utility.RawFilePath as R
import Utility.FileMode
#endif

import Control.Concurrent
import Control.Concurrent.MSampleVar
import qualified Data.Map as M
import qualified Data.ByteString as S
import Network.URI

remote :: RemoteType
remote = RemoteType
	{ typename = "git"
	, enumerate = list
	, generate = gen
	, configParser = mkRemoteConfigParser
		[ optionalStringParser locationField
			(FieldDesc "url of git remote to remember with special remote")
		]
	, setup = gitSetup
	, exportSupported = exportUnsupported
	, importSupported = importUnsupported
	, thirdPartyPopulated = False
	}

locationField :: RemoteConfigField
locationField = Accepted "location"

list :: Bool -> Annex [Git.Repo]
list autoinit = do
	c <- fromRepo Git.config
	rs <- mapM (tweakurl c) =<< Annex.getGitRemotes
	mapM (configRead autoinit) rs
  where
	annexurl r = remoteConfig r "annexurl"
	tweakurl c r = do
		let n = fromJust $ Git.remoteName r
		case M.lookup (annexurl r) c of
			Nothing -> return r
			Just url -> inRepo $ \g ->
				Git.Construct.remoteNamed n $
					Git.Construct.fromRemoteLocation (Git.fromConfigValue url) g

{- Git remotes are normally set up using standard git command, not
 - git-annex initremote and enableremote.
 -
 - For initremote, the git remote must already be set up, and have a uuid.
 - Initremote simply remembers its location.
 -
 - enableremote simply sets up a git remote using the stored location.
 - No attempt is made to make the remote be accessible via ssh key setup,
 - etc.
 -}
gitSetup :: SetupStage -> Maybe UUID -> Maybe CredPair -> RemoteConfig -> RemoteGitConfig -> Annex (RemoteConfig, UUID)
gitSetup Init mu _ c _ = do
	let location = fromMaybe (giveup "Specify location=url") $
		Url.parseURIRelaxed . fromProposedAccepted
			=<< M.lookup locationField c
	rs <- Annex.getGitRemotes
	u <- case filter (\r -> Git.location r == Git.Url location) rs of
		[r] -> getRepoUUID r
		[] -> giveup "could not find existing git remote with specified location"
		_ -> giveup "found multiple git remotes with specified location"
	if isNothing mu || mu == Just u
		then return (c, u)
		else error "git remote did not have specified uuid"
gitSetup (Enable _) (Just u) _ c _ = do
	inRepo $ Git.Command.run
		[ Param "remote"
		, Param "add"
		, Param $ fromMaybe (giveup "no name") (SpecialRemote.lookupName c)
		, Param $ maybe (giveup "no location") fromProposedAccepted (M.lookup locationField c)
		]
	return (c, u)
gitSetup (Enable _) Nothing _ _ _ = error "unable to enable git remote with no specified uuid"

{- It's assumed to be cheap to read the config of non-URL remotes, so this is
 - done each time git-annex is run in a way that uses remotes, unless
 - annex-checkuuid is false.
 -
 - Conversely, the config of an URL remote is only read when there is no
 - cached UUID value. -}
configRead :: Bool -> Git.Repo -> Annex Git.Repo
configRead autoinit r = do
	gc <- Annex.getRemoteGitConfig r
	hasuuid <- (/= NoUUID) <$> getRepoUUID r
	annexignore <- liftIO $ getDynamicConfig (remoteAnnexIgnore gc)
	case (repoCheap r, annexignore, hasuuid) of
		(_, True, _) -> return r
		(True, _, _)
			| remoteAnnexCheckUUID gc -> tryGitConfigRead autoinit r hasuuid
			| otherwise -> return r
		(False, _, False) -> configSpecialGitRemotes r >>= \case
			Nothing -> tryGitConfigRead autoinit r False
			Just r' -> return r'
		_ -> return r

gen :: Git.Repo -> UUID -> RemoteConfig -> RemoteGitConfig -> RemoteStateHandle -> Annex (Maybe Remote)
gen r u rc gc rs
	-- Remote.GitLFS may be used with a repo that is also encrypted
	-- with gcrypt so is checked first.
	| remoteAnnexGitLFS gc = Remote.GitLFS.gen r u rc gc rs
	| Git.GCrypt.isEncrypted r = Remote.GCrypt.chainGen r u rc gc rs
	| otherwise = case repoP2PAddress r of
		Nothing -> do
			st <- mkState r u gc
			c <- parsedRemoteConfig remote rc
			go st c <$> remoteCost gc defcst
		Just addr -> Remote.P2P.chainGen addr r u rc gc rs
  where
	defcst = if repoCheap r then cheapRemoteCost else expensiveRemoteCost
	go st c cst = Just new
	  where
		new = Remote 
			{ uuid = u
			, cost = cst
			, name = Git.repoDescribe r
			, storeKey = copyToRemote new st
			, retrieveKeyFile = copyFromRemote new st
			, retrieveKeyFileCheap = copyFromRemoteCheap new st r
			, retrievalSecurityPolicy = RetrievalAllKeysSecure
			, removeKey = dropKey new st
			, lockContent = Just (lockKey new st)
			, checkPresent = inAnnex new st
			, checkPresentCheap = repoCheap r
			, exportActions = exportUnsupported
			, importActions = importUnsupported
			, whereisKey = Nothing
			, remoteFsck = if Git.repoIsUrl r
				then Nothing
				else Just $ fsckOnRemote r
			, repairRepo = if Git.repoIsUrl r
				then Nothing
				else Just $ repairRemote r
			, config = c
			, localpath = localpathCalc r
			, getRepo = getRepoFromState st
			, gitconfig = gc
			, readonly = Git.repoIsHttp r
			, appendonly = False
			, availability = availabilityCalc r
			, remotetype = remote
			, mkUnavailable = unavailable r u rc gc rs
			, getInfo = gitRepoInfo new
			, claimUrl = Nothing
			, checkUrl = Nothing
			, remoteStateHandle = rs
			}

unavailable :: Git.Repo -> UUID -> RemoteConfig -> RemoteGitConfig -> RemoteStateHandle -> Annex (Maybe Remote)
unavailable r = gen r'
  where
	r' = case Git.location r of
		Git.Local { Git.gitdir = d } ->
			r { Git.location = Git.LocalUnknown d }
		Git.Url url -> case uriAuthority url of
			Just auth -> 
				let auth' = auth { uriRegName = "!dne!" }
				in r { Git.location = Git.Url (url { uriAuthority = Just auth' })}
			Nothing -> r { Git.location = Git.Unknown }
		_ -> r -- already unavailable

{- Checks relatively inexpensively if a repository is available for use. -}
repoAvail :: Git.Repo -> Annex Bool
repoAvail r 
	| Git.repoIsHttp r = return True
	| Git.GCrypt.isEncrypted r = do
		g <- gitRepo
		liftIO $ do
			er <- Git.GCrypt.encryptedRemote g r
			if Git.repoIsLocal er || Git.repoIsLocalUnknown er
				then catchBoolIO $
					void (Git.Config.read er) >> return True
				else return True
	| Git.repoIsUrl r = return True
	| Git.repoIsLocalUnknown r = return False
	| otherwise = liftIO $ isJust <$> catchMaybeIO (Git.Config.read r)

{- Tries to read the config for a specified remote, updates state, and
 - returns the updated repo. -}
tryGitConfigRead :: Bool -> Git.Repo -> Bool -> Annex Git.Repo
tryGitConfigRead autoinit r hasuuid
	| haveconfig r = return r -- already read
	| Git.repoIsSsh r = storeUpdatedRemote $ do
		v <- Ssh.onRemote NoConsumeStdin r
			(pipedconfig Git.Config.ConfigList autoinit (Git.repoDescribe r), return (Left $ giveup "configlist failed"))
			"configlist" [] configlistfields
		case v of
			Right r'
				| haveconfig r' -> return r'
				| otherwise -> configlist_failed
			Left _ -> configlist_failed
	| Git.repoIsHttp r = storeUpdatedRemote geturlconfig
	| Git.GCrypt.isEncrypted r = handlegcrypt =<< getConfigMaybe (remoteAnnexConfig r "uuid")
	| Git.repoIsUrl r = do
		set_ignore "uses a protocol not supported by git-annex" False
		return r
	| otherwise = storeUpdatedRemote $
		liftIO readlocalannexconfig
			`catchNonAsync` const failedreadlocalconfig
  where
	haveconfig = not . M.null . Git.config

	pipedconfig st mustincludeuuuid configloc cmd params = do
		v <- liftIO $ Git.Config.fromPipe r cmd params st
		case v of
			Right (r', val, _err) -> do
				unless (isUUIDConfigured r' || S.null val || not mustincludeuuuid) $ do
					warning $ "Failed to get annex.uuid configuration of repository " ++ Git.repoDescribe r
					warning $ "Instead, got: " ++ show val
					warning $ "This is unexpected; please check the network transport!"
				return $ Right r'
			Left l -> do
				warning $ "Unable to parse git config from " ++ configloc
				return $ Left l

	geturlconfig = Url.withUrlOptionsPromptingCreds $ \uo -> do
		v <- withTmpFile "git-annex.tmp" $ \tmpfile h -> do
			liftIO $ hClose h
			let url = Git.repoLocation r ++ "/config"
			ifM (liftIO $ Url.downloadQuiet nullMeterUpdate url tmpfile uo)
				( Just <$> pipedconfig Git.Config.ConfigNullList False url "git" [Param "config", Param "--null", Param "--list", Param "--file", File tmpfile]
				, return Nothing
				)
		case v of
			Just (Right r') -> do
				-- Cache when http remote is not bare for
				-- optimisation.
				unless (Git.Config.isBare r') $
					setremote setRemoteBare False
				return r'
			_ -> do
				set_ignore "not usable by git-annex" False
				return r

	{- Is this remote just not available, or does
	 - it not have git-annex-shell?
	 - Find out by trying to fetch from the remote. -}
	configlist_failed = case Git.remoteName r of
		Nothing -> return r
		Just n -> do
			whenM (inRepo $ Git.Command.runBool [Param "fetch", Param "--quiet", Param n]) $ do
				set_ignore "does not have git-annex installed" True
			return r
	
	set_ignore msg longmessage = do
		case Git.remoteName r of
			Nothing -> noop
			Just n -> do
				warning $ "Remote " ++ n ++ " " ++ msg ++ "; setting annex-ignore"
				when longmessage $
					warning $ "This could be a problem with the git-annex installation on the remote. Please make sure that git-annex-shell is available in PATH when you ssh into the remote. Once you have fixed the git-annex installation, run: git annex enableremote " ++ n
		setremote setRemoteIgnore True
	
	setremote setter v = case Git.remoteName r of
		Nothing -> noop
		Just _ -> setter r v
	
	handlegcrypt Nothing = return r
	handlegcrypt (Just _cacheduuid) = do
		-- Generate UUID from the gcrypt-id
		g <- gitRepo
		case Git.GCrypt.remoteRepoId g (Git.remoteName r) of
			Nothing -> return r
			Just v -> storeUpdatedRemote $ liftIO $ setUUID r $
				genUUIDInNameSpace gCryptNameSpace v

	{- The local repo may not yet be initialized, so try to initialize
	 - it if allowed. However, if that fails, still return the read
	 - git config. -}
	readlocalannexconfig = do
		let check = do
			Annex.BranchState.disableUpdate
			catchNonAsync autoInitialize $ \e ->
				warning $ "remote " ++ Git.repoDescribe r ++
					":"  ++ show e
			Annex.getState Annex.repo
		s <- Annex.new r
		Annex.eval s $ check `finally` stopCoProcesses
		
	failedreadlocalconfig = do
		unless hasuuid $ case Git.remoteName r of
			Nothing -> noop
			Just n -> do
				warning $ "Remote " ++ n ++ " cannot currently be accessed."
		return r
		
	configlistfields = if autoinit
		then [(Fields.autoInit, "1")]
		else []

{- Handles special remotes that can be enabled by the presence of
 - regular git remotes.
 -
 - When a remote repo is found to be such a special remote, its
 - UUID is cached in the git config, and the repo returned with
 - the UUID set.
 -}
configSpecialGitRemotes :: Git.Repo -> Annex (Maybe Git.Repo)
configSpecialGitRemotes r = Remote.GitLFS.configKnownUrl r >>= \case
	Nothing -> return Nothing
	Just r' -> Just <$> storeUpdatedRemote (return r')

storeUpdatedRemote :: Annex Git.Repo -> Annex Git.Repo
storeUpdatedRemote = observe $ \r' -> do
	l <- Annex.getGitRemotes
	let rs = exchange l r'
	Annex.changeState $ \s -> s { Annex.gitremotes = Just rs }
  where
	exchange [] _ = []
	exchange (old:ls) new
		| Git.remoteName old == Git.remoteName new =
			new : exchange ls new
		| otherwise =
			old : exchange ls new

{- Checks if a given remote has the content for a key in its annex. -}
inAnnex :: Remote -> State -> Key -> Annex Bool
inAnnex rmt st key = do
	repo <- getRepo rmt
	inAnnex' repo rmt st key

inAnnex' :: Git.Repo -> Remote -> State -> Key -> Annex Bool
inAnnex' repo rmt st@(State connpool duc _ _ _) key
	| Git.repoIsHttp repo = checkhttp
	| Git.repoIsUrl repo = checkremote
	| otherwise = checklocal
  where
	checkhttp = do
		showChecking repo
		gc <- Annex.getGitConfig
		ifM (Url.withUrlOptionsPromptingCreds $ \uo -> anyM (\u -> Url.checkBoth u (fromKey keySize key) uo) (keyUrls gc repo rmt key))
			( return True
			, giveup "not found"
			)
	checkremote = 
		let fallback = Ssh.inAnnex repo key
		in P2PHelper.checkpresent (Ssh.runProto rmt connpool (cantCheck rmt) fallback) key
	checklocal = ifM duc
		( guardUsable repo (cantCheck repo) $
			maybe (cantCheck repo) return
				=<< onLocalFast st (Annex.Content.inAnnexSafe key)
		, cantCheck repo
		)

keyUrls :: GitConfig -> Git.Repo -> Remote -> Key -> [String]
keyUrls gc repo r key = map tourl locs'
  where
	tourl l = Git.repoLocation repo ++ "/" ++ l
	-- If the remote is known to not be bare, try the hash locations
	-- used for non-bare repos first, as an optimisation.
	locs
		| remoteAnnexBare remoteconfig == Just False = reverse (annexLocations gc key)
		| otherwise = annexLocations gc key
#ifndef mingw32_HOST_OS
	locs' = map fromRawFilePath locs
#else
	locs' = map (replace "\\" "/" . fromRawFilePath) locs
#endif
	remoteconfig = gitconfig r

dropKey :: Remote -> State -> Key -> Annex ()
dropKey r st key = do
	repo <- getRepo r
	dropKey' repo r st key

dropKey' :: Git.Repo -> Remote -> State -> Key -> Annex ()
dropKey' repo r st@(State connpool duc _ _ _) key
	| not $ Git.repoIsUrl repo = ifM duc
		( guardUsable repo (giveup "cannot access remote") $
			commitOnCleanup repo r st $ onLocalFast st $ do
				whenM (Annex.Content.inAnnex key) $ do
					let cleanup = logStatus key InfoMissing
					Annex.Content.lockContentForRemoval key cleanup $ \lock -> do
						Annex.Content.removeAnnex lock
						cleanup
					Annex.Content.saveState True
		, giveup "remote does not have expected annex.uuid value"
		)
	| Git.repoIsHttp repo = giveup "dropping from http remote not supported"
	| otherwise = commitOnCleanup repo r st $ do
		let fallback = Ssh.dropKey' repo key
		P2PHelper.remove (Ssh.runProto r connpool (return False) fallback) key

lockKey :: Remote -> State -> Key -> (VerifiedCopy -> Annex r) -> Annex r
lockKey r st key callback = do
	repo <- getRepo r
	lockKey' repo r st key callback

lockKey' :: Git.Repo -> Remote -> State -> Key -> (VerifiedCopy -> Annex r) -> Annex r
lockKey' repo r st@(State connpool duc _ _ _) key callback
	| not $ Git.repoIsUrl repo = ifM duc
		( guardUsable repo failedlock $ do
			inorigrepo <- Annex.makeRunner
			-- Lock content from perspective of remote,
			-- and then run the callback in the original
			-- annex monad, not the remote's.
			onLocalFast st $ 
				Annex.Content.lockContentShared key $
					liftIO . inorigrepo . callback
		, failedlock
		)
	| Git.repoIsSsh repo = do
		showLocking r
		let withconn = Ssh.withP2PSshConnection r connpool fallback
		P2PHelper.lock withconn Ssh.runProtoConn (uuid r) key callback
	| otherwise = failedlock
  where
	fallback = withNullHandle $ \nullh -> do
		Just (cmd, params) <- Ssh.git_annex_shell ConsumeStdin
			repo "lockcontent"
			[Param $ serializeKey key] []
		let p = (proc cmd (toCommand params))
			{ std_in = CreatePipe
			, std_out = CreatePipe
			, std_err = UseHandle nullh
			}
		bracketIO (createProcess p) cleanupProcess fallback'

	fallback' (Just hin, Just hout, Nothing, p) = do
		v <- liftIO $ tryIO $ getProtocolLine hout
		let signaldone = void $ tryNonAsync $ liftIO $ mapM_ tryNonAsync
			[ hPutStrLn hout ""
			, hFlush hout
			, hClose hin
			, hClose hout
			, void $ waitForProcess p
			]
		let checkexited = not . isJust <$> getProcessExitCode p
		case v of
			Left _exited -> do
				showNote "lockcontent failed"
				liftIO $ do
					hClose hin
					hClose hout
					void $ waitForProcess p
				failedlock
			Right l 
				| l == Just Ssh.contentLockedMarker -> bracket_
					noop
					signaldone 
					(withVerifiedCopy LockedCopy r checkexited callback)
				| otherwise -> do
					showNote "lockcontent failed"
					signaldone
					failedlock
	fallback' _ = error "internal"

	failedlock = giveup "can't lock content"

{- Tries to copy a key's content from a remote's annex to a file. -}
copyFromRemote :: Remote -> State -> Key -> AssociatedFile -> FilePath -> MeterUpdate -> Annex Verification
copyFromRemote = copyFromRemote' False

copyFromRemote' :: Bool -> Remote -> State -> Key -> AssociatedFile -> FilePath -> MeterUpdate -> Annex Verification
copyFromRemote' forcersync r st key file dest meterupdate = do
	repo <- getRepo r
	copyFromRemote'' repo forcersync r st key file dest meterupdate

copyFromRemote'' :: Git.Repo -> Bool -> Remote -> State -> Key -> AssociatedFile -> FilePath -> MeterUpdate -> Annex Verification
copyFromRemote'' repo forcersync r st@(State connpool _ _ _ _) key file dest meterupdate
	| Git.repoIsHttp repo = do
		gc <- Annex.getGitConfig
		ok <- Url.withUrlOptionsPromptingCreds $
			Annex.Content.downloadUrl key meterupdate (keyUrls gc repo r key) dest
		unless ok $
			giveup "failed to download content"
		return UnVerified
	| not $ Git.repoIsUrl repo = guardUsable repo (giveup "cannot access remote") $ do
		params <- Ssh.rsyncParams r Download
		u <- getUUID
		hardlink <- wantHardLink
		-- run copy from perspective of remote
		onLocalFast st $ Annex.Content.prepSendAnnex key >>= \case
			Just (object, checksuccess) -> do
				copier <- mkCopier hardlink st params
				(ok, v) <- runTransfer (Transfer Download u (fromKey id key))
					file stdRetry $ \p ->
						metered (Just (combineMeterUpdate p meterupdate)) key $ \_ p' -> 
							copier object dest p' checksuccess
				if ok
					then return v
					else giveup "failed to retrieve content from remote"
			Nothing -> giveup "content is not present in remote"
	| Git.repoIsSsh repo = if forcersync
		then do
			(ok, v) <- fallback meterupdate
			if ok
				then return v
				else giveup "failed to retrieve content from remote"
		else P2PHelper.retrieve
			(\p -> Ssh.runProto r connpool (return (False, UnVerified)) (fallback p))
			key file dest meterupdate
	| otherwise = giveup "copying from non-ssh, non-http remote not supported"
  where
	fallback p = unVerified $ feedprogressback $ \p' -> do
		oh <- mkOutputHandlerQuiet
		Ssh.rsyncHelper oh (Just (combineMeterUpdate p' p))
			=<< Ssh.rsyncParamsRemote False r Download key dest file

	{- Feed local rsync's progress info back to the remote,
	 - by forking a feeder thread that runs
	 - git-annex-shell transferinfo at the same time
	 - git-annex-shell sendkey is running.
	 -
	 - To avoid extra password prompts, this is only done when ssh
	 - connection caching is supported.
	 - Note that it actually waits for rsync to indicate
	 - progress before starting transferinfo, in order
	 - to ensure ssh connection caching works and reuses 
	 - the connection set up for the sendkey.
	 -
	 - Also note that older git-annex-shell does not support
	 - transferinfo, so stderr is dropped and failure ignored.
	 -}
	feedprogressback a = ifM (isJust <$> sshCacheDir)
		( feedprogressback' a
		, a $ const noop
		)
	feedprogressback' a = do
		u <- getUUID
		let AssociatedFile afile = file
		let fields = (Fields.remoteUUID, fromUUID u)
			: maybe [] (\f -> [(Fields.associatedFile, fromRawFilePath f)]) afile
		Just (cmd, params) <- Ssh.git_annex_shell ConsumeStdin
			repo "transferinfo" 
			[Param $ serializeKey key] fields
		v <- liftIO (newEmptySV :: IO (MSampleVar Integer))
		pv <- liftIO $ newEmptyMVar
		tid <- liftIO $ forkIO $ void $ tryIO $ do
			bytes <- readSV v
			p <- createProcess $
				 (proc cmd (toCommand params))
					{ std_in = CreatePipe
					, std_err = CreatePipe
					}
			putMVar pv p
			hClose $ stderrHandle p
			let h = stdinHandle p
			let send b = do
				hPrint h b
				hFlush h
			send bytes
			forever $
				send =<< readSV v
		let feeder = \n -> do
			meterupdate n
			writeSV v (fromBytesProcessed n)

		-- It can easily take 0.3 seconds to clean up after
		-- the transferinfo, and all that's involved is shutting
		-- down the process and associated thread cleanly. So,
		-- do it in the background.
		let cleanup = forkIO $ do
			void $ tryIO $ killThread tid
			void $ tryNonAsync $ 
				maybe noop (void . waitForProcess . processHandle)
					=<< tryTakeMVar pv

		let forcestop = do
			void $ tryIO $ killThread tid
			void $ tryNonAsync $ 
				maybe noop cleanupProcess
					=<< tryTakeMVar pv

		bracketIO noop (const cleanup) (const $ a feeder)
			`onException` liftIO forcestop

copyFromRemoteCheap :: Remote -> State -> Git.Repo -> Maybe (Key -> AssociatedFile -> FilePath -> Annex ())
#ifndef mingw32_HOST_OS
copyFromRemoteCheap r st repo
	| not $ Git.repoIsUrl repo = Just $ \key _af file -> guardUsable repo (giveup "cannot access remote") $ do
		gc <- getGitConfigFromState st
		loc <- liftIO $ gitAnnexLocation key repo gc
		liftIO $ ifM (R.doesPathExist loc)
			( do
				absloc <- absPath loc
				R.createSymbolicLink absloc (toRawFilePath file)
			, giveup "remote does not contain key"
			)
	| Git.repoIsSsh repo = Just $ \key af file ->
		ifM (Annex.Content.preseedTmp key file)
			( void $ copyFromRemote' True r st key af file nullMeterUpdate
			, giveup "cannot preseed rsync with existing content"
			)
	| otherwise = Nothing
#else
copyFromRemoteCheap _ _ _ = Nothing
#endif

{- Tries to copy a key's content to a remote's annex. -}
copyToRemote :: Remote -> State -> Key -> AssociatedFile -> MeterUpdate -> Annex ()
copyToRemote r st key file meterupdate = do
	repo <- getRepo r
	copyToRemote' repo r st key file meterupdate

copyToRemote' :: Git.Repo -> Remote -> State -> Key -> AssociatedFile -> MeterUpdate -> Annex ()
copyToRemote' repo r st@(State connpool duc _ _ _) key file meterupdate
	| not $ Git.repoIsUrl repo = ifM duc
		( guardUsable repo (giveup "cannot access remote") $ commitOnCleanup repo r st $
			copylocal =<< Annex.Content.prepSendAnnex key
		, giveup "remote does not have expected annex.uuid value"
		)
	| Git.repoIsSsh repo = commitOnCleanup repo r st $
		P2PHelper.store
			(Ssh.runProto r connpool (return False) . copyremotefallback)
			key file meterupdate
		
	| otherwise = giveup "copying to non-ssh repo not supported"
  where
	copylocal Nothing = giveup "content not available"
	copylocal (Just (object, checksuccess)) = do
		-- The checksuccess action is going to be run in
		-- the remote's Annex, but it needs access to the local
		-- Annex monad's state.
		checksuccessio <- Annex.withCurrentState checksuccess
		params <- Ssh.rsyncParams r Upload
		u <- getUUID
		hardlink <- wantHardLink
		-- run copy from perspective of remote
		res <- onLocalFast st $ ifM (Annex.Content.inAnnex key)
			( return True
			, runTransfer (Transfer Download u (fromKey id key)) file stdRetry $ \p -> do
				copier <- mkCopier hardlink st params
				let verify = Annex.Content.RemoteVerify r
				let rsp = RetrievalAllKeysSecure
				res <- logStatusAfter key $ Annex.Content.getViaTmp rsp verify key file $ \dest ->
					metered (Just (combineMeterUpdate meterupdate p)) key $ \_ p' -> 
						copier object (fromRawFilePath dest) p' (liftIO checksuccessio)
				Annex.Content.saveState True
				return res
			)
		unless res $
			giveup "failed to send content to remote"
	copyremotefallback p = either (const False) id
		<$> tryNonAsync (copyremotefallback' p)
	copyremotefallback' p = Annex.Content.sendAnnex key noop $ \object -> do
		-- This is too broad really, but recvkey normally
		-- verifies content anyway, so avoid complicating
		-- it with a local sendAnnex check and rollback.
		let unlocked = True
		oh <- mkOutputHandlerQuiet
		Ssh.rsyncHelper oh (Just p)
			=<< Ssh.rsyncParamsRemote unlocked r Upload key object file

fsckOnRemote :: Git.Repo -> [CommandParam] -> Annex (IO Bool)
fsckOnRemote r params
	| Git.repoIsUrl r = do
		s <- Ssh.git_annex_shell NoConsumeStdin r "fsck" params []
		return $ case s of
			Nothing -> return False
			Just (c, ps) -> batchCommand c ps
	| otherwise = return $ do
		program <- programPath
		r' <- Git.Config.read r
		environ <- getEnvironment
		let environ' = addEntries 
			[ ("GIT_WORK_TREE", fromRawFilePath $ Git.repoPath r')
			, ("GIT_DIR", fromRawFilePath $ Git.localGitDir r')
			] environ
		batchCommandEnv program (Param "fsck" : params) (Just environ')

{- The passed repair action is run in the Annex monad of the remote. -}
repairRemote :: Git.Repo -> Annex Bool -> Annex (IO Bool)
repairRemote r a = return $ do
	s <- Annex.new r
	Annex.eval s $ do
		Annex.BranchState.disableUpdate
		ensureInitialized
		a `finally` stopCoProcesses

data LocalRemoteAnnex = LocalRemoteAnnex Git.Repo (MVar (Maybe Annex.AnnexState))

{- This can safely be called on a Repo that is not local, but of course
 - onLocal will not work if used with the result. -}
mkLocalRemoteAnnex :: Git.Repo -> Annex (LocalRemoteAnnex)
mkLocalRemoteAnnex repo = LocalRemoteAnnex repo <$> liftIO (newMVar Nothing)

{- Runs an action from the perspective of a local remote.
 -
 - The AnnexState is cached for speed and to avoid resource leaks.
 - However, coprocesses are stopped after each call to avoid git
 - processes hanging around on removable media.
 -
 - The remote will be automatically initialized/upgraded first,
 - when possible.
 -}
onLocal :: State -> Annex a -> Annex a
onLocal (State _ _ _ _ lra) = onLocal' lra

onLocalRepo :: Git.Repo -> Annex a -> Annex a
onLocalRepo repo a = do
	lra <- mkLocalRemoteAnnex repo
	onLocal' lra a

onLocal' :: LocalRemoteAnnex -> Annex a -> Annex a
onLocal' (LocalRemoteAnnex repo v) a = liftIO (takeMVar v) >>= \case
	Nothing -> do
		st <- liftIO $ Annex.new repo
		go (st, ensureInitialized >> a)
	Just st -> go (st, a)
  where
	go (st, a') = do
		curro <- Annex.getState Annex.output
		let act = Annex.run (st { Annex.output = curro }) $
			a' `finally` stopCoProcesses
		(ret, st') <- liftIO $ act `onException` cache st
		liftIO $ cache st'
		return ret
	cache st = putMVar v (Just st)

{- Faster variant of onLocal.
 -
 - The repository's git-annex branch is not updated, as an optimisation.
 - No caller of onLocalFast can query data from the branch and be ensured
 - it gets the most current value. Caller of onLocalFast can make changes
 - to the branch, however.
 -}
onLocalFast :: State -> Annex a -> Annex a
onLocalFast st a = onLocal st $ Annex.BranchState.disableUpdate >> a

-- To avoid the overhead of trying copy-on-write every time, it's tried
-- once and if it fails, is not tried again.
newtype CopyCoWTried = CopyCoWTried (MVar Bool)

newCopyCoWTried :: IO CopyCoWTried
newCopyCoWTried = CopyCoWTried <$> newEmptyMVar

{- Copys a file. Uses copy-on-write if it is supported. Otherwise,
 - uses rsync, so that interrupted copies can be resumed. -}
rsyncOrCopyFile :: State -> [CommandParam] -> FilePath -> FilePath -> MeterUpdate -> Annex Bool
#ifdef mingw32_HOST_OS
rsyncOrCopyFile _st _rsyncparams src dest p =
	-- rsync is only available on Windows in some installation methods,
	-- and is not strictly needed here, so don't use it.
	docopywith copyFileExternal
  where
#else
rsyncOrCopyFile st rsyncparams src dest p =
	-- If multiple threads reach this at the same time, they
	-- will both try CoW, which is acceptable.
	ifM (liftIO $ isEmptyMVar copycowtried)
		( do
			ok <- docopycow
			void $ liftIO $ tryPutMVar copycowtried ok
			pure ok <||> dorsync
		, ifM (liftIO $ readMVar copycowtried)
			( docopycow <||> dorsync
			, dorsync
			)
		)
  where
	copycowtried = case st of
		State _ _ (CopyCoWTried v) _ _ -> v
	dorsync = do
		-- dest may already exist, so make sure rsync can write to it
		void $ liftIO $ tryIO $ allowWrite (toRawFilePath dest)
		oh <- mkOutputHandlerQuiet
		Ssh.rsyncHelper oh (Just p) $
			rsyncparams ++ [File src, File dest]
	docopycow = docopywith copyCoW
#endif
	docopywith a = liftIO $ watchFileSize dest p $
		a CopyTimeStamps src dest

commitOnCleanup :: Git.Repo -> Remote -> State -> Annex a -> Annex a
commitOnCleanup repo r st a = go `after` a
  where
	go = Annex.addCleanupAction (RemoteCleanup $ uuid r) cleanup
	cleanup
		| not $ Git.repoIsUrl repo = onLocalFast st $
			doQuietSideAction $
				Annex.Branch.commit =<< Annex.Branch.commitMessage
		| otherwise = do
			Just (shellcmd, shellparams) <-
				Ssh.git_annex_shell NoConsumeStdin
					repo "commit" [] []
			
			-- Throw away stderr, since the remote may not
			-- have a new enough git-annex shell to
			-- support committing.
			liftIO $ void $ catchMaybeIO $ withNullHandle $ \nullh ->
				let p = (proc shellcmd (toCommand shellparams))
					{ std_out = UseHandle nullh
					, std_err = UseHandle nullh
					}
				in withCreateProcess p $ \_ _ _ ->
					forceSuccessProcess p

wantHardLink :: Annex Bool
wantHardLink = (annexHardLink <$> Annex.getGitConfig)
	-- Not unlocked files that are hard linked in the work tree,
	-- because they can be modified at any time.
	<&&> (not <$> annexThin <$> Annex.getGitConfig)

-- Copies from src to dest, updating a meter. If the copy finishes
-- successfully, calls a final check action, which must also succeed, or
-- returns false.
--
-- If either the remote or local repository wants to use hard links,
-- the copier will do so (falling back to copying if a hard link cannot be
-- made).
--
-- When a hard link is created, returns Verified; the repo being linked
-- from is implicitly trusted, so no expensive verification needs to be
-- done.
type Copier = FilePath -> FilePath -> MeterUpdate -> Annex Bool -> Annex (Bool, Verification)

mkCopier :: Bool -> State -> [CommandParam] -> Annex Copier
mkCopier remotewanthardlink st rsyncparams = do
	let copier = \src dest p check -> unVerified $
		rsyncOrCopyFile st rsyncparams src dest p <&&> check
	localwanthardlink <- wantHardLink
	let linker = \src dest -> createLink src dest >> return True
	if remotewanthardlink || localwanthardlink
		then return $ \src dest p check ->
			ifM (liftIO (catchBoolIO (linker src dest)))
				( return (True, Verified)
				, copier src dest p check
				)
		else return copier

{- Normally the UUID of a local repository is checked at startup,
 - but annex-checkuuid config can prevent that. To avoid getting
 - confused, a deferred check is done just before the repository
 - is used.
 - This returns False when the repository UUID is not as expected. -}
type DeferredUUIDCheck = Annex Bool

data State = State Ssh.P2PSshConnectionPool DeferredUUIDCheck CopyCoWTried (Annex (Git.Repo, GitConfig)) LocalRemoteAnnex

getRepoFromState :: State -> Annex Git.Repo
getRepoFromState (State _ _ _ a _) = fst <$> a

#ifndef mingw32_HOST_OS
{- The config of the remote git repository, cached for speed. -}
getGitConfigFromState :: State -> Annex GitConfig
getGitConfigFromState (State _ _ _ a _) = snd <$> a
#endif

mkState :: Git.Repo -> UUID -> RemoteGitConfig -> Annex State
mkState r u gc = do
	pool <- Ssh.mkP2PSshConnectionPool
	copycowtried <- liftIO newCopyCoWTried
	lra <- mkLocalRemoteAnnex r
	(duc, getrepo) <- go
	return $ State pool duc copycowtried getrepo lra
  where
	go
		| remoteAnnexCheckUUID gc = return
			(return True, return (r, extractGitConfig FromGitConfig r))
		| otherwise = do
			rv <- liftIO newEmptyMVar
			let getrepo = ifM (liftIO $ isEmptyMVar rv)
				( do
					r' <- tryGitConfigRead False r True
					let t = (r', extractGitConfig FromGitConfig r')
					void $ liftIO $ tryPutMVar rv t
					return t
				, liftIO $ readMVar rv
				)

			cv <- liftIO newEmptyMVar
			let duc = ifM (liftIO $ isEmptyMVar cv)
				( do
					r' <- fst <$> getrepo
					u' <- getRepoUUID r'
					let ok = u' == u
					void $ liftIO $ tryPutMVar cv ok
					unless ok $
						warning $ Git.repoDescribe r ++ " is not the expected repository. The remote's annex-checkuuid configuration prevented noticing the change until now."
					return ok
				, liftIO $ readMVar cv
				)

			return (duc, getrepo)
