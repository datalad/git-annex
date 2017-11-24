{- Standard git remotes.
 -
 - Copyright 2011-2017 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Remote.Git (
	remote,
	configRead,
	repoAvail,
	onLocal,
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
import Utility.Tmp
import Config
import Config.Cost
import Config.DynamicConfig
import Annex.Init
import Annex.Version
import Types.CleanupActions
import qualified CmdLine.GitAnnexShell.Fields as Fields
import Logs.Location
import Utility.Metered
#ifndef mingw32_HOST_OS
import Utility.CopyFile
#endif
import Utility.Env
import Utility.Batch
import Utility.SimpleProtocol
import Remote.Helper.Git
import Remote.Helper.Messages
import Remote.Helper.Export
import qualified Remote.Helper.Ssh as Ssh
import qualified Remote.GCrypt
import qualified Remote.P2P
import P2P.Address
import Annex.Path
import Creds
import Messages.Progress
import Types.NumCopies
import Annex.Action

import Control.Concurrent
import Control.Concurrent.MSampleVar
import qualified Data.Map as M
import Network.URI

remote :: RemoteType
remote = RemoteType
	{ typename = "git"
	, enumerate = list
	, generate = gen
	, setup = gitSetup
	, exportSupported = exportUnsupported
	}

list :: Bool -> Annex [Git.Repo]
list autoinit = do
	c <- fromRepo Git.config
	rs <- mapM (tweakurl c) =<< fromRepo Git.remotes
	mapM (configRead autoinit) rs
  where
	annexurl n = "remote." ++ n ++ ".annexurl"
	tweakurl c r = do
		let n = fromJust $ Git.remoteName r
		case M.lookup (annexurl n) c of
			Nothing -> return r
			Just url -> inRepo $ \g ->
				Git.Construct.remoteNamed n $
					Git.Construct.fromRemoteLocation url g

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
		Url.parseURIRelaxed =<< M.lookup "location" c
	g <- Annex.gitRepo
	u <- case filter (\r -> Git.location r == Git.Url location) (Git.remotes g) of
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
		, Param $ fromMaybe (giveup "no name") (M.lookup "name" c)
		, Param $ fromMaybe (giveup "no location") (M.lookup "location" c)
		]
	return (c, u)
gitSetup (Enable _) Nothing _ _ _ = error "unable to enable git remote with no specified uuid"

{- It's assumed to be cheap to read the config of non-URL remotes, so this is
 - done each time git-annex is run in a way that uses remotes.
 -
 - Conversely, the config of an URL remote is only read when there is no
 - cached UUID value. -}
configRead :: Bool -> Git.Repo -> Annex Git.Repo
configRead autoinit r = do
	gc <- Annex.getRemoteGitConfig r
	u <- getRepoUUID r
	annexignore <- liftIO $ getDynamicConfig (remoteAnnexIgnore gc)
	case (repoCheap r, annexignore, u) of
		(_, True, _) -> return r
		(True, _, _) -> tryGitConfigRead autoinit r
		(False, _, NoUUID) -> tryGitConfigRead autoinit r
		_ -> return r

gen :: Git.Repo -> UUID -> RemoteConfig -> RemoteGitConfig -> Annex (Maybe Remote)
gen r u c gc
	| Git.GCrypt.isEncrypted r = Remote.GCrypt.chainGen r u c gc
	| otherwise = case repoP2PAddress r of
		Nothing -> go <$> remoteCost gc defcst
		Just addr -> Remote.P2P.chainGen addr r u c gc
  where
	defcst = if repoCheap r then cheapRemoteCost else expensiveRemoteCost
	go cst = Just new
	  where
		new = Remote 
			{ uuid = u
			, cost = cst
			, name = Git.repoDescribe r
			, storeKey = copyToRemote new
			, retrieveKeyFile = copyFromRemote new
			, retrieveKeyFileCheap = copyFromRemoteCheap new
			, removeKey = dropKey new
			, lockContent = Just (lockKey new)
			, checkPresent = inAnnex new
			, checkPresentCheap = repoCheap r
			, exportActions = exportUnsupported
			, whereisKey = Nothing
			, remoteFsck = if Git.repoIsUrl r
				then Nothing
				else Just $ fsckOnRemote r
			, repairRepo = if Git.repoIsUrl r
				then Nothing
				else Just $ repairRemote r
			, config = c
			, localpath = localpathCalc r
			, repo = r
			, gitconfig = gc { remoteGitConfig = extractGitConfig r }
			, readonly = Git.repoIsHttp r
			, availability = availabilityCalc r
			, remotetype = remote
			, mkUnavailable = unavailable r u c gc
			, getInfo = gitRepoInfo new
			, claimUrl = Nothing
			, checkUrl = Nothing
			}

unavailable :: Git.Repo -> UUID -> RemoteConfig -> RemoteGitConfig -> Annex (Maybe Remote)
unavailable r u c gc = gen r' u c gc
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
tryGitConfigRead :: Bool -> Git.Repo -> Annex Git.Repo
tryGitConfigRead autoinit r 
	| haveconfig r = return r -- already read
	| Git.repoIsSsh r = store $ do
		v <- Ssh.onRemote NoConsumeStdin r
			(pipedconfig, return (Left $ giveup "configlist failed"))
			"configlist" [] configlistfields
		case v of
			Right r'
				| haveconfig r' -> return r'
				| otherwise -> configlist_failed
			Left _ -> configlist_failed
	| Git.repoIsHttp r = store geturlconfig
	| Git.GCrypt.isEncrypted r = handlegcrypt =<< getConfigMaybe (remoteConfig r "uuid")
	| Git.repoIsUrl r = return r
	| otherwise = store $ liftIO $ 
		readlocalannexconfig `catchNonAsync` (const $ return r)
  where
	haveconfig = not . M.null . Git.config

	pipedconfig cmd params = do
		v <- Git.Config.fromPipe r cmd params
		case v of
			Right (r', val) -> do
				unless (isUUIDConfigured r' || null val) $ do
					warningIO $ "Failed to get annex.uuid configuration of repository " ++ Git.repoDescribe r
					warningIO $ "Instead, got: " ++ show val
					warningIO $ "This is unexpected; please check the network transport!"
				return $ Right r'
			Left l -> return $ Left l

	geturlconfig = do
		uo <- Url.getUrlOptions
		v <- liftIO $ withTmpFile "git-annex.tmp" $ \tmpfile h -> do
			hClose h
			let url = Git.repoLocation r ++ "/config"
			ifM (Url.downloadQuiet url tmpfile uo)
				( Just <$> pipedconfig "git" [Param "config", Param "--null", Param "--list", Param "--file", File tmpfile]
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

	store = observe $ \r' -> do
		g <- gitRepo
		let l = Git.remotes g
		let g' = g { Git.remotes = exchange l r' }
		Annex.changeState $ \s -> s { Annex.repo = g' }

	exchange [] _ = []
	exchange (old:ls) new
		| Git.remoteName old == Git.remoteName new =
			new : exchange ls new
		| otherwise =
			old : exchange ls new

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
			Just v -> store $ liftIO $ setUUID r $
				genUUIDInNameSpace gCryptNameSpace v

	{- The local repo may not yet be initialized, so try to initialize
	 - it if allowed. However, if that fails, still return the read
	 - git config. -}
	readlocalannexconfig = do
		let check = do
			Annex.BranchState.disableUpdate
			void $ tryNonAsync $ ensureInitialized
			Annex.getState Annex.repo
		s <- Annex.new r
		Annex.eval s $ check `finally` stopCoProcesses
		
	configlistfields = if autoinit
		then [(Fields.autoInit, "1")]
		else []

{- Checks if a given remote has the content for a key in its annex. -}
inAnnex :: Remote -> Key -> Annex Bool
inAnnex rmt key
	| Git.repoIsHttp r = checkhttp
	| Git.repoIsUrl r = checkremote
	| otherwise = checklocal
  where
	r = repo rmt
	checkhttp = do
		showChecking r
		ifM (Url.withUrlOptions $ \uo -> anyM (\u -> Url.checkBoth u (keySize key) uo) (keyUrls rmt key))
			( return True
			, giveup "not found"
			)
	checkremote = Ssh.inAnnex r key
	checklocal = guardUsable r (cantCheck r) $
		maybe (cantCheck r) return
			=<< onLocalFast rmt (Annex.Content.inAnnexSafe key)

keyUrls :: Remote -> Key -> [String]
keyUrls r key = map tourl locs'
  where
	tourl l = Git.repoLocation (repo r) ++ "/" ++ l
	-- If the remote is known to not be bare, try the hash locations
	-- used for non-bare repos first, as an optimisation.
	locs
		| remoteAnnexBare remoteconfig == Just False = reverse (annexLocations cfg key)
		| otherwise = annexLocations cfg key
#ifndef mingw32_HOST_OS
	locs' = locs
#else
	locs' = map (replace "\\" "/") locs
#endif
	remoteconfig = gitconfig r
	cfg = remoteGitConfig remoteconfig

dropKey :: Remote -> Key -> Annex Bool
dropKey r key
	| not $ Git.repoIsUrl (repo r) =
		guardUsable (repo r) (return False) $
			commitOnCleanup r $ onLocalFast r $ do
				ensureInitialized
				whenM (Annex.Content.inAnnex key) $ do
					Annex.Content.lockContentForRemoval key $ \lock -> do
						Annex.Content.removeAnnex lock
						logStatus key InfoMissing
					Annex.Content.saveState True
				return True
	| Git.repoIsHttp (repo r) = giveup "dropping from http remote not supported"
	| otherwise = commitOnCleanup r $ Ssh.dropKey (repo r) key

lockKey :: Remote -> Key -> (VerifiedCopy -> Annex r) -> Annex r
lockKey r key callback
	| not $ Git.repoIsUrl (repo r) =
		guardUsable (repo r) failedlock $ do
			inorigrepo <- Annex.makeRunner
			-- Lock content from perspective of remote,
			-- and then run the callback in the original
			-- annex monad, not the remote's.
			onLocalFast r $ 
				Annex.Content.lockContentShared key $ \vc ->
					ifM (Annex.Content.inAnnex key)
						( liftIO $ inorigrepo $ callback vc
						, failedlock
						)
	| Git.repoIsSsh (repo r) = do
		showLocking r
		Just (cmd, params) <- Ssh.git_annex_shell ConsumeStdin
			(repo r) "lockcontent"
			[Param $ key2file key] []
		(Just hin, Just hout, Nothing, p) <- liftIO $ 
			withFile devNull WriteMode $ \nullh ->
				createProcess $
					 (proc cmd (toCommand params))
						{ std_in = CreatePipe
						, std_out = CreatePipe
						, std_err = UseHandle nullh
						}
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
	| otherwise = failedlock
  where
	failedlock = giveup "can't lock content"

{- Tries to copy a key's content from a remote's annex to a file. -}
copyFromRemote :: Remote -> Key -> AssociatedFile -> FilePath -> MeterUpdate -> Annex (Bool, Verification)
copyFromRemote r key file dest p
	| Git.repoIsHttp (repo r) = unVerified $
		Annex.Content.downloadUrl key p (keyUrls r key) dest
	| otherwise = commandMetered (Just p) key (return Nothing) $
		copyFromRemote' r key file dest

copyFromRemote' :: Remote -> Key -> AssociatedFile -> FilePath -> MeterUpdate -> Annex (Bool, Verification)
copyFromRemote' r key file dest meterupdate
	| not $ Git.repoIsUrl (repo r) = guardUsable (repo r) (unVerified (return False)) $ do
		params <- Ssh.rsyncParams r Download
		u <- getUUID
		hardlink <- wantHardLink
		-- run copy from perspective of remote
		onLocalFast r $ do
			ensureInitialized
			v <- Annex.Content.prepSendAnnex key
			case v of
				Nothing -> return (False, UnVerified)
				Just (object, checksuccess) -> do
					copier <- mkCopier hardlink params
					runTransfer (Transfer Download u key)
						file forwardRetry
						(\p -> copier object dest (combineMeterUpdate p meterupdate) checksuccess)
	| Git.repoIsSsh (repo r) = unVerified $ feedprogressback $ \p -> do
		Ssh.rsyncHelper (Just (combineMeterUpdate meterupdate p))
			=<< Ssh.rsyncParamsRemote False r Download key dest file
	| otherwise = giveup "copying from non-ssh, non-http remote not supported"
  where
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
			: maybe [] (\f -> [(Fields.associatedFile, f)]) afile
		Just (cmd, params) <- Ssh.git_annex_shell ConsumeStdin
			(repo r) "transferinfo" 
			[Param $ key2file key] fields
		v <- liftIO (newEmptySV :: IO (MSampleVar Integer))
		pidv <- liftIO $ newEmptyMVar
		tid <- liftIO $ forkIO $ void $ tryIO $ do
			bytes <- readSV v
			p <- createProcess $
				 (proc cmd (toCommand params))
					{ std_in = CreatePipe
					, std_err = CreatePipe
					}
			putMVar pidv (processHandle p)
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
				maybe noop (void . waitForProcess)
					=<< tryTakeMVar pidv
		bracketIO noop (const cleanup) (const $ a feeder)

copyFromRemoteCheap :: Remote -> Key -> AssociatedFile -> FilePath -> Annex Bool
#ifndef mingw32_HOST_OS
copyFromRemoteCheap r key af file
	| not $ Git.repoIsUrl (repo r) = guardUsable (repo r) (return False) $ liftIO $ do
		loc <- gitAnnexLocation key (repo r) $
			remoteGitConfig $ gitconfig r
		ifM (doesFileExist loc)
			( do
				absloc <- absPath loc
				catchBoolIO $ do
					createSymbolicLink absloc file
					return True
			, return False
			)
	| Git.repoIsSsh (repo r) =
		ifM (Annex.Content.preseedTmp key file)
			( fst <$> copyFromRemote r key af file nullMeterUpdate
			, return False
			)
	| otherwise = return False
#else
copyFromRemoteCheap _ _ _ _ = return False
#endif

{- Tries to copy a key's content to a remote's annex. -}
copyToRemote :: Remote -> Key -> AssociatedFile -> MeterUpdate -> Annex Bool
copyToRemote r key file meterupdate
	| not $ Git.repoIsUrl (repo r) =
		guardUsable (repo r) (return False) $ commitOnCleanup r $
			copylocal =<< Annex.Content.prepSendAnnex key
	| Git.repoIsSsh (repo r) = commitOnCleanup r $
		Annex.Content.sendAnnex key noop $ \object ->
			withmeter object $ \p -> do
				-- This is too broad really, but recvkey normally
				-- verifies content anyway, so avoid complicating
				-- it with a local sendAnnex check and rollback.
				unlocked <- isDirect <||> versionSupportsUnlockedPointers
				Ssh.rsyncHelper (Just p)
					=<< Ssh.rsyncParamsRemote unlocked r Upload key object file
	| otherwise = giveup "copying to non-ssh repo not supported"
  where
	withmeter object = commandMetered (Just meterupdate) key (return $ Just object)
	copylocal Nothing = return False
	copylocal (Just (object, checksuccess)) = withmeter object $ \p -> do
		-- The checksuccess action is going to be run in
		-- the remote's Annex, but it needs access to the local
		-- Annex monad's state.
		checksuccessio <- Annex.withCurrentState checksuccess
		params <- Ssh.rsyncParams r Upload
		u <- getUUID
		hardlink <- wantHardLink
		-- run copy from perspective of remote
		onLocalFast r $ ifM (Annex.Content.inAnnex key)
			( return True
			, do
				ensureInitialized
				copier <- mkCopier hardlink params
				let verify = Annex.Content.RemoteVerify r
				runTransfer (Transfer Download u key) file forwardRetry $ \p' ->
					let p'' = combineMeterUpdate p p'
					in Annex.Content.saveState True `after`
						Annex.Content.getViaTmp verify key
							(\dest -> copier object dest p'' (liftIO checksuccessio))
			)

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
			[ ("GIT_WORK_TREE", Git.repoPath r')
			, ("GIT_DIR", Git.localGitDir r')
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

{- Runs an action from the perspective of a local remote.
 -
 - The AnnexState is cached for speed and to avoid resource leaks.
 - However, coprocesses are stopped after each call to avoid git
 - processes hanging around on removable media.
 -}
onLocal :: Remote -> Annex a -> Annex a
onLocal r a = do
	m <- Annex.getState Annex.remoteannexstate
	go =<< maybe
		(liftIO $ Annex.new $ repo r)
		return
		(M.lookup (uuid r) m)
  where
	cache st = Annex.changeState $ \s -> s
		{ Annex.remoteannexstate = M.insert (uuid r) st (Annex.remoteannexstate s) }
	go st = do
		curro <- Annex.getState Annex.output
		(ret, st') <- liftIO $ Annex.run (st { Annex.output = curro }) $
			a `finally` stopCoProcesses
		cache st'
		return ret

{- Faster variant of onLocal.
 -
 - The repository's git-annex branch is not updated, as an optimisation.
 - No caller of onLocalFast can query data from the branch and be ensured
 - it gets the most current value. Caller of onLocalFast can make changes
 - to the branch, however.
 -}
onLocalFast :: Remote -> Annex a -> Annex a
onLocalFast r a = onLocal r $ Annex.BranchState.disableUpdate >> a

{- Copys a file with rsync unless both locations are on the same
 - filesystem. Then cp could be faster. -}
rsyncOrCopyFile :: [CommandParam] -> FilePath -> FilePath -> MeterUpdate -> Annex Bool
rsyncOrCopyFile rsyncparams src dest p =
#ifdef mingw32_HOST_OS
	dorsync
  where
#else
	ifM (sameDeviceIds src dest) (docopy, dorsync)
  where
	sameDeviceIds a b = (==) <$> getDeviceId a <*> getDeviceId b
	getDeviceId f = deviceID <$> liftIO (getFileStatus $ parentDir f)
	docopy = liftIO $ watchFileSize dest p $
		copyFileExternal CopyTimeStamps src dest
#endif
	dorsync = Ssh.rsyncHelper (Just p) $
		rsyncparams ++ [File src, File dest]

commitOnCleanup :: Remote -> Annex a -> Annex a
commitOnCleanup r a = go `after` a
  where
	go = Annex.addCleanup (RemoteCleanup $ uuid r) cleanup
	cleanup
		| not $ Git.repoIsUrl (repo r) = onLocalFast r $
			doQuietSideAction $
				Annex.Branch.commit "update"
		| otherwise = void $ do
			Just (shellcmd, shellparams) <-
				Ssh.git_annex_shell NoConsumeStdin
					(repo r) "commit" [] []
			
			-- Throw away stderr, since the remote may not
			-- have a new enough git-annex shell to
			-- support committing.
			liftIO $ catchMaybeIO $
				withQuietOutput createProcessSuccess $
					proc shellcmd $
						toCommand shellparams

wantHardLink :: Annex Bool
wantHardLink = (annexHardLink <$> Annex.getGitConfig)
	-- Not direct mode files because they can be modified at any time.
	<&&> (not <$> isDirect)
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

mkCopier :: Bool -> [CommandParam] -> Annex Copier
mkCopier remotewanthardlink rsyncparams = do
	let copier = \src dest p check -> unVerified $
		rsyncOrCopyFile rsyncparams src dest p <&&> check
	localwanthardlink <- wantHardLink
	let linker = \src dest -> createLink src dest >> return True
	ifM (pure (remotewanthardlink || localwanthardlink) <&&> not <$> isDirect)
		( return $ \src dest p check ->
			ifM (liftIO (catchBoolIO (linker src dest)))
				( return (True, Verified)
				, copier src dest p check
				)
		, return copier
		)
