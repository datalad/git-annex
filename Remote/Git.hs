{- Standard git remotes.
 -
 - Copyright 2011-2021 Joey Hess <id@joeyh.name>
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
import Annex.CopyFile
import Annex.Verify
import Annex.Content (verificationOfContentFailed)
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
import Utility.Env
import Utility.Batch
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

import Control.Concurrent
import qualified Data.Map as M
import qualified Data.ByteString as S
import qualified Utility.RawFilePath as R
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
					Git.Construct.fromRemoteLocation (Git.fromConfigValue url) False g

{- Git remotes are normally set up using standard git commands, not
 - git-annex initremote and enableremote.
 -
 - For initremote, probe the location to find the uuid.
 - and set up a git remote.
 -
 - enableremote simply sets up a git remote using the stored location.
 - No attempt is made to make the remote be accessible via ssh key setup,
 - etc.
 -}
gitSetup :: SetupStage -> Maybe UUID -> Maybe CredPair -> RemoteConfig -> RemoteGitConfig -> Annex (RemoteConfig, UUID)
gitSetup Init mu _ c _ = do
	let location = maybe (giveup "Specify location=url") fromProposedAccepted $
		M.lookup locationField c
	r <- inRepo $ Git.Construct.fromRemoteLocation location False
	r' <- tryGitConfigRead False r False
	let u = getUncachedUUID r'
	if u == NoUUID
		then giveup "git repository does not have an annex uuid"
		else if isNothing mu || mu == Just u
			then enableRemote (Just u) c
			else giveup "git repository does not have specified uuid"
gitSetup (Enable _) mu _ c _ = enableRemote mu c
gitSetup (AutoEnable _) mu _ c _ = enableRemote mu c

enableRemote :: Maybe UUID -> RemoteConfig -> Annex (RemoteConfig, UUID)
enableRemote (Just u) c = do
	inRepo $ Git.Command.run
		[ Param "remote"
		, Param "add"
		, Param $ fromMaybe (giveup "no name") (SpecialRemote.lookupName c)
		, Param $ maybe (giveup "no location") fromProposedAccepted (M.lookup locationField c)
		]
	return (c, u)
enableRemote Nothing _ = giveup "unable to enable git remote with no specified uuid"

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
			go st c <$> remoteCost gc c defcst
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
			, retrieveKeyFileCheap = copyFromRemoteCheap st r
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
			, untrustworthy = False
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
			( pipedconfig Git.Config.ConfigList autoinit (Git.repoDescribe r)
			, return (Left "configlist failed")
			)
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
		readlocalannexconfig
			`catchNonAsync` const failedreadlocalconfig
  where
	haveconfig = not . M.null . Git.config

	pipedconfig st mustincludeuuuid configloc cmd params = do
		v <- liftIO $ Git.Config.fromPipe r cmd params st
		case v of
			Right (r', val, _err) -> do
				unless (isUUIDConfigured r' || S.null val || not mustincludeuuuid) $ do
					warning $ UnquotedString $ "Failed to get annex.uuid configuration of repository " ++ Git.repoDescribe r
					warning $ UnquotedString $ "Instead, got: " ++ show val
					warning "This is unexpected; please check the network transport!"
				return $ Right r'
			Left l -> do
				warning $ UnquotedString $ "Unable to parse git config from " ++ configloc
				return $ Left (show l)

	geturlconfig = Url.withUrlOptionsPromptingCreds $ \uo -> do
		let url = Git.repoLocation r ++ "/config"
		v <- withTmpFile "git-annex.tmp" $ \tmpfile h -> do
			liftIO $ hClose h
			Url.download' nullMeterUpdate Nothing url tmpfile uo >>= \case
				Right () -> pipedconfig Git.Config.ConfigNullList
					False url "git"
					[ Param "config"
					, Param "--null"
					, Param "--list"
					, Param "--file"
					, File tmpfile
					]
				Left err -> return (Left err)
		case v of
			Right r' -> do
				-- Cache when http remote is not bare for
				-- optimisation.
				unless (fromMaybe False $ Git.Config.isBare r') $
					setremote setRemoteBare False
				return r'
			Left err -> do
				set_ignore "not usable by git-annex" False
				warning $ UnquotedString $ url ++ " " ++ err
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
				warning $ UnquotedString $ "Remote " ++ n ++ " " ++ msg ++ "; setting annex-ignore"
				when longmessage $
					warning $ UnquotedString $ "This could be a problem with the git-annex installation on the remote. Please make sure that git-annex-shell is available in PATH when you ssh into the remote. Once you have fixed the git-annex installation, run: git annex enableremote " ++ n
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
			catchNonAsync (autoInitialize (pure [])) $ \e ->
				warning $ UnquotedString $ "Remote " ++ Git.repoDescribe r ++
					": "  ++ show e
			Annex.getState Annex.repo
		s <- newLocal r
		liftIO $ Annex.eval s $ check
			`finally` quiesce True
		
	failedreadlocalconfig = do
		unless hasuuid $ case Git.remoteName r of
			Nothing -> noop
			Just n -> do
				warning $ UnquotedString $ "Remote " ++ n ++ " cannot currently be accessed."
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
		gc <- Annex.getGitConfig
		Url.withUrlOptionsPromptingCreds $ \uo -> 
			anyM (\u -> Url.checkBoth u (fromKey keySize key) uo)
				(keyUrls gc repo rmt key)
	checkremote = P2PHelper.checkpresent (Ssh.runProto rmt connpool (cantCheck rmt)) key
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
		| remoteAnnexBare remoteconfig == Just False = annexLocationsNonBare gc key
		| otherwise = annexLocationsBare gc key
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
		, giveup "remote does not have expected annex.uuid value"
		)
	| Git.repoIsHttp repo = giveup "dropping from http remote not supported"
	| otherwise = P2PHelper.remove (Ssh.runProto r connpool (return False)) key

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
		let withconn = Ssh.withP2PSshConnection r connpool failedlock
		P2PHelper.lock withconn Ssh.runProtoConn (uuid r) key callback
	| otherwise = failedlock
  where
	failedlock = giveup "can't lock content"

{- Tries to copy a key's content from a remote's annex to a file. -}
copyFromRemote :: Remote -> State -> Key -> AssociatedFile -> FilePath -> MeterUpdate -> VerifyConfig -> Annex Verification
copyFromRemote r st key file dest meterupdate vc = do
	repo <- getRepo r
	copyFromRemote'' repo r st key file dest meterupdate vc

copyFromRemote'' :: Git.Repo -> Remote -> State -> Key -> AssociatedFile -> FilePath -> MeterUpdate -> VerifyConfig -> Annex Verification
copyFromRemote'' repo r st@(State connpool _ _ _ _) key file dest meterupdate vc
	| Git.repoIsHttp repo = verifyKeyContentIncrementally vc key $ \iv -> do
		gc <- Annex.getGitConfig
		ok <- Url.withUrlOptionsPromptingCreds $
			Annex.Content.downloadUrl False key meterupdate iv (keyUrls gc repo r key) dest
		unless ok $
			giveup "failed to download content"
	| not $ Git.repoIsUrl repo = guardUsable repo (giveup "cannot access remote") $ do
		u <- getUUID
		hardlink <- wantHardLink
		let bwlimit = remoteAnnexBwLimit (gitconfig r)
		-- run copy from perspective of remote
		onLocalFast st $ Annex.Content.prepSendAnnex' key >>= \case
			Just (object, check) -> do
				let checksuccess = check >>= \case
					Just err -> giveup err
					Nothing -> return True
				copier <- mkFileCopier hardlink st
				(ok, v) <- runTransfer (Transfer Download u (fromKey id key))
					Nothing file Nothing stdRetry $ \p ->
						metered (Just (combineMeterUpdate p meterupdate)) key bwlimit $ \_ p' -> 
							copier object dest key p' checksuccess vc
				if ok
					then return v
					else giveup "failed to retrieve content from remote"
			Nothing -> giveup "content is not present in remote"
	| Git.repoIsSsh repo =
		P2PHelper.retrieve
			(gitconfig r)
			(Ssh.runProto r connpool (return (False, UnVerified)))
			key file dest meterupdate vc
	| otherwise = giveup "copying from non-ssh, non-http remote not supported"

copyFromRemoteCheap :: State -> Git.Repo -> Maybe (Key -> AssociatedFile -> FilePath -> Annex ())
#ifndef mingw32_HOST_OS
copyFromRemoteCheap st repo
	| not $ Git.repoIsUrl repo = Just $ \key _af file -> guardUsable repo (giveup "cannot access remote") $ do
		gc <- getGitConfigFromState st
		loc <- liftIO $ gitAnnexLocation key repo gc
		liftIO $ ifM (R.doesPathExist loc)
			( do
				absloc <- absPath loc
				R.createSymbolicLink absloc (toRawFilePath file)
			, giveup "remote does not contain key"
			)
	| otherwise = Nothing
#else
copyFromRemoteCheap _ _ = Nothing
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
			copylocal =<< Annex.Content.prepSendAnnex' key
		, giveup "remote does not have expected annex.uuid value"
		)
	| Git.repoIsSsh repo =
		P2PHelper.store (gitconfig r)
			(Ssh.runProto r connpool (return False))
			key file meterupdate
		
	| otherwise = giveup "copying to non-ssh repo not supported"
  where
	copylocal Nothing = giveup "content not available"
	copylocal (Just (object, check)) = do
		-- The check action is going to be run in
		-- the remote's Annex, but it needs access to the local
		-- Annex monad's state.
		checkio <- Annex.withCurrentState check
		u <- getUUID
		hardlink <- wantHardLink
		let bwlimit = remoteAnnexBwLimit (gitconfig r)
		-- run copy from perspective of remote
		res <- onLocalFast st $ ifM (Annex.Content.inAnnex key)
			( return True
			, runTransfer (Transfer Download u (fromKey id key)) Nothing file Nothing stdRetry $ \p -> do
				let verify = RemoteVerify r
				copier <- mkFileCopier hardlink st
				let rsp = RetrievalAllKeysSecure
				let checksuccess = liftIO checkio >>= \case
					Just err -> giveup err
					Nothing -> return True
				logStatusAfter key $ Annex.Content.getViaTmp rsp verify key file $ \dest ->
					metered (Just (combineMeterUpdate meterupdate p)) key bwlimit $ \_ p' -> 
						copier object (fromRawFilePath dest) key p' checksuccess verify
			)
		unless res $
			giveup "failed to send content to remote"

fsckOnRemote :: Git.Repo -> [CommandParam] -> Annex (IO Bool)
fsckOnRemote r params
	| Git.repoIsUrl r = return $ return False
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
		ensureInitialized (pure [])
		a `finally` quiesce True

data LocalRemoteAnnex = LocalRemoteAnnex Git.Repo (MVar [(Annex.AnnexState, Annex.AnnexRead)])

{- This can safely be called on a Repo that is not local, but of course
 - onLocal will not work if used with the result. -}
mkLocalRemoteAnnex :: Git.Repo -> Annex (LocalRemoteAnnex)
mkLocalRemoteAnnex repo = LocalRemoteAnnex repo <$> liftIO (newMVar [])

{- Runs an action from the perspective of a local remote.
 -
 - The AnnexState is cached for speed and to avoid resource leaks.
 - However, it is quiesced after each call to avoid git processes
 - hanging around on removable media.
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

newLocal :: Git.Repo -> Annex (Annex.AnnexState, Annex.AnnexRead)
newLocal repo = do
	(st, rd) <- liftIO $ Annex.new repo
	debugenabled <- Annex.getRead Annex.debugenabled
	debugselector <- Annex.getRead Annex.debugselector
	return (st,  rd
		{ Annex.debugenabled = debugenabled
		, Annex.debugselector = debugselector
		})

onLocal' :: LocalRemoteAnnex -> Annex a -> Annex a
onLocal' (LocalRemoteAnnex repo mv) a = liftIO (takeMVar mv) >>= \case
	[] -> do
		liftIO $ putMVar mv []
		v <- newLocal repo
		go (v, ensureInitialized (pure []) >> a)
	(v:rest) -> do
		liftIO $ putMVar mv rest
		go (v, a)
  where
	go ((st, rd), a') = do
		curro <- Annex.getState Annex.output
		let act = Annex.run (st { Annex.output = curro }, rd) $
			a' `finally` quiesce True
		(ret, (st', _rd)) <- liftIO $ act `onException` cache (st, rd)
		liftIO $ cache (st', rd)
		return ret
	cache v = do
		l <- takeMVar mv
		putMVar mv (v:l)

{- Faster variant of onLocal.
 -
 - The repository's git-annex branch is not updated, as an optimisation.
 - No caller of onLocalFast can query data from the branch and be ensured
 - it gets the most current value. Caller of onLocalFast can make changes
 - to the branch, however.
 -}
onLocalFast :: State -> Annex a -> Annex a
onLocalFast st a = onLocal st $ Annex.BranchState.disableUpdate >> a

commitOnCleanup :: Git.Repo -> Remote -> State -> Annex a -> Annex a
commitOnCleanup repo r st a = go `after` a
  where
	go = Annex.addCleanupAction (RemoteCleanup $ uuid r) cleanup
	cleanup
		| not $ Git.repoIsUrl repo = onLocalFast st $
			doQuietSideAction $
				Annex.Branch.commit =<< Annex.Branch.commitMessage
		| otherwise = noop

wantHardLink :: Annex Bool
wantHardLink = (annexHardLink <$> Annex.getGitConfig)
	-- Not unlocked files that are hard linked in the work tree,
	-- because they can be modified at any time.
	<&&> (not <$> annexThin <$> Annex.getGitConfig)

type FileCopier = FilePath -> FilePath -> Key -> MeterUpdate -> Annex Bool -> VerifyConfig -> Annex (Bool, Verification)

-- If either the remote or local repository wants to use hard links,
-- the copier will do so (falling back to copying if a hard link cannot be
-- made).
--
-- When a hard link is created, returns Verified; the repo being linked
-- from is implicitly trusted, so no expensive verification needs to be
-- done. Also returns Verified if the key's content is verified while
-- copying it.
mkFileCopier :: Bool -> State -> Annex FileCopier
mkFileCopier remotewanthardlink (State _ _ copycowtried _ _) = do
	localwanthardlink <- wantHardLink
	let linker = \src dest -> R.createLink (toRawFilePath src) (toRawFilePath dest) >> return True
	if remotewanthardlink || localwanthardlink
		then return $ \src dest k p check verifyconfig ->
			ifM (liftIO (catchBoolIO (linker src dest)))
				( ifM check
					( return (True, Verified)
					, do
						verificationOfContentFailed (toRawFilePath dest)
						return (False, UnVerified)
					)
				, copier src dest k p check verifyconfig
				)
		else return copier
  where
	copier src dest k p check verifyconfig = do
		iv <- startVerifyKeyContentIncrementally verifyconfig k
		liftIO (fileCopier copycowtried src dest p iv) >>= \case
			Copied -> ifM check
				( finishVerifyKeyContentIncrementally iv
				, do
					verificationOfContentFailed (toRawFilePath dest)
					return (False, UnVerified)
				)
			CopiedCoW -> unVerified check

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
						warning $ UnquotedString $ Git.repoDescribe r ++ " is not the expected repository. The remote's annex-checkuuid configuration prevented noticing the change until now."
					return ok
				, liftIO $ readMVar cv
				)

			return (duc, getrepo)
