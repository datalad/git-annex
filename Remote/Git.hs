{- Standard git remotes.
 -
 - Copyright 2011-2024 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Remote.Git (
	remote,
	configRead,
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
import Logs.Proxy
import Logs.Cluster.Basic
import Utility.Metered
import Utility.Env
import Utility.Batch
import qualified Utility.FileIO as F
import Remote.Helper.Git
import Remote.Helper.Messages
import Remote.Helper.ExportImport
import qualified Remote.Helper.Ssh as Ssh
import qualified Remote.GCrypt
import qualified Remote.GitLFS
import qualified Remote.P2P
import qualified Remote.Helper.P2P as P2PHelper
import qualified P2P.Protocol as P2P
import P2P.Address
import P2P.Http.Url
import P2P.Http.Client
import Annex.Path
import Creds
import Types.NumCopies
import Annex.SafeDropProof
import Types.ProposedAccepted
import Annex.Action
import Messages.Progress

import Control.Concurrent
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List.NonEmpty as NE
import qualified Data.ByteString as B
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
		, yesNoParser versioningField (Just False) HiddenField
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
	cfg <- fromRepo Git.config
	fullcfg <- fromRepo Git.fullconfig
	rs <- mapM (tweakurl cfg fullcfg) =<< Annex.getGitRemotes
	rs' <- mapM (configRead autoinit) (filter (not . isGitRemoteAnnex) rs)
	proxies <- doQuietAction getProxies
	if proxies == mempty
		then return rs'
		else do
			proxied <- listProxied proxies rs'
			return (proxied++rs')
  where
	tweakurl cfg fullcfg r = do
		let n = fromJust $ Git.remoteName r
		case getAnnexUrl r cfg fullcfg of
			Just url | not (isP2PHttpProtocolUrl url) ->
				inRepo $ \g -> Git.Construct.remoteNamed n $
					Git.Construct.fromRemoteLocation url
						False g
			_ -> return r

getAnnexUrl :: Git.Repo -> Git.RepoConfig -> Git.RepoFullConfig -> Maybe String
getAnnexUrl r cfg fullcfg = 
	(Git.fromConfigValue <$> M.lookup (annexUrlConfigKey r) cfg)
		<|>
	annexInsteadOfUrl fullcfg (Git.repoLocation r)

annexUrlConfigKey :: Git.Repo -> Git.ConfigKey
annexUrlConfigKey r = remoteConfig r "annexurl"

isGitRemoteAnnex :: Git.Repo -> Bool
isGitRemoteAnnex r = "annex::" `isPrefixOf` Git.repoLocation r

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
gitSetup :: SetupStage -> Maybe UUID -> RemoteName -> Maybe CredPair -> RemoteConfig -> RemoteGitConfig -> Annex (RemoteConfig, UUID)
gitSetup Init mu _ _ c gc = do
	let location = maybe (giveup "Specify location=url") fromProposedAccepted $
		M.lookup locationField c
	r <- inRepo $ Git.Construct.fromRemoteLocation location False
	r' <- tryGitConfigRead gc False r False
	let u = getUncachedUUID r'
	if u == NoUUID
		then giveup "git repository does not have an annex uuid"
		else if isNothing mu || mu == Just u
			then enableRemote (Just u) c
			else giveup "git repository does not have specified uuid"
gitSetup (Enable _) mu _ _ c _ = enableRemote mu c
gitSetup (AutoEnable _) mu _ _ c _ = enableRemote mu c

enableRemote :: Maybe UUID -> RemoteConfig -> Annex (RemoteConfig, UUID)
enableRemote (Just u) c = do
	rs <- Annex.getGitRemotes
	unless (any (\r -> Git.remoteName r == Just cname) rs) $
		inRepo $ Git.Command.run
			[ Param "remote"
			, Param "add"
			, Param cname
			, Param clocation
			]
	return (c, u)
  where
	cname = fromMaybe (giveup "no name") (SpecialRemote.lookupName c)
	clocation = maybe (giveup "no location") fromProposedAccepted (M.lookup locationField c)
enableRemote Nothing _ = giveup "unable to enable git remote with no specified uuid"

{- It's assumed to be cheap to read the config of non-URL remotes, so this is
 - done each time git-annex is run in a way that uses remotes, unless
 - annex-checkuuid is false.
 -
 - The config of other URL remotes is only read when there is no
 - cached UUID value. 
 -}
configRead :: Bool -> Git.Repo -> Annex Git.Repo
configRead autoinit r = do
	gc <- Annex.getRemoteGitConfig r
	hasuuid <- (/= NoUUID) <$> getRepoUUID r
	annexignore <- liftIO $ getDynamicConfig (remoteAnnexIgnore gc)
	case (repoCheap r, annexignore, hasuuid) of
		(True, _, _)
			| remoteAnnexCheckUUID gc -> tryGitConfigRead gc autoinit r hasuuid
			| otherwise -> return r
		(_, True, _) -> return r
		(False, _, False) -> configSpecialGitRemotes r >>= \case
			Nothing -> tryGitConfigRead gc autoinit r False
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
			go st c <$> remoteCost gc c (defaultRepoCost r)
		Just addr -> Remote.P2P.chainGen addr r u rc gc rs
  where
	go st c cst = Just new
	  where
		new = Remote 
			{ uuid = u
			, cost = cst
			, name = Git.repoDescribe r
			, storeKey = copyToRemote new st
			, retrieveKeyFile = copyFromRemote new st
			, retrieveKeyFileInOrder = pure True
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
			, readonly = Git.repoIsHttp r && not (isP2PHttp' gc)
			, appendonly = False
			, untrustworthy = isJust (remoteAnnexProxiedBy gc) 
				&& (exportTree c || importTree c) 
				&& not (isVersioning c)
			, availability = repoAvail r
			, remotetype = remote
			, mkUnavailable = unavailable r u rc gc rs
			, getInfo = gitRepoInfo new
			, claimUrl = Nothing
			, checkUrl = Nothing
			, remoteStateHandle = rs
			}

defaultRepoCost :: Git.Repo -> Cost
defaultRepoCost r
	| repoCheap r = cheapRemoteCost
	| otherwise = expensiveRemoteCost

unavailable :: Git.Repo -> UUID -> RemoteConfig -> RemoteGitConfig -> RemoteStateHandle -> Annex (Maybe Remote)
unavailable r u c gc = gen r' u c gc'
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
	gc' = gc
		{ remoteAnnexP2PHttpUrl =
			unavailableP2PHttpUrl <$> remoteAnnexP2PHttpUrl gc
		}

{- Tries to read the config for a specified remote, updates state, and
 - returns the updated repo. -}
tryGitConfigRead :: RemoteGitConfig -> Bool -> Git.Repo -> Bool -> Annex Git.Repo
tryGitConfigRead gc autoinit r hasuuid
	| haveconfig r = return r -- already read
	| Git.repoIsSsh r = storeUpdatedRemote $ do
		v <- Ssh.onRemote NoConsumeStdin r
			( pipedconfig Git.Config.ConfigList autoinit (Git.repoDescribe r)
			, error "internal"
			)
			"configlist" [] configlistfields
		case v of
			Right r'
				| haveconfig r' -> return r'
				| otherwise -> do
					configlist_failed
					return r
			Left exitcode -> do
				-- ssh exits 255 when there was an error
				-- connecting to the remote server.
				if exitcode /= ExitFailure 255 
					then do
						configlist_failed
						return r
					else do
						warning $ UnquotedString $ "Unable to connect to repository " ++ Git.repoDescribe r ++ " to get its annex.uuid configuration."
						return r
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
		(r', val, exitcode, _err) <- liftIO $ 
			Git.Config.fromPipe r cmd params st
		if exitcode == ExitSuccess
			then do
				unless (isUUIDConfigured r' || val == mempty || not mustincludeuuuid) $ do
					warning $ UnquotedString $ "Failed to get annex.uuid configuration of repository " ++ Git.repoDescribe r
					warning $ UnquotedString $ "Instead, got: " ++ show val
					warning "This is unexpected; please check the network transport!"
				return $ Right r'
			else do
				warning $ UnquotedString $ "Unable to parse git config from " ++ configloc
				return $ Left exitcode

	geturlconfig = Url.withUrlOptionsPromptingCreds (Just gc) $ \uo -> do
		let url = Git.repoLocation r ++ "/config"
		v <- withTmpFile (literalOsPath "git-annex.tmp") $ \tmpfile h -> do
			liftIO $ hClose h
			Url.download' nullMeterUpdate Nothing url tmpfile uo >>= \case
				Right () ->
					pipedconfig Git.Config.ConfigNullList
						False url "git"
						[ Param "config"
						, Param "--null"
						, Param "--list"
						, Param "--file"
						, File (fromOsPath tmpfile)
						] >>= return . \case
							Right r' -> Right r'
							Left exitcode -> Left $ "git config exited " ++ show exitcode
				Left err -> return (Left err)
		case v of
			Right r' -> do
				-- Cache when http remote is not bare for
				-- optimisation.
				unless (fromMaybe False $ Git.Config.isBare r') $
					setremote setRemoteBare False
				-- When annex.url is set to a P2P http url,
				-- store in remote.name.annexUrl
				case Git.fromConfigValue <$> Git.Config.getMaybe (annexConfig "url") r' of
					Just u | isP2PHttpProtocolUrl u ->
						setremote (setConfig . annexUrlConfigKey) u
					_ -> noop
				return r'
			Left err -> do
				set_ignore "not usable by git-annex" False
				warning $ UnquotedString $ url ++ " " ++ err
				return r

	configlist_failed = set_ignore "does not have git-annex installed" True
	
	set_ignore msg longmessage = do
		case Git.remoteName r of
			Nothing -> noop
			Just n -> do
				warning $ UnquotedString $ "Remote " ++ n ++ " " ++ msg ++ "; setting annex-ignore"
				when longmessage $
					warning $ UnquotedString $ "This could be a problem with the git-annex installation on the remote. Please make sure that git-annex-shell is available in PATH when you ssh into the remote. Once you have fixed the git-annex installation, run: git annex enableremote " ++ n
		setremote setRemoteIgnore True
		setremote setRemoteIgnoreAuto True
	
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
				genUUIDInNameSpace gCryptNameSpace (encodeBS v)

	{- The local repo may not yet be initialized, so try to initialize
	 - it if allowed. However, if that fails, still return the read
	 - git config. -}
	readlocalannexconfig = do
		let check = do
			Annex.BranchState.disableUpdate
			catchNonAsync (autoInitialize noop (pure [])) $ \e ->
				warning $ UnquotedString $ "Remote " ++ Git.repoDescribe r ++
					": "  ++ show e
			Annex.getState Annex.repo
		let r' = r { Git.repoPathSpecifiedExplicitly = True }
		s <- newLocal r'
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
inAnnex' repo rmt st@(State connpool duc _ _ _ _) key
	| isP2PHttp rmt = checkp2phttp
	| Git.repoIsHttp repo = checkhttp
	| Git.repoIsUrl repo = checkremote
	| otherwise = checklocal
  where
	checkp2phttp = p2pHttpClient rmt giveup (clientCheckPresent key)
	checkhttp = do
		gc <- Annex.getGitConfig
		Url.withUrlOptionsPromptingCreds (Just (gitconfig rmt)) $ \uo -> 
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
		| remoteAnnexBare remoteconfig == Just False = 
			annexLocationsNonBare glm gc key
		| otherwise = annexLocationsBare glm gc key
#ifndef mingw32_HOST_OS
	locs' = map fromOsPath locs
#else
	locs' = map (replace "\\" "/" . fromOsPath) locs
#endif
	remoteconfig = gitconfig r
	glm = repoGitLocationMaker repo

dropKey :: Remote -> State -> Maybe SafeDropProof -> Key -> Annex ()
dropKey r st proof key = do
	repo <- getRepo r
	dropKey' repo r st proof key

dropKey' :: Git.Repo -> Remote -> State -> Maybe SafeDropProof -> Key -> Annex ()
dropKey' repo r st@(State connpool duc _ _ _ _) proof key
	| isP2PHttp r = 
		clientRemoveWithProof proof key unabletoremove r >>= \case
			RemoveResultPlus True fanoutuuids ->
				storefanout fanoutuuids
			RemoveResultPlus False fanoutuuids -> do
				storefanout fanoutuuids
				unabletoremove
	| not $ Git.repoIsUrl repo = ifM duc
		( guardUsable repo (giveup "cannot access remote") removelocal
		, giveup "remote does not have expected annex.uuid value"
		)
	| Git.repoIsHttp repo = giveup "dropping from this remote is not supported"
	| otherwise = P2PHelper.remove (uuid r) p2prunner proof key
  where
	p2prunner = Ssh.runProto r connpool (return (Right False, Nothing))

	unabletoremove = giveup "removing content from remote failed"

	-- It could take a long time to eg, automount a drive containing
	-- the repo, so check the proof for expiry again after locking the
	-- content for removal.
	removelocal = do
		proofunexpired <- commitOnCleanup repo r st $ onLocalFast st $ do
			ifM (Annex.Content.inAnnex key)
				( do
					let cleanup = do
						logStatus NoLiveUpdate key InfoMissing
						return True
					Annex.Content.lockContentForRemoval key cleanup $ \lock ->
						ifM (liftIO $ checkSafeDropProofEndTime proof) 
							( do
								Annex.Content.removeAnnex (Annex.getState Annex.remotes) lock
								cleanup
							, return False
							)
				, return True
				)
		unless proofunexpired
			safeDropProofExpired
			
	storefanout = P2PHelper.storeFanout NoLiveUpdate key InfoMissing (uuid r) . map fromB64UUID

lockKey :: Remote -> State -> Key -> (VerifiedCopy -> Annex r) -> Annex r
lockKey r st key callback = do	
	repo <- getRepo r
	lockKey' repo r st key callback

lockKey' :: Git.Repo -> Remote -> State -> Key -> (VerifiedCopy -> Annex r) -> Annex r
lockKey' repo r st@(State connpool duc _ _ _ _) key callback
	| isP2PHttp r = do	
		showLocking r
		p2pHttpClient r giveup (clientLockContent key) >>= \case
			LockResult True (Just lckid) ->
				p2pHttpClient r failedlock $
					clientKeepLocked lckid (uuid r)
						failedlock callback
			_ -> failedlock
	| not $ Git.repoIsUrl repo = ifM duc
		( guardUsable repo failedlock $ do
			inorigrepo <- Annex.makeRunner
			-- Lock content from perspective of remote,
			-- and then run the callback in the original
			-- annex monad, not the remote's.
			onLocalFast st $ 
				Annex.Content.lockContentShared key Nothing $
					liftIO . inorigrepo . callback
		, failedlock
		)
	| Git.repoIsSsh repo = do
		showLocking r
		let withconn = Ssh.withP2PShellConnection r connpool failedlock
		P2PHelper.lock withconn Ssh.runProtoConn (uuid r) key callback
	| otherwise = failedlock
  where
	failedlock = giveup "can't lock content"

{- Tries to copy a key's content from a remote's annex to a file. -}
copyFromRemote :: Remote -> State -> Key -> AssociatedFile -> OsPath -> MeterUpdate -> VerifyConfig -> Annex Verification
copyFromRemote r st key file dest meterupdate vc = do
	repo <- getRepo r
	copyFromRemote'' repo r st key file dest meterupdate vc

copyFromRemote'' :: Git.Repo -> Remote -> State -> Key -> AssociatedFile -> OsPath -> MeterUpdate -> VerifyConfig -> Annex Verification
copyFromRemote'' repo r st@(State connpool _ _ _ _ _) key af dest meterupdate vc
	| isP2PHttp r = copyp2phttp
	| Git.repoIsHttp repo = verifyKeyContentIncrementally vc key $ \iv -> do
		gc <- Annex.getGitConfig
		ok <- Url.withUrlOptionsPromptingCreds (Just (gitconfig r)) $
			Annex.Content.downloadUrl False key meterupdate iv (keyUrls gc repo r key) dest
		unless ok $
			giveup "failed to download content"
	| not $ Git.repoIsUrl repo = guardUsable repo (giveup "cannot access remote") $ do
		u <- getUUID
		hardlink <- wantHardLink
		-- run copy from perspective of remote
		onLocalFast st $ Annex.Content.prepSendAnnex' key Nothing >>= \case
			Just (object, _sz, check) -> do
				let checksuccess = check >>= \case
					Just err -> giveup err
					Nothing -> return True
				copier <- mkFileCopier hardlink st
				(ok, v) <- runTransfer (Transfer Download u (fromKey id key))
					Nothing af Nothing stdRetry $ \p ->
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
			key af dest meterupdate vc
	| otherwise = giveup "copying from this remote is not supported"
  where
	bwlimit = remoteAnnexBwLimitDownload (gitconfig r)
		<|> remoteAnnexBwLimit (gitconfig r)
		
	copyp2phttp = verifyKeyContentIncrementally vc key $ \iv -> do
		startsz <- liftIO $ tryWhenExists $ getFileSize dest
		bracketIO (F.openBinaryFile dest ReadWriteMode) (hClose) $ \h -> do
			metered (Just meterupdate) key bwlimit $ \_ p -> do
				p' <- case startsz of
					Just startsz' -> liftIO $ do
						resumeVerifyFromOffset startsz' iv p h 
					_ -> return p
				let consumer = meteredWrite' p' 
					(writeVerifyChunk iv h)
				p2pHttpClient r giveup (clientGet key af consumer startsz) >>= \case
					Valid -> return ()
					Invalid -> giveup "Transfer failed"

copyFromRemoteCheap :: State -> Git.Repo -> Maybe (Key -> AssociatedFile -> OsPath -> Annex ())
#ifndef mingw32_HOST_OS
copyFromRemoteCheap st repo
	| not $ Git.repoIsUrl repo = Just $ \key _af file -> guardUsable repo (giveup "cannot access remote") $ do
		gc <- getGitConfigFromState st
		loc <- liftIO $ gitAnnexLocation key repo gc
		liftIO $ ifM (doesFileExist loc)
			( do
				absloc <- absPath loc
				R.createSymbolicLink
					(fromOsPath absloc)
					(fromOsPath file)
			, giveup "remote does not contain key"
			)
	| otherwise = Nothing
#else
copyFromRemoteCheap _ _ = Nothing
#endif

{- Tries to copy a key's content to a remote's annex. -}
copyToRemote :: Remote -> State -> Key -> AssociatedFile -> Maybe OsPath -> MeterUpdate -> Annex ()
copyToRemote r st key af o meterupdate = do
	repo <- getRepo r
	copyToRemote' repo r st key af o meterupdate

copyToRemote' :: Git.Repo -> Remote -> State -> Key -> AssociatedFile -> Maybe OsPath -> MeterUpdate -> Annex ()
copyToRemote' repo r st@(State connpool duc _ _ _ _) key af o meterupdate
	| isP2PHttp r = prepsendwith copyp2phttp
	| not $ Git.repoIsUrl repo = ifM duc
		( guardUsable repo (giveup "cannot access remote") $ commitOnCleanup repo r st $
			prepsendwith copylocal
		, giveup "remote does not have expected annex.uuid value"
		)
	| Git.repoIsSsh repo =
		P2PHelper.store (uuid r) (gitconfig r)
			(Ssh.runProto r connpool (return Nothing))
			key af o meterupdate
		
	| otherwise = giveup "copying to this remote is not supported"
  where
	prepsendwith a = Annex.Content.prepSendAnnex' key o >>= \case
		Nothing -> giveup "content not available"
		Just v -> a v
		
	bwlimit = remoteAnnexBwLimitUpload (gitconfig r)
		<|> remoteAnnexBwLimit (gitconfig r)

	failedsend = giveup "failed to send content to remote"

	copylocal (object, sz, check) = do
		-- The check action is going to be run in
		-- the remote's Annex, but it needs access to the local
		-- Annex monad's state.
		checkio <- Annex.withCurrentState check
		u <- getUUID
		hardlink <- wantHardLink
		-- run copy from perspective of remote
		res <- onLocalFast st $ ifM (Annex.Content.inAnnex key)
			( return True
			, runTransfer (Transfer Download u (fromKey id key)) Nothing af Nothing stdRetry $ \p -> do
				let verify = RemoteVerify r
				copier <- mkFileCopier hardlink st
				let rsp = RetrievalAllKeysSecure
				let checksuccess = liftIO checkio >>= \case
					Just err -> giveup err
					Nothing -> return True
				logStatusAfter NoLiveUpdate key $ Annex.Content.getViaTmp rsp verify key (Just sz) $ \dest ->
					metered (Just (combineMeterUpdate meterupdate p)) key bwlimit $ \_ p' -> 
						copier object dest key p' checksuccess verify
			)
		unless res $
			failedsend

	copyp2phttp (object, sz, check) =
		let check' = check >>= \case
			Just s -> do
				warning (UnquotedString s)
				return False
			Nothing -> return True
		in p2pHttpClient r (const $ pure $ PutOffsetResultPlus (Offset 0)) (clientPutOffset key) >>= \case
			PutOffsetResultPlus (offset@(Offset (P2P.Offset n))) ->
				metered (Just meterupdate) key bwlimit $ \_ p -> do
					let p' = offsetMeterUpdate p (BytesProcessed n)
					res <- p2pHttpClient r giveup $
						clientPut p' key (Just offset) af object sz check' False
					case res of
						PutResultPlus False fanoutuuids -> do
							storefanout fanoutuuids
							failedsend
						PutResultPlus True fanoutuuids ->
							storefanout fanoutuuids
			PutOffsetResultAlreadyHavePlus fanoutuuids ->
				storefanout fanoutuuids
	
	storefanout = P2PHelper.storeFanout NoLiveUpdate key InfoPresent (uuid r) . map fromB64UUID

fsckOnRemote :: Git.Repo -> [CommandParam] -> Annex (IO Bool)
fsckOnRemote r params
	| Git.repoIsUrl r = return $ return False
	| otherwise = return $ do
		program <- programPath
		r' <- Git.Config.read r
		environ <- getEnvironment
		let environ' = addEntries 
			[ ("GIT_WORK_TREE", fromOsPath $ Git.repoPath r')
			, ("GIT_DIR", fromOsPath $ Git.localGitDir r')
			] environ
		batchCommandEnv (fromOsPath program)
			(Param "fsck" : params)
			(Just environ')

{- The passed repair action is run in the Annex monad of the remote. -}
repairRemote :: Git.Repo -> Annex Bool -> Annex (IO Bool)
repairRemote r a = return $ do
	s <- Annex.new r
	Annex.eval s $ do
		Annex.BranchState.disableUpdate
		ensureInitialized noop (pure [])
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
onLocal (State _ _ _ _ _ lra) = onLocal' lra

onLocalRepo :: Git.Repo -> Annex a -> Annex a
onLocalRepo repo a = do
	lra <- mkLocalRemoteAnnex repo
	onLocal' lra a

newLocal :: Git.Repo -> Annex (Annex.AnnexState, Annex.AnnexRead)
newLocal repo = do
	(st, rd) <- liftIO $ Annex.new repo
	debugenabled <- Annex.getRead Annex.debugenabled
	debugselector <- Annex.getRead Annex.debugselector
	force <- Annex.getRead Annex.force
	return (st,  rd
		{ Annex.debugenabled = debugenabled
		, Annex.debugselector = debugselector
		, Annex.force = force
		})

onLocal' :: LocalRemoteAnnex -> Annex a -> Annex a
onLocal' (LocalRemoteAnnex repo mv) a = liftIO (takeMVar mv) >>= \case
	[] -> do
		liftIO $ putMVar mv []
		v <- newLocal repo
		go (v, ensureInitialized noop (pure []) >> a)
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

type FileCopier = OsPath -> OsPath -> Key -> MeterUpdate -> Annex Bool -> VerifyConfig -> Annex (Bool, Verification)

-- If either the remote or local repository wants to use hard links,
-- the copier will do so (falling back to copying if a hard link cannot be
-- made).
--
-- When a hard link is created, returns Verified; the repo being linked
-- from is implicitly trusted, so no expensive verification needs to be
-- done. Also returns Verified if the key's content is verified while
-- copying it.
mkFileCopier :: Bool -> State -> Annex FileCopier
mkFileCopier remotewanthardlink (State _ _ copycowtried fastcopy _ _) = do
	localwanthardlink <- wantHardLink
	let linker = \src dest -> R.createLink (fromOsPath src) (fromOsPath dest) >> return True
	if remotewanthardlink || localwanthardlink
		then return $ \src dest k p check verifyconfig ->
			ifM (liftIO (catchBoolIO (linker src dest)))
				( ifM check
					( return (True, Verified)
					, do
						verificationOfContentFailed dest
						return (False, UnVerified)
					)
				, copier src dest k p check verifyconfig
				)
		else return copier
  where
	copier src dest k p check verifyconfig = do
		iv <- startVerifyKeyContentIncrementally verifyconfig k
		liftIO (fileCopier copycowtried fastcopy src dest p iv) >>= \case
			Copied -> ifM check
				( finishVerifyKeyContentIncrementally iv
				, do
					verificationOfContentFailed dest
					return (False, UnVerified)
				)
			CopiedCoW -> unVerified check

{- Normally the UUID of a local repository is checked at startup,
 - but annex-checkuuid config can prevent that. To avoid getting
 - confused, a deferred check is done just before the repository
 - is used.
 - This returns False when the repository UUID is not as expected. -}
type DeferredUUIDCheck = Annex Bool

data State = State Ssh.P2PShellConnectionPool DeferredUUIDCheck CopyCoWTried FastCopy (Annex (Git.Repo, GitConfig)) LocalRemoteAnnex

getRepoFromState :: State -> Annex Git.Repo
getRepoFromState (State _ _ _ _ a _) = fst <$> a

#ifndef mingw32_HOST_OS
{- The config of the remote git repository, cached for speed. -}
getGitConfigFromState :: State -> Annex GitConfig
getGitConfigFromState (State _ _ _ _ a _) = snd <$> a
#endif

mkState :: Git.Repo -> UUID -> RemoteGitConfig -> Annex State
mkState r u gc = do
	pool <- Ssh.mkP2PShellConnectionPool
	copycowtried <- liftIO newCopyCoWTried
	fastcopy <- getFastCopy gc
	lra <- mkLocalRemoteAnnex r
	(duc, getrepo) <- go
	return $ State pool duc copycowtried fastcopy getrepo lra
  where
	go
		| remoteAnnexCheckUUID gc = return
			(return True, return (r, extractGitConfig FromGitConfig r))
		| otherwise = do
			rv <- liftIO newEmptyMVar
			let getrepo = ifM (liftIO $ isEmptyMVar rv)
				( do
					r' <- tryGitConfigRead gc False r True
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

listProxied :: M.Map UUID (S.Set Proxy) -> [Git.Repo] -> Annex [Git.Repo]
listProxied proxies rs = concat <$> mapM go rs
  where
	go r = do
		g <- Annex.gitRepo
		u <- getRepoUUID r
		gc <- Annex.getRemoteGitConfig r
		let cu = fromMaybe u $ remoteAnnexConfigUUID gc
		if not (canproxy gc r) || cu == NoUUID
			then pure []
			else case M.lookup cu proxies of
				Nothing -> pure []
				Just proxied -> catMaybes
					<$> mapM (mkproxied g r gc proxied)
						(S.toList proxied)
	
	proxiedremotename r p = do
		n <- Git.remoteName r
		pure $ n ++ "-" ++ proxyRemoteName p

	mkproxied g r gc proxied p = case proxiedremotename r p of
		Nothing -> pure Nothing
		Just proxyname -> mkproxied' g r gc proxied p proxyname
	
	-- The proxied remote is constructed by renaming the proxy remote,
	-- changing its uuid, and setting the proxied remote's inherited
	-- configs and uuid in Annex state.
	mkproxied' g r gc proxied p proxyname
		| any isconfig (M.keys (Git.config g)) = pure Nothing
		| otherwise = do
			clusters <- getClustersWith id
			-- Not using addGitConfigOverride for inherited
			-- configs, because child git processes do not
			-- need them to be provided with -c.
			Annex.adjustGitRepo (pure . annexconfigadjuster clusters)
			return $ Just $ renamedr
	  where
		renamedr = 
			let c = adduuid configkeyUUID $
				Git.fullconfig r
			in r 
				{ Git.remoteName = Just proxyname
				, Git.config = M.map NE.head c
				, Git.fullconfig = c
				}
		
		annexconfigadjuster clusters r' = 
			let c = adduuid (configRepoUUID renamedr) $
				addurl $
				addproxiedby $
				adjustclusternode clusters $
				inheritconfigs $ Git.fullconfig r'
			in r'
				{ Git.config = M.map NE.head c
				, Git.fullconfig = c
				}

		adduuid ck = M.insert ck $ 
			(Git.ConfigValue $ fromUUID $ proxyRemoteUUID p)
				NE.:| []

		addurl = M.insert (mkRemoteConfigKey renamedr (remoteGitConfigKey UrlField)) $
			(Git.ConfigValue $ encodeBS $ Git.repoLocation r)
				NE.:| []
		
		addproxiedby = case remoteAnnexUUID gc of
			Just u -> addremoteannexfield ProxiedByField
				(Git.ConfigValue $ fromUUID u)
			Nothing -> id
		
		-- A node of a cluster that is being proxied along with
		-- that cluster does not need to be synced with
		-- by default, because syncing with the cluster will
		-- effectively sync with all of its nodes.
		--
		-- Also, give it a slightly higher cost than the
		-- cluster by default, to encourage using the cluster.
		adjustclusternode clusters =
			case M.lookup (ClusterNodeUUID (proxyRemoteUUID p)) (clusterNodeUUIDs clusters) of
				Just cs
					| any (\c -> S.member (fromClusterUUID c) proxieduuids) (S.toList cs) ->
						addremoteannexfield SyncField
							(Git.ConfigValue $ Git.Config.boolConfig' False)
						. addremoteannexfield CostField 
							(Git.ConfigValue $ encodeBS $ show $ defaultRepoCost r + 0.1)
				_ -> id

		proxieduuids = S.map proxyRemoteUUID proxied

		addremoteannexfield f = M.insert
			(mkRemoteConfigKey renamedr (remoteGitConfigKey f)) 
			. (\v -> v NE.:| [])

		inheritconfigs c = foldl' inheritconfig c proxyInheritedFields
		
		inheritconfig c k = case (M.lookup dest c, M.lookup src c) of
			(Nothing, Just v) -> M.insert dest v c
			_ -> c
		  where
			src = mkRemoteConfigKey r k
			dest = mkRemoteConfigKey renamedr k
		
		-- When the git config has anything set for a remote,
		-- avoid making a proxied remote with the same name.
		-- It is possible to set git configs of proxies, but it
		-- needs both the url and uuid config to be manually set.
		isconfig (Git.ConfigKey configkey) = 
			proxyconfigprefix `B.isPrefixOf` configkey
		  where 
			Git.ConfigKey proxyconfigprefix = remoteConfig proxyname mempty

	-- Git remotes that are gcrypt or git-lfs special remotes cannot
	-- proxy. Local git remotes cannot proxy either because
	-- git-annex-shell is not used to access a local git url.
	-- Proxing is also yet supported for remotes using P2P
	-- addresses.
	canproxy gc r
		| isP2PHttp' gc = True
		| remoteAnnexGitLFS gc = False
		| Git.GCrypt.isEncrypted r = False
		| Git.repoIsLocal r || Git.repoIsLocalUnknown r = False
		| otherwise = isNothing (repoP2PAddress r)

isP2PHttp :: Remote -> Bool
isP2PHttp = isP2PHttp' . gitconfig

isP2PHttp' :: RemoteGitConfig -> Bool
isP2PHttp' = isJust . remoteAnnexP2PHttpUrl

