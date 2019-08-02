{- Using git-lfs as a remote.
 -
 - Copyright 2019 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Remote.GitLFS (remote, gen) where

import Annex.Common
import Types.Remote
import Annex.Url
import Types.Creds
import qualified Git
import qualified Git.Types as Git
import qualified Git.Url
import Config
import Config.Cost
import Remote.Helper.Special
import Remote.Helper.ExportImport
import Remote.Helper.Git
import qualified Remote.Helper.Ssh as Ssh
import Annex.Ssh
import Annex.UUID
import Utility.SshHost
import qualified Utility.GitLFS as LFS

import Control.Concurrent.STM
import Data.String
import qualified Data.Map as M
import qualified Network.URI as URI

remote :: RemoteType
remote = RemoteType
	{ typename = "git-lfs"
	-- Remote.Git takes care of enumerating git-lfs remotes too,
	-- and will call our gen on them.
	, enumerate = const (return [])
	, generate = gen
	, setup = mySetup
	, exportSupported = exportUnsupported
	, importSupported = importUnsupported
	}

gen :: Git.Repo -> UUID -> RemoteConfig -> RemoteGitConfig -> Annex (Maybe Remote)
gen r u c gc = do
	handle <- liftIO $ newTVarIO $ LFSHandle Nothing Nothing r gc
	cst <- remoteCost gc expensiveRemoteCost
	return $ Just $ specialRemote' specialcfg c
		(simplyPrepare $ store handle)
		(simplyPrepare $ retrieve handle)
		(simplyPrepare $ remove handle)
		(simplyPrepare $ checkKey handle)
		(this cst)
  where
	this cst = Remote
		{ uuid = u
		, cost = cst
		, name = Git.repoDescribe r
		, storeKey = storeKeyDummy
		, retrieveKeyFile = retreiveKeyFileDummy
		, retrieveKeyFileCheap = retrieveCheap
		-- content stored on git-lfs is hashed with SHA256
		-- no matter what git-annex key it's for, and the hash
		-- is checked on download
		, retrievalSecurityPolicy = RetrievalAllKeysSecure
		, removeKey = removeKeyDummy
		, lockContent = Nothing
		, checkPresent = checkPresentDummy
		, checkPresentCheap = False
		, exportActions = exportUnsupported
		, importActions = importUnsupported
		, whereisKey = Nothing
		, remoteFsck = Nothing
		, repairRepo = Nothing
		, config = c
		, getRepo = return r
		, gitconfig = gc
		, localpath = Nothing
		, remotetype = remote
		, availability = GloballyAvailable
		, readonly = False
		-- content cannot be removed from a git-lfs repo
		, appendonly = True
		, mkUnavailable = return Nothing
		, getInfo = gitRepoInfo (this cst)
		, claimUrl = Nothing
		, checkUrl = Nothing
		}
	specialcfg = (specialRemoteCfg c)
		-- chunking would not improve git-lfs
		{ chunkConfig = NoChunks
		}

mySetup :: SetupStage -> Maybe UUID -> Maybe CredPair -> RemoteConfig -> RemoteGitConfig -> Annex (RemoteConfig, UUID)
mySetup _ mu _ c gc = do
	u <- maybe (liftIO genUUID) return mu

	let repo = fromMaybe (giveup "Specify url=") $
		M.lookup "url" c
	(c', _encsetup) <- encryptionSetup c gc

	-- The repo is not stored in the remote log, because the same
	-- git-lfs repo can be accessed using different urls by different
	-- people (eg over ssh or http).
	--
	-- Instead, set up remote.name.url to point to the repo,
	-- (so it's also usable by git as a non-special remote),
	-- and set remote.name.git-lfs = true
	let c'' = M.delete "repo" c'
	gitConfigSpecialRemote u c'' [("git-lfs", "true")]
	setConfig (ConfigKey ("remote." ++ getRemoteName c ++ ".url")) repo
	return (c'', u)

data LFSHandle = LFSHandle
	{ downloadEndpoint :: Maybe LFS.Endpoint
	, uploadEndpoint :: Maybe LFS.Endpoint
	, remoteRepo :: Git.Repo
	, remoteGitConfig :: RemoteGitConfig
	}

discoverLFSEndpoint :: LFS.TransferRequestOperation -> LFSHandle -> Annex (Maybe LFS.Endpoint)
discoverLFSEndpoint tro h
	| Git.repoIsSsh r = gossh
	| Git.repoIsHttp r = gohttp
	| otherwise = do
		warning "git-lfs endpoint has unsupported URI scheme"
		return Nothing
  where
  	r = remoteRepo h
	lfsrepouri = case Git.location r of
		Git.Url u -> u
		_ -> giveup $ "unsupported git-lfs remote location " ++ Git.repoLocation r
	gohttp = case tro of
		LFS.RequestDownload -> return $ LFS.guessEndpoint lfsrepouri
		LFS.RequestUpload -> do
			-- git-lfs does support storing over http,
			-- but it would need prompting for http basic
			-- authentication each time git-annex discovered
			-- the endpoint.
			warning "Storing content in git-lfs currently needs a ssh repository url, not http."
			return Nothing
	gossh = case mkSshHost <$> Git.Url.hostuser r of
		Nothing -> do
			warning "Unable to parse ssh url for git-lfs remote."
			return Nothing
		Just (Left err) -> do
			warning err
			return Nothing
		Just (Right hostuser) -> do
			let port = Git.Url.port r
			-- Remove leading /~/ from path. That is added when
			-- converting a scp-style repository location with
			-- a relative path into an url, and is legal
			-- according to git-clone(1), but github does not
			-- support it.
			let remotepath = if "/~/" `isPrefixOf` Git.Url.path r
				then drop 3 (Git.Url.path r)
				else Git.Url.path r
			let ps = LFS.sshDiscoverEndpointCommand remotepath tro
			-- Note that no shellEscape is done here, because
			-- at least github's git-lfs implementation does
			-- not allow for shell quoting.
			let remotecmd = unwords ps
			(sshcommand, sshparams) <- sshCommand NoConsumeStdin (hostuser, port) (remoteGitConfig h) remotecmd
			liftIO (tryIO (readProcess sshcommand (toCommand sshparams))) >>= \case
				Left err -> do
					warning $ "ssh connection to git-lfs remote failed: " ++ show err
					return Nothing
				Right resp -> case LFS.parseSshDiscoverEndpointResponse (fromString resp) of
					Nothing -> do
						warning $ "unexpected response from git-lfs remote when doing ssh endpoint discovery"
						return Nothing
					Just endpoint -> return (Just endpoint)

-- The endpoint is cached for later use.
getLFSEndpoint :: LFS.TransferRequestOperation -> TVar LFSHandle -> Annex (Maybe LFS.Endpoint)
getLFSEndpoint tro hv = do
	h <- liftIO $ atomically $ readTVar hv
	case f h of
		Just endpoint -> return (Just endpoint)
		Nothing -> discoverLFSEndpoint tro h >>= \case
			Just endpoint -> do
				liftIO $ atomically $ writeTVar hv $
					case tro of
						LFS.RequestDownload -> 
							h { downloadEndpoint = Just endpoint }
						LFS.RequestUpload ->
							h { uploadEndpoint = Just endpoint }
				return (Just endpoint)
			Nothing -> return Nothing
  where
	f = case tro of
		LFS.RequestDownload -> downloadEndpoint
		LFS.RequestUpload -> uploadEndpoint

store :: TVar LFSHandle -> Storer
store h = fileStorer $ \k src p -> getLFSEndpoint LFS.RequestUpload h >>= \case
	Nothing -> return False
	Just endpoint -> do
		liftIO $ print ("endpoint", endpoint)
		return False

retrieve :: TVar LFSHandle -> Retriever
retrieve h = byteRetriever $ \k sink -> getLFSEndpoint LFS.RequestDownload h >>= \case
	Nothing -> return False
	Just endpoint -> do
		liftIO $ print ("endpoint", endpoint)
		return False

checkKey :: TVar LFSHandle -> CheckPresent
checkKey h key = getLFSEndpoint LFS.RequestDownload h >>= \case
	Nothing -> giveup "unable to connect to git-lfs endpoint"
	Just endpoint -> do
		liftIO $ print ("endpoint", endpoint)
		return False

retrieveCheap :: Key -> AssociatedFile -> FilePath -> Annex Bool
retrieveCheap _ _ _ = return False

remove :: TVar LFSHandle -> Remover
remove h key = do
	warning "git-lfs does not support removing content"
	return False
