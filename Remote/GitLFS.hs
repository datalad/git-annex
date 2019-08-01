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
import Config
import Config.Cost
import Remote.Helper.Special
import Remote.Helper.ExportImport
import Remote.Helper.Git
import Annex.Ssh
import Annex.UUID
import Utility.SshHost
import qualified Utility.GitLFS as LFS

import Control.Concurrent.STM
import qualified Data.Map as M

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

type LFSHandle = TVar (String, Maybe LFS.Endpoint)

gen :: Git.Repo -> UUID -> RemoteConfig -> RemoteGitConfig -> Annex (Maybe Remote)
gen r u c gc = do
	handle <- liftIO $ newTVarIO (lfsrepo, Nothing)
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
	lfsrepo = fromMaybe
		(giveup "remote url is not configured")
		(M.lookup "url" $ Git.config r)
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

store :: LFSHandle -> Storer
store h = fileStorer $ \k src p -> undefined

retrieve :: LFSHandle -> Retriever
retrieve h = byteRetriever $ \k sink -> undefined

retrieveCheap :: Key -> AssociatedFile -> FilePath -> Annex Bool
retrieveCheap _ _ _ = return False

checkKey :: LFSHandle -> CheckPresent
checkKey h key = undefined

remove :: LFSHandle -> Remover
remove h key = do
	warning "git-lfs does not support removing content"
	return False
