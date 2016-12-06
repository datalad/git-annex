{- git remotes using the git-annex P2P protocol
 -
 - Copyright 2016 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Remote.P2P (
	remote,
	chainGen
) where

import Annex.Common
import P2P.Address
import Types.Remote
import Types.GitConfig
import qualified Git
import Config
import Config.Cost
import Remote.Helper.Git
import Remote.Helper.Special

remote :: RemoteType
remote = RemoteType {
	typename = "p2p",
	-- Remote.Git takes care of enumerating P2P remotes,
	-- and will call chainGen on them.
	enumerate = const (return []),
	generate = \_ _ _ _ -> return Nothing,
	setup = error "P2P remotes are set up using git-annex p2p"
}

chainGen :: P2PAddress -> Git.Repo -> UUID -> RemoteConfig -> RemoteGitConfig -> Annex (Maybe Remote)
chainGen addr r u c gc = do
	workerpool <- mkWorkerPool addr
	cst <- remoteCost gc expensiveRemoteCost
	let this = Remote 
		{ uuid = u
		, cost = cst
		, name = Git.repoDescribe r
		, storeKey = storeKeyDummy
		, retrieveKeyFile = retreiveKeyFileDummy
		, retrieveKeyFileCheap = \_ _ _ -> return False
		, removeKey = removeKeyDummy
		, lockContent = Nothing -- TODO use p2p protocol locking
		, checkPresent = checkPresentDummy
		, checkPresentCheap = False
		, whereisKey = Nothing
		, remoteFsck = Nothing
		, repairRepo = Nothing
		, config = c
		, localpath = Nothing
		, repo = r
		, gitconfig = gc { remoteGitConfig = Just $ extractGitConfig r }
		, readonly = False
		, availability = GloballyAvailable
		, remotetype = remote
		, mkUnavailable = return Nothing
		, getInfo = gitRepoInfo this
		, claimUrl = Nothing
		, checkUrl = Nothing
	}
	return $ Just $ specialRemote' (specialRemoteCfg c) c
		(simplyPrepare $ store this workerpool)
		(simplyPrepare $ retrieve this workerpool)
		(simplyPrepare $ remove this workerpool)
		(simplyPrepare $ checkKey this workerpool)
		this

data WorkerPool = WorkerPool

mkWorkerPool :: P2PAddress -> Annex WorkerPool
mkWorkerPool addr = undefined

store :: Remote -> WorkerPool -> Storer
store r workerpool = undefined

retrieve :: Remote -> WorkerPool -> Retriever
retrieve r workerpool = undefined

remove :: Remote -> WorkerPool -> Remover
remove r workerpool k = undefined

checkKey :: Remote -> WorkerPool -> CheckPresent
checkKey r workerpool k = undefined
