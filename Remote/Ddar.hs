{- Using ddar as a remote. Based on bup and rsync remotes.
 -
 - Copyright 2011 Joey Hess <id@joeyh.name>
 - Copyright 2014 Robie Basak <robie@justgohome.co.uk>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Remote.Ddar (remote) where

import qualified Data.Map as M
import qualified Data.ByteString.Lazy as L
import System.IO.Error

import Annex.Common
import Types.Remote
import Types.Creds
import qualified Git
import Config
import Config.Cost
import Remote.Helper.Special
import Annex.Ssh
import Annex.UUID

data DdarRepo = DdarRepo
	{ ddarRepoConfig :: RemoteGitConfig
	, ddarRepoLocation :: String
	}

remote :: RemoteType
remote = RemoteType {
	typename = "ddar",
	enumerate = const (findSpecialRemotes "ddarrepo"),
	generate = gen,
	setup = ddarSetup
}

gen :: Git.Repo -> UUID -> RemoteConfig -> RemoteGitConfig -> Annex (Maybe Remote)
gen r u c gc = do
	cst <- remoteCost gc $
		if ddarLocal ddarrepo
			then nearlyCheapRemoteCost
			else expensiveRemoteCost
	return $ Just $ specialRemote' specialcfg c
		(simplyPrepare $ store ddarrepo)
		(simplyPrepare $ retrieve ddarrepo)
		(simplyPrepare $ remove ddarrepo)
		(simplyPrepare $ checkKey ddarrepo)
		(this cst)
  where
	this cst = Remote
		{ uuid = u
		, cost = cst
		, name = Git.repoDescribe r
		, storeKey = storeKeyDummy
		, retrieveKeyFile = retreiveKeyFileDummy
		, retrieveKeyFileCheap = retrieveCheap
		, removeKey = removeKeyDummy
		, lockContent = Nothing
		, checkPresent = checkPresentDummy
		, checkPresentCheap = ddarLocal ddarrepo
		, whereisKey = Nothing
		, remoteFsck = Nothing
		, repairRepo = Nothing
		, config = c
		, repo = r
		, gitconfig = gc
		, localpath = if ddarLocal ddarrepo && not (null $ ddarRepoLocation ddarrepo)
			then Just $ ddarRepoLocation ddarrepo
			else Nothing
		, remotetype = remote
		, availability = if ddarLocal ddarrepo then LocallyAvailable else GloballyAvailable
		, readonly = False
		, mkUnavailable = return Nothing
		, getInfo = return [("repo", ddarRepoLocation ddarrepo)]
		, claimUrl = Nothing
		, checkUrl = Nothing
		}
	ddarrepo = maybe (giveup "missing ddarrepo") (DdarRepo gc) (remoteAnnexDdarRepo gc)
	specialcfg = (specialRemoteCfg c)
		-- chunking would not improve ddar
		{ chunkConfig = NoChunks
		}

ddarSetup :: SetupStage -> Maybe UUID -> Maybe CredPair -> RemoteConfig -> RemoteGitConfig -> Annex (RemoteConfig, UUID)
ddarSetup _ mu _ c gc = do
	u <- maybe (liftIO genUUID) return mu

	-- verify configuration is sane
	let ddarrepo = fromMaybe (giveup "Specify ddarrepo=") $
		M.lookup "ddarrepo" c
	(c', _encsetup) <- encryptionSetup c gc

	-- The ddarrepo is stored in git config, as well as this repo's
	-- persistant state, so it can vary between hosts.
	gitConfigSpecialRemote u c' "ddarrepo" ddarrepo

	return (c', u)

store :: DdarRepo -> Storer
store ddarrepo = fileStorer $ \k src _p -> do
	let params =
		[ Param "c"
		, Param "-N"
		, Param $ key2file k
		, Param $ ddarRepoLocation ddarrepo
		, File src
		]
	liftIO $ boolSystem "ddar" params

{- Convert remote DdarRepo to host and path on remote end -}
splitRemoteDdarRepo :: DdarRepo -> (String, String)
splitRemoteDdarRepo ddarrepo =
	(host, ddarrepo')
  where
	(host, remainder) = span (/= ':') (ddarRepoLocation ddarrepo)
	ddarrepo' = drop 1 remainder

{- Return the command and parameters to use for a ddar call that may need to be
 - made on a remote repository. This will call ssh if needed. -}
ddarRemoteCall :: DdarRepo -> Char -> [CommandParam] -> Annex (String, [CommandParam])
ddarRemoteCall ddarrepo cmd params
	| ddarLocal ddarrepo = return ("ddar", localParams)
	| otherwise = do
		os <- sshOptions (host, Nothing) (ddarRepoConfig ddarrepo) []
		return ("ssh", os ++ remoteParams)
  where
	(host, ddarrepo') = splitRemoteDdarRepo ddarrepo
	localParams = Param [cmd] : Param (ddarRepoLocation ddarrepo) : params
	remoteParams = Param host : Param "ddar" : Param [cmd] : Param ddarrepo' : params

{- Specialized ddarRemoteCall that includes extraction command and flags -}
ddarExtractRemoteCall :: DdarRepo -> Key -> Annex (String, [CommandParam])
ddarExtractRemoteCall ddarrepo k =
	ddarRemoteCall ddarrepo 'x' [Param "--force-stdout", Param $ key2file k]

retrieve :: DdarRepo -> Retriever
retrieve ddarrepo = byteRetriever $ \k sink -> do
	(cmd, params) <- ddarExtractRemoteCall ddarrepo k
	let p = (proc cmd $ toCommand params) { std_out = CreatePipe }
	(_, Just h, _, pid) <- liftIO $ createProcess p
	liftIO (hClose h >> forceSuccessProcess p pid)
		`after` (sink =<< liftIO (L.hGetContents h))

retrieveCheap :: Key -> AssociatedFile -> FilePath -> Annex Bool
retrieveCheap _ _ _ = return False

remove :: DdarRepo -> Remover
remove ddarrepo key = do
	(cmd, params) <- ddarRemoteCall ddarrepo 'd' [Param $ key2file key]
	liftIO $ boolSystem cmd params

ddarDirectoryExists :: DdarRepo -> Annex (Either String Bool)
ddarDirectoryExists ddarrepo
	| ddarLocal ddarrepo = do
		maybeStatus <- liftIO $ tryJust (guard . isDoesNotExistError) $ getFileStatus $ ddarRepoLocation ddarrepo
		return $ case maybeStatus of
			Left _ -> Right False
			Right status -> Right $ isDirectory status
	| otherwise = do
		ps <- sshOptions (host, Nothing) (ddarRepoConfig ddarrepo) []
		exitCode <- liftIO $ safeSystem "ssh" (ps ++ params)
		case exitCode of
			ExitSuccess -> return $ Right True
			ExitFailure 1 -> return $ Right False
			ExitFailure code -> return $ Left $ "ssh call " ++
				show (unwords $ toCommand params) ++
				" failed with status " ++ show code
  where
	(host, ddarrepo') = splitRemoteDdarRepo ddarrepo
	params =
		[ Param host
		, Param "test"
		, Param "-d"
		, Param ddarrepo'
		]

{- Use "ddar t" to determine if a given key is present in a ddar archive -}
inDdarManifest :: DdarRepo -> Key -> Annex (Either String Bool)
inDdarManifest ddarrepo k = do
	(cmd, params) <- ddarRemoteCall ddarrepo 't' []
	let p = proc cmd $ toCommand params
	liftIO $ catchMsgIO $ withHandle StdoutHandle createProcessSuccess p $ \h -> do
		contents <- hGetContents h
		return $ elem k' $ lines contents
  where
	k' = key2file k

checkKey :: DdarRepo -> CheckPresent
checkKey ddarrepo key = do
	directoryExists <- ddarDirectoryExists ddarrepo
	case directoryExists of
		Left e -> error e
		Right True -> either error return
			=<< inDdarManifest ddarrepo key
		Right False -> return False

ddarLocal :: DdarRepo -> Bool
ddarLocal = notElem ':' . ddarRepoLocation
