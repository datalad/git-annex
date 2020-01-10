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
import Remote.Helper.ExportImport
import Annex.Ssh
import Annex.UUID
import Utility.SshHost
import Types.ProposedAccepted

data DdarRepo = DdarRepo
	{ ddarRepoConfig :: RemoteGitConfig
	, ddarRepoLocation :: String
	}

remote :: RemoteType
remote = RemoteType
	{ typename = "ddar"
	, enumerate = const (findSpecialRemotes "ddarrepo")
	, generate = gen
	, setup = ddarSetup
	, exportSupported = exportUnsupported
	, importSupported = importUnsupported
	}

gen :: Git.Repo -> UUID -> RemoteConfig -> RemoteGitConfig -> RemoteStateHandle -> Annex (Maybe Remote)
gen r u c gc rs = do
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
		-- ddar communicates over ssh, not subject to http redirect
		-- type attacks
		, retrievalSecurityPolicy = RetrievalAllKeysSecure
		, removeKey = removeKeyDummy
		, lockContent = Nothing
		, checkPresent = checkPresentDummy
		, checkPresentCheap = ddarLocal ddarrepo
		, exportActions = exportUnsupported
		, importActions = importUnsupported
		, whereisKey = Nothing
		, remoteFsck = Nothing
		, repairRepo = Nothing
		, config = c
		, getRepo = return r
		, gitconfig = gc
		, localpath = if ddarLocal ddarrepo && not (null $ ddarRepoLocation ddarrepo)
			then Just $ ddarRepoLocation ddarrepo
			else Nothing
		, remotetype = remote
		, availability = if ddarLocal ddarrepo then LocallyAvailable else GloballyAvailable
		, readonly = False
		, appendonly = False
		, mkUnavailable = return Nothing
		, getInfo = return [("repo", ddarRepoLocation ddarrepo)]
		, claimUrl = Nothing
		, checkUrl = Nothing
		, remoteStateHandle = rs
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
	let ddarrepo = maybe (giveup "Specify ddarrepo=") fromProposedAccepted $
		M.lookup (Accepted "ddarrepo") c
	(c', _encsetup) <- encryptionSetup c gc

	-- The ddarrepo is stored in git config, as well as this repo's
	-- persistant state, so it can vary between hosts.
	gitConfigSpecialRemote u c' [("ddarrepo", ddarrepo)]

	return (c', u)

store :: DdarRepo -> Storer
store ddarrepo = fileStorer $ \k src _p -> do
	let params =
		[ Param "c"
		, Param "-N"
		, Param $ serializeKey k
		, Param $ ddarRepoLocation ddarrepo
		, File src
		]
	liftIO $ boolSystem "ddar" params

{- Convert remote DdarRepo to host and path on remote end -}
splitRemoteDdarRepo :: DdarRepo -> (SshHost, String)
splitRemoteDdarRepo ddarrepo = (either error id $ mkSshHost host, ddarrepo')
  where
	(host, remainder) = span (/= ':') (ddarRepoLocation ddarrepo)
	ddarrepo' = drop 1 remainder

{- Return the command and parameters to use for a ddar call that may need to be
 - made on a remote repository. This will call ssh if needed. -}
ddarRemoteCall :: ConsumeStdin -> DdarRepo -> Char -> [CommandParam] -> Annex (String, [CommandParam])
ddarRemoteCall cs ddarrepo cmd params
	| ddarLocal ddarrepo = return ("ddar", localParams)
	| otherwise = sshCommand cs (host, Nothing) (ddarRepoConfig ddarrepo) remoteCommand
  where
	(host, ddarrepo') = splitRemoteDdarRepo ddarrepo
	localParams = Param [cmd] : Param (ddarRepoLocation ddarrepo) : params
	remoteCommand = unwords $ map shellEscape $ toCommand $
		[Param "ddar", Param [cmd], Param ddarrepo'] ++ params

{- Specialized ddarRemoteCall that includes extraction command and flags -}
ddarExtractRemoteCall :: ConsumeStdin -> DdarRepo -> Key -> Annex (String, [CommandParam])
ddarExtractRemoteCall cs ddarrepo k =
	ddarRemoteCall cs ddarrepo 'x' [Param "--force-stdout", Param $ serializeKey k]

retrieve :: DdarRepo -> Retriever
retrieve ddarrepo = byteRetriever $ \k sink -> do
	(cmd, params) <- ddarExtractRemoteCall NoConsumeStdin ddarrepo k
	let p = (proc cmd $ toCommand params) { std_out = CreatePipe }
	(_, Just h, _, pid) <- liftIO $ createProcess p
	liftIO (hClose h >> forceSuccessProcess p pid)
		`after` (sink =<< liftIO (L.hGetContents h))

retrieveCheap :: Key -> AssociatedFile -> FilePath -> Annex Bool
retrieveCheap _ _ _ = return False

remove :: DdarRepo -> Remover
remove ddarrepo key = do
	(cmd, params) <- ddarRemoteCall NoConsumeStdin ddarrepo 'd'
		[Param $ serializeKey key]
	liftIO $ boolSystem cmd params

ddarDirectoryExists :: DdarRepo -> Annex (Either String Bool)
ddarDirectoryExists ddarrepo
	| ddarLocal ddarrepo = do
		maybeStatus <- liftIO $ tryJust (guard . isDoesNotExistError) $ getFileStatus $ ddarRepoLocation ddarrepo
		return $ case maybeStatus of
			Left _ -> Right False
			Right status -> Right $ isDirectory status
	| otherwise = do
		let remotecmd = unwords $ map shellEscape
			[ "test", "-d", ddarrepo' ]
		(sshcmd, sshps) <- sshCommand NoConsumeStdin (host, Nothing)
			(ddarRepoConfig ddarrepo) remotecmd
		exitCode <- liftIO $ safeSystem sshcmd sshps
		case exitCode of
			ExitSuccess -> return $ Right True
			ExitFailure 1 -> return $ Right False
			ExitFailure code -> return $ Left $ "ssh " ++
				show (unwords $ toCommand sshps) ++
				" failed with status " ++ show code
  where
	(host, ddarrepo') = splitRemoteDdarRepo ddarrepo

{- Use "ddar t" to determine if a given key is present in a ddar archive -}
inDdarManifest :: DdarRepo -> Key -> Annex (Either String Bool)
inDdarManifest ddarrepo k = do
	(cmd, params) <- ddarRemoteCall NoConsumeStdin ddarrepo 't' []
	let p = proc cmd $ toCommand params
	liftIO $ catchMsgIO $ withHandle StdoutHandle createProcessSuccess p $ \h -> do
		contents <- hGetContents h
		return $ elem k' $ lines contents
  where
	k' = serializeKey k

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
