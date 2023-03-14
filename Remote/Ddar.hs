{- Using ddar as a remote. Based on bup and rsync remotes.
 -
 - Copyright 2011-2020 Joey Hess <id@joeyh.name>
 - Copyright 2014 Robie Basak <robie@justgohome.co.uk>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}

module Remote.Ddar (remote) where

import qualified Data.Map as M
import qualified Data.ByteString.Lazy as L
import System.IO.Error
import System.PosixCompat.Files (isDirectory)

import Annex.Common
import Types.Remote
import Types.Creds
import qualified Git
import Config
import Config.Cost
import Annex.SpecialRemote.Config
import Remote.Helper.Special
import Remote.Helper.ExportImport
import Annex.Ssh
import Annex.UUID
import Utility.SshHost
import Types.ProposedAccepted
import qualified Utility.RawFilePath as R

data DdarRepo = DdarRepo
	{ ddarRepoConfig :: RemoteGitConfig
	, ddarRepoLocation :: String
	}

remote :: RemoteType
remote = specialRemoteType $ RemoteType
	{ typename = "ddar"
	, enumerate = const (findSpecialRemotes "ddarrepo")
	, generate = gen
	, configParser = mkRemoteConfigParser
		[ optionalStringParser ddarrepoField
			(FieldDesc "(required) location of ddar archive to use")
		]
	, setup = ddarSetup
	, exportSupported = exportUnsupported
	, importSupported = importUnsupported
	, thirdPartyPopulated = False
	}

ddarrepoField :: RemoteConfigField
ddarrepoField = Accepted "ddarrepo"

gen :: Git.Repo -> UUID -> RemoteConfig -> RemoteGitConfig -> RemoteStateHandle -> Annex (Maybe Remote)
gen r u rc gc rs = do
	c <- parsedRemoteConfig remote rc
	cst <- remoteCost gc c $
		if ddarLocal ddarrepo
			then nearlyCheapRemoteCost
			else expensiveRemoteCost
	let specialcfg = (specialRemoteCfg c)
		-- chunking would not improve ddar
		{ chunkConfig = NoChunks
		}
	return $ Just $ specialRemote' specialcfg c
		(store ddarrepo)
		(retrieve ddarrepo)
		(remove ddarrepo)
		(checkKey ddarrepo)
		(this c cst)
  where
	this c cst = Remote
		{ uuid = u
		, cost = cst
		, name = Git.repoDescribe r
		, storeKey = storeKeyDummy
		, retrieveKeyFile = retrieveKeyFileDummy
		, retrieveKeyFileCheap = Nothing
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
		, untrustworthy = False
		, mkUnavailable = return Nothing
		, getInfo = return [("repo", ddarRepoLocation ddarrepo)]
		, claimUrl = Nothing
		, checkUrl = Nothing
		, remoteStateHandle = rs
		}
	ddarrepo = maybe (giveup "missing ddarrepo") (DdarRepo gc) (remoteAnnexDdarRepo gc)

ddarSetup :: SetupStage -> Maybe UUID -> Maybe CredPair -> RemoteConfig -> RemoteGitConfig -> Annex (RemoteConfig, UUID)
ddarSetup _ mu _ c gc = do
	u <- maybe (liftIO genUUID) return mu

	-- verify configuration is sane
	let ddarrepo = maybe (giveup "Specify ddarrepo=") fromProposedAccepted $
		M.lookup ddarrepoField c
	(c', _encsetup) <- encryptionSetup c gc

	-- The ddarrepo is stored in git config, as well as this repo's
	-- persistent state, so it can vary between hosts.
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
	unlessM (liftIO $ boolSystem "ddar" params) $
		giveup "ddar failed"

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
	let p = (proc cmd $ toCommand params)
		{ std_out = CreatePipe }
	bracketIO (createProcess p) cleanupProcess (go sink p)
  where
	go sink p (_, Just h, _, pid) = do
		r <- sink =<< liftIO (L.hGetContents h)
		liftIO $ do
			hClose h
			forceSuccessProcess p pid
		return r
	go _ _ _ = error "internal"

remove :: DdarRepo -> Remover
remove ddarrepo key = do
	(cmd, params) <- ddarRemoteCall NoConsumeStdin ddarrepo 'd'
		[Param $ serializeKey key]
	unlessM (liftIO $ boolSystem cmd params) $
		giveup "ddar failed to remove"

ddarDirectoryExists :: DdarRepo -> Annex (Either String Bool)
ddarDirectoryExists ddarrepo
	| ddarLocal ddarrepo = do
		maybeStatus <- liftIO $ tryJust (guard . isDoesNotExistError) $
			R.getSymbolicLinkStatus $ toRawFilePath $ ddarRepoLocation ddarrepo
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
	let p = (proc cmd $ toCommand params)
		{ std_out = CreatePipe }
	liftIO $ catchMsgIO $ withCreateProcess p (go p)
  where
	k' = serializeKey k
	
	go p _ (Just hout) _ pid = do
		contents <- hGetContents hout
		let !r = elem k' (lines contents)
		forceSuccessProcess p pid
		return r
	go _ _ _ _ _ = error "internal"

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
