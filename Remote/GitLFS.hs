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
import Types.Key
import Types.Creds
import qualified Annex
import qualified Git
import qualified Git.Types as Git
import qualified Git.Url
import qualified Git.GCrypt
import qualified Git.Credential as Git
import Config
import Config.Cost
import Remote.Helper.Special
import Remote.Helper.ExportImport
import Remote.Helper.Git
import Remote.Helper.Http
import qualified Remote.GCrypt
import Annex.Ssh
import Annex.UUID
import Crypto
import Backend.Hash
import Utility.Hash
import Utility.SshHost
import Logs.RemoteState
import qualified Utility.GitLFS as LFS

import Control.Concurrent.STM
import Data.String
import Network.HTTP.Types
import Network.HTTP.Client hiding (port)
import System.Log.Logger
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Control.Concurrent.MSemN as MSemN

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

gen :: Git.Repo -> UUID -> RemoteConfig -> RemoteGitConfig -> RemoteStateHandle -> Annex (Maybe Remote)
gen r u c gc rs = do
	-- If the repo uses gcrypt, get the underlaying repo without the
	-- gcrypt url, to do LFS endpoint discovery on.
	r' <- if Git.GCrypt.isEncrypted r
		then do
			g <- Annex.gitRepo
			liftIO $ Git.GCrypt.encryptedRemote g r
		else pure r
	sem <- liftIO $ MSemN.new 1
	h <- liftIO $ newTVarIO $ LFSHandle Nothing Nothing sem r' gc
	cst <- remoteCost gc expensiveRemoteCost
	return $ Just $ specialRemote' specialcfg c
		(simplyPrepare $ store rs h)
		(simplyPrepare $ retrieve rs h)
		(simplyPrepare $ remove h)
		(simplyPrepare $ checkKey rs h)
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
		, remoteStateHandle = rs
		}
	specialcfg = (specialRemoteCfg c)
		-- chunking would not improve git-lfs
		{ chunkConfig = NoChunks
		}

mySetup :: SetupStage -> Maybe UUID -> Maybe CredPair -> RemoteConfig -> RemoteGitConfig -> Annex (RemoteConfig, UUID)
mySetup _ mu _ c gc = do
	u <- maybe (liftIO genUUID) return mu

	(c', _encsetup) <- encryptionSetup c gc
	case (isEncrypted c', Git.GCrypt.urlPrefix `isPrefixOf` url) of
		(False, False) -> noop
		(True, True) -> Remote.GCrypt.setGcryptEncryption c' remotename
		(True, False) -> unlessM (Annex.getState Annex.force) $
			giveup $ unwords $
				[ "Encryption is enabled for this remote,"
				, "but only the files that git-annex stores on"
				, "it would be encrypted; "
				, "anything that git push sends to it would"
				, "not be encrypted. Recommend prefixing the"
				, "url with \"gcrypt::\" to also encrypt"
				, "git pushes."
				, "(Use --force if you want to use this"
				, "likely insecure configuration.)"
				]
		(False, True) -> unlessM (Annex.getState Annex.force) $
			giveup $ unwords $
				[ "You used a \"gcrypt::\" url for this remote,"
				, "but encryption=none prevents git-annex"
				, "from encrypting files it stores there."
				, "(Use --force if you want to use this"
				, "likely insecure configuration.)"
				]

	-- The url is not stored in the remote log, because the same
	-- git-lfs repo can be accessed using different urls by different
	-- people (eg over ssh or http).
	--
	-- Instead, set up remote.name.url to point to the repo,
	-- (so it's also usable by git as a non-special remote),
	-- and set remote.name.git-lfs = true
	let c'' = M.delete "url" c'
	gitConfigSpecialRemote u c'' [("git-lfs", "true")]
	setConfig (ConfigKey ("remote." ++ getRemoteName c ++ ".url")) url
	return (c'', u)
  where
	url = fromMaybe (giveup "Specify url=") (M.lookup "url" c)
	remotename = fromJust (lookupName c)

data LFSHandle = LFSHandle
	{ downloadEndpoint :: Maybe LFS.Endpoint
	, uploadEndpoint :: Maybe LFS.Endpoint
	, getEndPointLock :: MSemN.MSemN Int 
	, remoteRepo :: Git.Repo
	, remoteGitConfig :: RemoteGitConfig
	}

-- Only let one thread at a time do endpoint discovery.
withEndPointLock :: LFSHandle -> Annex a -> Annex a
withEndPointLock h = bracket_
	(liftIO $ MSemN.wait l 1)
	(liftIO $ MSemN.signal l 1)
  where
	l = getEndPointLock h

discoverLFSEndpoint :: LFS.TransferRequestOperation -> LFSHandle -> Annex (Maybe LFS.Endpoint)
discoverLFSEndpoint tro h
	| Git.repoIsSsh r = gossh
	| Git.repoIsHttp r = gohttp
	| otherwise = unsupportedurischeme
  where
  	r = remoteRepo h
	lfsrepouri = case Git.location r of
		Git.Url u -> u
		_ -> giveup $ "unsupported git-lfs remote location " ++ Git.repoLocation r
	
	unsupportedurischeme = do
		warning "git-lfs endpoint has unsupported URI scheme"
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
	
	-- The endpoint may or may not need http basic authentication,
	-- which involves using git-credential to prompt for the password.
	--
	-- To determine if it does, make a download or upload request to
	-- it, not including any objects in the request, and see if
	-- the server requests authentication.
	gohttp = case LFS.guessEndpoint lfsrepouri of
		Nothing -> unsupportedurischeme
		Just endpoint -> do
			let testreq = LFS.startTransferRequest endpoint transfernothing
			flip catchNonAsync (const (returnendpoint endpoint)) $ do
				resp <- makeSmallAPIRequest testreq
				if needauth (responseStatus resp)
					then do
						cred <- prompt $ inRepo $ Git.getUrlCredential (show lfsrepouri)
						let endpoint' = addbasicauth cred endpoint
						let testreq' = LFS.startTransferRequest endpoint' transfernothing
						flip catchNonAsync (const (returnendpoint endpoint')) $ do
						resp' <- makeSmallAPIRequest testreq'
						inRepo $ if needauth (responseStatus resp')
							then Git.rejectUrlCredential cred
							else Git.approveUrlCredential cred
						returnendpoint endpoint'
					else returnendpoint endpoint
	  where
	  	transfernothing = LFS.TransferRequest
			{ LFS.req_operation = tro
			, LFS.req_transfers = [LFS.Basic]
			, LFS.req_ref = Nothing
			, LFS.req_objects = []
			}
		returnendpoint = return . Just

		needauth status = status == unauthorized401

		addbasicauth cred endpoint =
			case (Git.credentialUsername cred, Git.credentialPassword cred) of
				(Just u, Just p) ->
					LFS.modifyEndpointRequest endpoint $
						applyBasicAuth (encodeBS u) (encodeBS p)
				_ -> endpoint

-- The endpoint is cached for later use.
getLFSEndpoint :: LFS.TransferRequestOperation -> TVar LFSHandle -> Annex (Maybe LFS.Endpoint)
getLFSEndpoint tro hv = do
	h <- liftIO $ atomically $ readTVar hv
	case f h of
		Just endpoint -> return (Just endpoint)
		Nothing -> withEndPointLock h $ discoverLFSEndpoint tro h >>= \case
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

-- Make an API request that is expected to have a small response body.
-- Not for use in downloading an object.
makeSmallAPIRequest :: Request -> Annex (Response L.ByteString)
makeSmallAPIRequest req = do
	uo <- getUrlOptions
	let req' = applyRequest uo req
	liftIO $ debugM "git-lfs" (show req')
	resp <- liftIO $ httpLbs req' (httpManager uo)
	-- Only debug the http status code, not the json
	-- which may include an authentication token.
	liftIO $ debugM "git-lfs" (show $ responseStatus resp)
	return resp

sendTransferRequest
	:: LFS.IsTransferResponseOperation op
	=> LFS.TransferRequest
	-> LFS.Endpoint
	-> Annex (Either String (LFS.TransferResponse op))
sendTransferRequest req endpoint = do
	let httpreq = LFS.startTransferRequest endpoint req
	httpresp <- makeSmallAPIRequest $ setRequestCheckStatus httpreq
	return $ case LFS.parseTransferResponse (responseBody httpresp) of
		LFS.ParsedTransferResponse resp -> Right resp
		LFS.ParsedTransferResponseError tro -> Left $
			T.unpack $ LFS.resperr_message tro
		LFS.ParseFailed err -> Left err

extractKeySha256 :: Key -> Maybe LFS.SHA256
extractKeySha256 k = case keyVariety k of
	SHA2Key (HashSize 256) (HasExt hasext)
		| hasext -> eitherToMaybe $ E.decodeUtf8' (keyHash k)
		| otherwise -> eitherToMaybe $ E.decodeUtf8' (keyName k)
	_ -> Nothing

-- The size of an encrypted key is the size of the input data, but we need
-- the actual object size.
extractKeySize :: Key -> Maybe Integer
extractKeySize k
	| isEncKey k = Nothing
	| otherwise = keySize k

mkUploadRequest :: RemoteStateHandle -> Key -> FilePath -> Annex (LFS.TransferRequest, LFS.SHA256, Integer)
mkUploadRequest rs k content = case (extractKeySha256 k, extractKeySize k) of
	(Just sha256, Just size) ->
		ret sha256 size
	(_, Just size) -> do
		sha256 <- calcsha256
		remembersha256 sha256
		ret sha256 size
	_ -> do
		sha256 <- calcsha256
		size <- liftIO $ getFileSize content
		rememberboth sha256 size
		ret sha256 size
  where
	calcsha256 = liftIO $ T.pack . show . sha2_256 <$> L.readFile content
	ret sha256 size = do
		let obj = LFS.TransferRequestObject
			{ LFS.req_oid = sha256
			, LFS.req_size = size
			}
		let req = LFS.TransferRequest
			{ LFS.req_operation = LFS.RequestUpload
			, LFS.req_transfers = [LFS.Basic]
			, LFS.req_ref = Nothing 
			, LFS.req_objects = [obj]
			}
		return (req, sha256, size)
	
	remembersha256 sha256 = setRemoteState rs k (T.unpack sha256)
	rememberboth sha256 size = setRemoteState rs k $
		show size ++ " " ++ T.unpack sha256

mkDownloadRequest :: RemoteStateHandle -> Key -> Annex (Maybe (LFS.TransferRequest, LFS.SHA256, Integer))
mkDownloadRequest rs k = case (extractKeySha256 k, extractKeySize k) of
	(Just sha256, Just size) -> ret sha256 size
	(_, Just size) ->
		remembersha256 >>= \case
			Just sha256 -> ret sha256 size
			Nothing -> return Nothing
	_ -> do
		rememberboth >>= \case
			Just (sha256, size) -> ret sha256 size
			Nothing -> return Nothing
  where
	ret sha256 size = do
		let obj = LFS.TransferRequestObject
			{ LFS.req_oid = sha256
			, LFS.req_size = size
			}
		let req = LFS.TransferRequest
			{ LFS.req_operation = LFS.RequestDownload
			, LFS.req_transfers = [LFS.Basic]
			, LFS.req_ref = Nothing 
			, LFS.req_objects = [obj]
			}
		return $ Just (req, sha256, size)
	remembersha256 = fmap T.pack <$> getRemoteState rs k
	rememberboth = maybe Nothing parse <$> getRemoteState rs k
	  where
		parse s = case words s of
			[ssize, ssha256] -> do
				size <- readish ssize
				return (T.pack ssha256, size)
			_ -> Nothing

store :: RemoteStateHandle -> TVar LFSHandle -> Storer
store rs h = fileStorer $ \k src p -> getLFSEndpoint LFS.RequestUpload h >>= \case
	Nothing -> return False
	Just endpoint -> flip catchNonAsync failederr $ do
		(req, sha256, size) <- mkUploadRequest rs k src
		sendTransferRequest req endpoint >>= \case
			Left err -> do
				warning err
				return False
			Right resp -> do
				body <- liftIO $ httpBodyStorer src p
				forM_ (LFS.objects resp) $
					send body sha256 size
				return True
  where
	send body sha256 size tro
		| LFS.resp_oid tro /= sha256 || LFS.resp_size tro /= size =
			giveup "git-lfs server requested other object than the one we asked to send"
		| otherwise = case LFS.resp_error tro of
			Just err -> giveup $
				T.unpack $ LFS.respobjerr_message err
			Nothing -> case LFS.resp_actions tro of
				Nothing -> noop
				Just op -> case LFS.uploadOperationRequests op body sha256 size of
					Nothing -> giveup "unable to parse git-lfs server upload url"
					Just [] -> noop -- server already has it
					Just reqs -> forM_ reqs $
						makeSmallAPIRequest . setRequestCheckStatus
	failederr e = do
		warning (show e)
		return False

retrieve :: RemoteStateHandle -> TVar LFSHandle -> Retriever
retrieve rs h = fileRetriever $ \dest k p -> getLFSEndpoint LFS.RequestDownload h >>= \case
	Nothing -> giveup "unable to connect to git-lfs endpoint"
	Just endpoint -> mkDownloadRequest rs k >>= \case
		Nothing -> giveup "unable to download this object from git-lfs"
		Just (req, sha256, size) -> sendTransferRequest req endpoint >>= \case
			Left err -> giveup (show err)
			Right resp -> case LFS.objects resp of
				[] -> giveup "git-lfs server did not provide a way to download this object"
				(tro:_)
					| LFS.resp_oid tro /= sha256 || LFS.resp_size tro /= size ->
						giveup "git-lfs server replied with other object than the one we requested"
					| otherwise -> go dest p tro
  where
	go dest p tro = case LFS.resp_error tro of
		Just err -> giveup $ T.unpack $ LFS.respobjerr_message err
		Nothing -> case LFS.resp_actions tro of
			Nothing -> giveup "git-lfs server did not provide a way to download this object"
			Just op -> case LFS.downloadOperationRequest op of
				Nothing -> giveup "unable to parse git-lfs server download url"
				Just req -> do
					uo <- getUrlOptions
					liftIO $ downloadConduit p req dest uo

checkKey :: RemoteStateHandle -> TVar LFSHandle -> CheckPresent
checkKey rs h key = getLFSEndpoint LFS.RequestDownload h >>= \case
	Nothing -> giveup "unable to connect to git-lfs endpoint"
	Just endpoint -> mkDownloadRequest rs key >>= \case
		-- Unable to find enough information to request the key
		-- from git-lfs, so it's not present there.
		Nothing -> return False
		Just (req, sha256, size) -> go sha256 size 
			=<< makeSmallAPIRequest (LFS.startTransferRequest endpoint req)
  where
	go sha256 size httpresp
		| responseStatus httpresp == status200 = go' sha256 size $
			LFS.parseTransferResponse (responseBody httpresp)
		| otherwise = giveup $
			"git-lfs server refused request: " ++ show (responseStatus httpresp)
	
	go' :: LFS.SHA256 -> Integer -> LFS.ParsedTransferResponse LFS.DownloadOperation -> Annex Bool
	go' _ _ (LFS.ParseFailed err) =
		giveup $ "unable to parse response from git-lfs server: " ++ err
	-- If the server responds with a json error message,
	-- the content is presumably not present.
	go' _ _ (LFS.ParsedTransferResponseError _) = return False
	-- If the server responds with at least one download operation,
	-- we will assume the content is present. We could also try to HEAD
	-- that download, but there's no guarantee HEAD is supported, and
	-- at most that would detect breakage where the server is confused
	-- about what objects it has.
	go' sha256 size (LFS.ParsedTransferResponse resp) =
		case LFS.objects resp of
			[] -> return False
			(tro:_)
				| isNothing (LFS.resp_actions tro) -> return False
				| isJust (LFS.resp_error tro) -> return False
				| LFS.resp_oid tro /= sha256 || LFS.resp_size tro /= size ->
					giveup "git-lfs server replied with other object than the one we requested"
				| otherwise -> return True

retrieveCheap :: Key -> AssociatedFile -> FilePath -> Annex Bool
retrieveCheap _ _ _ = return False

remove :: TVar LFSHandle -> Remover
remove _h _key = do
	warning "git-lfs does not support removing content"
	return False
