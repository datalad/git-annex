{- External special remote interface.
 -
 - Copyright 2013-2018 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Remote.External (remote) where

import Remote.External.Types
import qualified Annex
import Annex.Common
import Types.Remote
import Types.Export
import Types.CleanupActions
import Types.UrlContents
import qualified Git
import Config
import Git.Config (isTrue, boolConfig)
import Git.Env
import Remote.Helper.Special
import Remote.Helper.ExportImport
import Remote.Helper.ReadOnly
import Remote.Helper.Messages
import Utility.Metered
import Utility.Shell
import Messages.Progress
import Types.Transfer
import Logs.PreferredContent.Raw
import Logs.RemoteState
import Logs.Web
import Config.Cost
import Annex.Content
import Annex.Url
import Annex.UUID
import Creds

import Control.Concurrent.STM
import Control.Concurrent.Async
import System.Log.Logger (debugM)
import qualified Data.Map as M

remote :: RemoteType
remote = RemoteType
	{ typename = "external"
	, enumerate = const (findSpecialRemotes "externaltype")
	, generate = gen
	, setup = externalSetup
	, exportSupported = checkExportSupported
	, importSupported = importUnsupported
	}

gen :: Git.Repo -> UUID -> RemoteConfig -> RemoteGitConfig -> RemoteStateHandle -> Annex (Maybe Remote)
gen r u c gc rs
	-- readonly mode only downloads urls; does not use external program
	| remoteAnnexReadOnly gc = do
		cst <- remoteCost gc expensiveRemoteCost
		mk cst GloballyAvailable
			readonlyStorer
			retrieveUrl
			readonlyRemoveKey
			(checkKeyUrl r)
			Nothing
			(externalInfo externaltype)
			Nothing
			Nothing
			exportUnsupported
			exportUnsupported
	| otherwise = do
		external <- newExternal externaltype u c gc (Just rs)
		Annex.addCleanup (RemoteCleanup u) $ stopExternal external
		cst <- getCost external r gc
		avail <- getAvailability external r gc
		exportsupported <- if exportTree c
			then checkExportSupported' external
			else return False
		let exportactions = if exportsupported
			then ExportActions
				{ storeExport = storeExportM external
				, retrieveExport = retrieveExportM external
				, removeExport = removeExportM external
				, checkPresentExport = checkPresentExportM external
				, removeExportDirectory = Just $ removeExportDirectoryM external
				, renameExport = renameExportM external
				}
			else exportUnsupported
		-- Cheap exportSupported that replaces the expensive
		-- checkExportSupported now that we've already checked it.
		let cheapexportsupported = if exportsupported
			then exportIsSupported
			else exportUnsupported
		mk cst avail
			(storeKeyM external)
			(retrieveKeyFileM external)
			(removeKeyM external)
			(checkPresentM external)
			(Just (whereisKeyM external))
			(getInfoM external)
			(Just (claimUrlM external))
			(Just (checkUrlM external))
			exportactions
			cheapexportsupported
  where
	mk cst avail tostore toretrieve toremove tocheckkey towhereis togetinfo toclaimurl tocheckurl exportactions cheapexportsupported = do
		let rmt = Remote
			{ uuid = u
			, cost = cst
			, name = Git.repoDescribe r
			, storeKey = storeKeyDummy
			, retrieveKeyFile = retreiveKeyFileDummy
			, retrieveKeyFileCheap = \_ _ _ -> return False
			-- External special remotes use many http libraries
			-- and have no protection against redirects to
			-- local private web servers, or in some cases
			-- to file:// urls.
			, retrievalSecurityPolicy = mkRetrievalVerifiableKeysSecure gc
			, removeKey = removeKeyDummy
			, lockContent = Nothing
			, checkPresent = checkPresentDummy
			, checkPresentCheap = False
			, exportActions = exportactions
			, importActions = importUnsupported
			, whereisKey = towhereis
			, remoteFsck = Nothing
			, repairRepo = Nothing
			, config = c
			, localpath = Nothing
			, getRepo = return r
			, gitconfig = gc
			, readonly = False
			, appendonly = False
			, availability = avail
			, remotetype = remote 
				{ exportSupported = cheapexportsupported }
			, mkUnavailable = gen r u c
				(gc { remoteAnnexExternalType = Just "!dne!" }) rs
			, getInfo = togetinfo
			, claimUrl = toclaimurl
			, checkUrl = tocheckurl
			, remoteStateHandle = rs
			}
		return $ Just $ specialRemote c
			(simplyPrepare tostore)
			(simplyPrepare toretrieve)
			(simplyPrepare toremove)
			(simplyPrepare tocheckkey)
			rmt
	externaltype = fromMaybe (giveup "missing externaltype") (remoteAnnexExternalType gc)

externalSetup :: SetupStage -> Maybe UUID -> Maybe CredPair -> RemoteConfig -> RemoteGitConfig -> Annex (RemoteConfig, UUID)
externalSetup _ mu _ c gc = do
	u <- maybe (liftIO genUUID) return mu
	let externaltype = fromMaybe (giveup "Specify externaltype=") $
		M.lookup "externaltype" c
	(c', _encsetup) <- encryptionSetup c gc

	c'' <- case M.lookup "readonly" c of
		Just v | isTrue v == Just True -> do
			setConfig (remoteConfig (fromJust (lookupName c)) "readonly") (boolConfig True)
			return c'
		_ -> do
			external <- newExternal externaltype u c' gc Nothing
			handleRequest external INITREMOTE Nothing $ \resp -> case resp of
				INITREMOTE_SUCCESS -> result ()
				INITREMOTE_FAILURE errmsg -> Just $ giveup errmsg
				_ -> Nothing
			withExternalState external $
				liftIO . atomically . readTVar . externalConfig

	gitConfigSpecialRemote u c'' [("externaltype", externaltype)]
	return (c'', u)

checkExportSupported :: RemoteConfig -> RemoteGitConfig -> Annex Bool
checkExportSupported c gc = do
	let externaltype = fromMaybe (giveup "Specify externaltype=") $
		remoteAnnexExternalType gc <|> M.lookup "externaltype" c
	checkExportSupported' 
		=<< newExternal externaltype NoUUID c gc Nothing

checkExportSupported' :: External -> Annex Bool
checkExportSupported' external = go `catchNonAsync` (const (return False))
  where
	go = handleRequest external EXPORTSUPPORTED Nothing $ \resp -> case resp of
		EXPORTSUPPORTED_SUCCESS -> result True
		EXPORTSUPPORTED_FAILURE -> result False
		UNSUPPORTED_REQUEST -> result False
		_ -> Nothing

storeKeyM :: External -> Storer
storeKeyM external = fileStorer $ \k f p ->
	handleRequestKey external (\sk -> TRANSFER Upload sk f) k (Just p) $ \resp ->
		case resp of
			TRANSFER_SUCCESS Upload k' | k == k' -> result True
			TRANSFER_FAILURE Upload k' errmsg | k == k' ->
				Just $ do
					warning errmsg
					return (Result False)
			_ -> Nothing

retrieveKeyFileM :: External -> Retriever
retrieveKeyFileM external = fileRetriever $ \d k p -> 
	handleRequestKey external (\sk -> TRANSFER Download sk d) k (Just p) $ \resp ->
		case resp of
			TRANSFER_SUCCESS Download k'
				| k == k' -> result ()
			TRANSFER_FAILURE Download k' errmsg
				| k == k' -> Just $ giveup errmsg
			_ -> Nothing

removeKeyM :: External -> Remover
removeKeyM external k = safely $ 
	handleRequestKey external REMOVE k Nothing $ \resp ->
		case resp of
			REMOVE_SUCCESS k'
				| k == k' -> result True
			REMOVE_FAILURE k' errmsg
				| k == k' -> Just $ do
					warning errmsg
					return (Result False)
			_ -> Nothing

checkPresentM :: External -> CheckPresent
checkPresentM external k = either giveup id <$> go
  where
	go = handleRequestKey external CHECKPRESENT k Nothing $ \resp ->
		case resp of
			CHECKPRESENT_SUCCESS k'
				| k' == k -> result $ Right True
			CHECKPRESENT_FAILURE k'
				| k' == k -> result $ Right False
			CHECKPRESENT_UNKNOWN k' errmsg
				| k' == k -> result $ Left errmsg
			_ -> Nothing

whereisKeyM :: External -> Key -> Annex [String]
whereisKeyM external k = handleRequestKey external WHEREIS k Nothing $ \resp -> case resp of
	WHEREIS_SUCCESS s -> result [s]
	WHEREIS_FAILURE -> result []
	UNSUPPORTED_REQUEST -> result []
	_ -> Nothing

storeExportM :: External -> FilePath -> Key -> ExportLocation -> MeterUpdate -> Annex Bool
storeExportM external f k loc p = safely $
	handleRequestExport external loc req k (Just p) $ \resp -> case resp of
		TRANSFER_SUCCESS Upload k' | k == k' -> result True
		TRANSFER_FAILURE Upload k' errmsg | k == k' ->
			Just $ do
				warning errmsg
				return (Result False)
		UNSUPPORTED_REQUEST -> Just $ do
			warning "TRANSFEREXPORT not implemented by external special remote"
			return (Result False)
		_ -> Nothing
  where
	req sk = TRANSFEREXPORT Upload sk f

retrieveExportM :: External -> Key -> ExportLocation -> FilePath -> MeterUpdate -> Annex Bool
retrieveExportM external k loc d p = safely $
	handleRequestExport external loc req k (Just p) $ \resp -> case resp of
		TRANSFER_SUCCESS Download k'
			| k == k' -> result True
		TRANSFER_FAILURE Download k' errmsg
			| k == k' -> Just $ do
				warning errmsg
				return (Result False)
		UNSUPPORTED_REQUEST -> Just $ do
			warning "TRANSFEREXPORT not implemented by external special remote"
			return (Result False)
		_ -> Nothing
  where
	req sk = TRANSFEREXPORT Download sk d

checkPresentExportM :: External -> Key -> ExportLocation -> Annex Bool
checkPresentExportM external k loc = either giveup id <$> go
  where
	go = handleRequestExport external loc CHECKPRESENTEXPORT k Nothing $ \resp -> case resp of
		CHECKPRESENT_SUCCESS k'
			| k' == k -> result $ Right True
		CHECKPRESENT_FAILURE k'
			| k' == k -> result $ Right False
		CHECKPRESENT_UNKNOWN k' errmsg
			| k' == k -> result $ Left errmsg
		UNSUPPORTED_REQUEST -> result $
			Left "CHECKPRESENTEXPORT not implemented by external special remote"
		_ -> Nothing

removeExportM :: External -> Key -> ExportLocation -> Annex Bool
removeExportM external k loc = safely $
	handleRequestExport external loc REMOVEEXPORT k Nothing $ \resp -> case resp of
		REMOVE_SUCCESS k'
			| k == k' -> result True
		REMOVE_FAILURE k' errmsg
			| k == k' -> Just $ do
				warning errmsg
				return (Result False)
		UNSUPPORTED_REQUEST -> Just $ do
			warning "REMOVEEXPORT not implemented by external special remote"
			return (Result False)
		_ -> Nothing

removeExportDirectoryM :: External -> ExportDirectory -> Annex Bool
removeExportDirectoryM external dir = safely $
	handleRequest external req Nothing $ \resp -> case resp of
		REMOVEEXPORTDIRECTORY_SUCCESS -> result True
		REMOVEEXPORTDIRECTORY_FAILURE -> result False
		UNSUPPORTED_REQUEST -> result True
		_ -> Nothing
  where
	req = REMOVEEXPORTDIRECTORY dir

renameExportM :: External -> Key -> ExportLocation -> ExportLocation -> Annex (Maybe Bool)
renameExportM external k src dest = safely' (Just False) $
	handleRequestExport external src req k Nothing $ \resp -> case resp of
		RENAMEEXPORT_SUCCESS k'
			| k' == k -> result (Just True)
		RENAMEEXPORT_FAILURE k' 
			| k' == k -> result (Just False)
		UNSUPPORTED_REQUEST -> result Nothing
		_ -> Nothing
  where
	req sk = RENAMEEXPORT sk dest

safely :: Annex Bool -> Annex Bool
safely = safely' False

safely' :: a -> Annex a -> Annex a
safely' onerr a = go =<< tryNonAsync a
  where
	go (Right r) = return r
	go (Left e) = do
		toplevelWarning False (show e)
		return onerr

{- Sends a Request to the external remote, and waits for it to generate
 - a Response. That is fed into the responsehandler, which should return
 - the action to run for it (or Nothing if there's a protocol error).
 -
 - While the external remote is processing the Request, it may send
 - any number of RemoteRequests, that are handled here.
 -
 - An external remote process can only handle one request at a time.
 - Concurrent requests will start up additional processes.
 -
 - May throw exceptions, for example on protocol errors, or
 - when the repository cannot be used.
 -}
handleRequest :: External -> Request -> Maybe MeterUpdate -> ResponseHandler a -> Annex a
handleRequest external req mp responsehandler = 
	withExternalState external $ \st -> 
		handleRequest' st external req mp responsehandler

handleRequestKey :: External -> (SafeKey -> Request) -> Key -> Maybe MeterUpdate -> ResponseHandler a -> Annex a
handleRequestKey external mkreq k mp responsehandler = case mkSafeKey k of
	Right sk -> handleRequest external (mkreq sk) mp responsehandler
	Left e -> giveup e

{- Export location is first sent in an EXPORT message before
 - the main request. This is done because the ExportLocation can
 - contain spaces etc. -}
handleRequestExport :: External -> ExportLocation -> (SafeKey -> Request) -> Key -> Maybe MeterUpdate -> ResponseHandler a -> Annex a
handleRequestExport external loc mkreq k mp responsehandler = do
	withExternalState external $ \st -> do
		checkPrepared st external
		sendMessage st external (EXPORT loc)
	handleRequestKey external mkreq k mp responsehandler

handleRequest' :: ExternalState -> External -> Request -> Maybe MeterUpdate -> ResponseHandler a -> Annex a
handleRequest' st external req mp responsehandler
	| needsPREPARE req = do
		checkPrepared st external
		go
	| otherwise = go
  where
	go = do
		sendMessage st external req
		loop
	loop = receiveMessage st external responsehandler
		(\rreq -> Just $ handleRemoteRequest rreq >> loop)
		(\msg -> Just $ handleAsyncMessage msg >> loop)

	handleRemoteRequest (PROGRESS bytesprocessed) =
		maybe noop (\a -> liftIO $ a bytesprocessed) mp
	handleRemoteRequest (DIRHASH k) = 
		send $ VALUE $ hashDirMixed def k
	handleRemoteRequest (DIRHASH_LOWER k) = 
		send $ VALUE $ hashDirLower def k
	handleRemoteRequest (SETCONFIG setting value) =
		liftIO $ atomically $ modifyTVar' (externalConfig st) $
			M.insert setting value
	handleRemoteRequest (GETCONFIG setting) = do
		value <- fromMaybe "" . M.lookup setting
			<$> liftIO (atomically $ readTVar $ externalConfig st)
		send $ VALUE value
	handleRemoteRequest (SETCREDS setting login password) = do
		let v = externalConfig st
		c <- liftIO $ atomically $ readTVar v
		let gc = externalGitConfig external
		c' <- setRemoteCredPair encryptionAlreadySetup c gc
			(credstorage setting)
			(Just (login, password))
		void $ liftIO $ atomically $ swapTVar v c'
	handleRemoteRequest (GETCREDS setting) = do
		c <- liftIO $ atomically $ readTVar $ externalConfig st
		let gc = externalGitConfig external
		creds <- fromMaybe ("", "") <$> 
			getRemoteCredPair c gc (credstorage setting)
		send $ CREDS (fst creds) (snd creds)
	handleRemoteRequest GETUUID = send $
		VALUE $ fromUUID $ externalUUID external
	handleRemoteRequest GETGITDIR = send . VALUE =<< fromRepo Git.localGitDir
	handleRemoteRequest (SETWANTED expr) =
		preferredContentSet (externalUUID external) expr
	handleRemoteRequest GETWANTED = do
		expr <- fromMaybe "" . M.lookup (externalUUID external)
			<$> preferredContentMapRaw
		send $ VALUE expr
	handleRemoteRequest (SETSTATE key state) =
		case externalRemoteStateHandle external of
			Just h -> setRemoteState h key state
			Nothing -> senderror "cannot send SETSTATE here"
	handleRemoteRequest (GETSTATE key) =
		case externalRemoteStateHandle external of
			Just h -> do
				state <- fromMaybe ""
					<$> getRemoteState h key
				send $ VALUE state
			Nothing -> senderror "cannot send GETSTATE here"
	handleRemoteRequest (SETURLPRESENT key url) =
		setUrlPresent key url
	handleRemoteRequest (SETURLMISSING key url) =
		setUrlMissing key url
	handleRemoteRequest (SETURIPRESENT key uri) =
		withurl (SETURLPRESENT key) uri
	handleRemoteRequest (SETURIMISSING key uri) =
		withurl (SETURLMISSING key) uri
	handleRemoteRequest (GETURLS key prefix) = do
		mapM_ (send . VALUE) =<< getUrlsWithPrefix key prefix
		send (VALUE "") -- end of list
	handleRemoteRequest (DEBUG msg) = liftIO $ debugM "external" msg
	handleRemoteRequest (INFO msg) = showInfo msg
	handleRemoteRequest (VERSION _) = senderror "too late to send VERSION"

	handleAsyncMessage (ERROR err) = giveup $ "external special remote error: " ++ err

	send = sendMessage st external
	senderror = sendMessage st external . ERROR 

	credstorage setting = CredPairStorage
		{ credPairFile = base
		, credPairEnvironment = (base ++ "login", base ++ "password")
		, credPairRemoteField = setting
		}
	  where
		base = replace "/" "_" $ fromUUID (externalUUID external) ++ "-" ++ setting
			
	withurl mk uri = handleRemoteRequest $ mk $
		setDownloader (show uri) OtherDownloader

sendMessage :: Sendable m => ExternalState -> External -> m -> Annex ()
sendMessage st external m = liftIO $ do
	protocolDebug external st True line
	hPutStrLn h line
	hFlush h
  where
	line = unwords $ formatMessage m
	h = externalSend st

{- A response handler can yeild a result, or it can request that another
 - message be consumed from the external result. -}
data ResponseHandlerResult a
	= Result a
	| GetNextMessage (ResponseHandler a)

type ResponseHandler a = Response -> Maybe (Annex (ResponseHandlerResult a))

result :: a -> Maybe (Annex (ResponseHandlerResult a))
result = Just . return . Result

{- Waits for a message from the external remote, and passes it to the
 - apppropriate handler. 
 -
 - If the handler returns Nothing, this is a protocol error.-}
receiveMessage
	:: ExternalState
	-> External 
	-> ResponseHandler a
	-> (RemoteRequest -> Maybe (Annex a))
	-> (AsyncMessage -> Maybe (Annex a))
	-> Annex a
receiveMessage st external handleresponse handlerequest handleasync =
	go =<< liftIO (catchMaybeIO $ hGetLine $ externalReceive st)
  where
	go Nothing = protocolError False ""
	go (Just s) = do
		liftIO $ protocolDebug external st False s
		case parseMessage s :: Maybe Response of
			Just resp -> case handleresponse resp of
				Nothing -> protocolError True s
				Just callback -> callback >>= \case
					Result a -> return a
					GetNextMessage handleresponse' ->
						receiveMessage st external handleresponse' handlerequest handleasync
			Nothing -> case parseMessage s :: Maybe RemoteRequest of
				Just req -> maybe (protocolError True s) id (handlerequest req)
				Nothing -> case parseMessage s :: Maybe AsyncMessage of
					Just msg -> maybe (protocolError True s) id (handleasync msg)
					Nothing -> protocolError False s
	protocolError parsed s = giveup $ "external special remote protocol error, unexpectedly received \"" ++ s ++ "\" " ++
		if parsed
			then "(command not allowed at this time)"
			else "(unable to parse command)"

protocolDebug :: External -> ExternalState -> Bool -> String -> IO ()
protocolDebug external st sendto line = debugM "external" $ unwords
	[ externalRemoteProgram (externalType external) ++ 
		"[" ++ show (externalPid st) ++ "]"
	, if sendto then "<--" else "-->"
	, line
	]

{- While the action is running, the ExternalState provided to it will not
 - be available to any other calls.
 -
 - Starts up a new process if no ExternalStates are available. -}
withExternalState :: External -> (ExternalState -> Annex a) -> Annex a
withExternalState external = bracket alloc dealloc
  where
	v = externalState external

	alloc = do
		ms <- liftIO $ atomically $ do
			l <- readTVar v
			case l of
				[] -> return Nothing
				(st:rest) -> do
					writeTVar v rest
					return (Just st)
		maybe (startExternal external) return ms
	
	dealloc st = liftIO $ atomically $ modifyTVar' v (st:)

{- Starts an external remote process running, and checks VERSION and
 - exchanges EXTENSIONS. -}
startExternal :: External -> Annex ExternalState
startExternal external = do
	errrelayer <- mkStderrRelayer
	st <- start errrelayer =<< Annex.gitRepo
	receiveMessage st external
		(const Nothing)
		(checkVersion st external)
		(const Nothing)
	sendMessage st external (EXTENSIONS supportedExtensionList)
	-- It responds with a EXTENSIONS_RESPONSE; that extensions list
	-- is reserved for future expansion. UNSUPPORTED_REQUEST is also
	-- accepted.
	receiveMessage st external
		(\resp -> case resp of
			EXTENSIONS_RESPONSE _ -> result ()
			UNSUPPORTED_REQUEST -> result ()
			_ -> Nothing
		)
		(const Nothing)
		(const Nothing)
	return st
  where
	start errrelayer g = liftIO $ do
		cmdpath <- searchPath basecmd
		(cmd, ps) <- maybe (pure (basecmd, [])) findShellCommand cmdpath
		let basep = (proc cmd (toCommand ps))
			{ std_in = CreatePipe
			, std_out = CreatePipe
			, std_err = CreatePipe
			}
		p <- propgit g basep
		(Just hin, Just hout, Just herr, ph) <- 
			createProcess p `catchIO` runerr cmdpath
		stderrelay <- async $ errrelayer herr
		cv <- newTVarIO $ externalDefaultConfig external
		pv <- newTVarIO Unprepared
		pid <- atomically $ do
			n <- succ <$> readTVar (externalLastPid external)
			writeTVar (externalLastPid external) n
			return n
		return $ ExternalState
			{ externalSend = hin
			, externalReceive = hout
			, externalPid = pid
			, externalShutdown = do
				cancel stderrelay
				void $ waitForProcess ph
			, externalPrepared = pv
			, externalConfig = cv
			}
	
	basecmd = externalRemoteProgram $ externalType external

	propgit g p = do
		environ <- propGitEnv g
		return $ p { env = Just environ }

	runerr (Just cmd) _ =
		giveup $ "Cannot run " ++ cmd ++ " -- Make sure it's executable and that its dependencies are installed."
	runerr Nothing _ = do
		path <- intercalate ":" <$> getSearchPath
		giveup $ "Cannot run " ++ basecmd ++ " -- It is not installed in PATH (" ++ path ++ ")"

stopExternal :: External -> Annex ()
stopExternal external = liftIO $ do
	l <- atomically $ swapTVar (externalState external) []
	mapM_ stop l
  where
	stop st = do
		hClose $ externalSend st
		hClose $ externalReceive st
		externalShutdown st

externalRemoteProgram :: ExternalType -> String
externalRemoteProgram externaltype = "git-annex-remote-" ++ externaltype

checkVersion :: ExternalState -> External -> RemoteRequest -> Maybe (Annex ())
checkVersion st external (VERSION v) = Just $
	if v `elem` supportedProtocolVersions
		then noop
		else sendMessage st external (ERROR "unsupported VERSION")
checkVersion _ _ _ = Nothing

{- If repo has not been prepared, sends PREPARE.
 -
 - If the repo fails to prepare, or failed before, throws an exception with
 - the error message. -}
checkPrepared :: ExternalState -> External -> Annex ()
checkPrepared st external = do
	v <- liftIO $ atomically $ readTVar $ externalPrepared st
	case v of
		Prepared -> noop
		FailedPrepare errmsg -> giveup errmsg
		Unprepared ->
			handleRequest' st external PREPARE Nothing $ \resp ->
				case resp of
					PREPARE_SUCCESS -> Just $ do
						setprepared Prepared
						return (Result ())
					PREPARE_FAILURE errmsg -> Just $ do
						setprepared $ FailedPrepare errmsg
						giveup errmsg
					_ -> Nothing
  where
	setprepared status = liftIO $ atomically $ void $
		swapTVar (externalPrepared st) status

{- Caches the cost in the git config to avoid needing to start up an
 - external special remote every time time just to ask it what its
 - cost is. -}
getCost :: External -> Git.Repo -> RemoteGitConfig -> Annex Cost
getCost external r gc =
	(go =<< remoteCost' gc) `catchNonAsync` const (pure defcst)
  where
	go (Just c) = return c
	go Nothing = do
		c <- handleRequest external GETCOST Nothing $ \req -> case req of
			COST c -> result c
			UNSUPPORTED_REQUEST -> result defcst
			_ -> Nothing
		setRemoteCost r c
		return c
	defcst = expensiveRemoteCost

{- Caches the availability in the git config to avoid needing to start up an
 - external special remote every time time just to ask it what its
 - availability is.
 -
 - Most remotes do not bother to implement a reply to this request;
 - globally available is the default.
 -}
getAvailability :: External -> Git.Repo -> RemoteGitConfig -> Annex Availability
getAvailability external r gc = 
	maybe (catchNonAsync query (const (pure defavail))) return
		(remoteAnnexAvailability gc)
  where
	query = do
		avail <- handleRequest external GETAVAILABILITY Nothing $ \req -> case req of
			AVAILABILITY avail -> result avail
			UNSUPPORTED_REQUEST -> result defavail
			_ -> Nothing
		setRemoteAvailability r avail
		return avail
	defavail = GloballyAvailable

claimUrlM :: External -> URLString -> Annex Bool
claimUrlM external url =
	handleRequest external (CLAIMURL url) Nothing $ \req -> case req of
		CLAIMURL_SUCCESS -> result True
		CLAIMURL_FAILURE -> result False
		UNSUPPORTED_REQUEST -> result False
		_ -> Nothing

checkUrlM :: External -> URLString -> Annex UrlContents
checkUrlM external url = 
	handleRequest external (CHECKURL url) Nothing $ \req -> case req of
		CHECKURL_CONTENTS sz f -> result $ UrlContents sz $
			if null f then Nothing else Just $ mkSafeFilePath f
		CHECKURL_MULTI l -> result $ UrlMulti $ map mkmulti l
		CHECKURL_FAILURE errmsg -> Just $ giveup errmsg
		UNSUPPORTED_REQUEST -> giveup "CHECKURL not implemented by external special remote"
		_ -> Nothing
  where
	mkmulti (u, s, f) = (u, s, mkSafeFilePath f)

retrieveUrl :: Retriever
retrieveUrl = fileRetriever $ \f k p -> do
	us <- getWebUrls k
	unlessM (downloadUrl k p us f) $
		giveup "failed to download content"

checkKeyUrl :: Git.Repo -> CheckPresent
checkKeyUrl r k = do
	showChecking r
	us <- getWebUrls k
	anyM (\u -> withUrlOptions $ checkBoth u (fromKey keySize k)) us

getWebUrls :: Key -> Annex [URLString]
getWebUrls key = filter supported <$> getUrls key
  where
	supported u = snd (getDownloader u) == WebDownloader
			
externalInfo :: ExternalType -> Annex [(String, String)]
externalInfo et = return [("externaltype", et)]

getInfoM :: External -> Annex [(String, String)]
getInfoM external = (++)
	<$> externalInfo (externalType external)
	<*> handleRequest external GETINFO Nothing (collect [])
  where
	collect l req = case req of
		INFOFIELD f -> Just $ return $
			GetNextMessage $ collectvalue l f
		INFOEND -> result (reverse l)
		UNSUPPORTED_REQUEST -> result []
		_ -> Nothing
	
	collectvalue l f req = case req of
		INFOVALUE v -> Just $ return $
			GetNextMessage $ collect ((f, v) : l)
		_ -> Nothing
