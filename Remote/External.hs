{- External special remote interface.
 -
 - Copyright 2013-2020 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Remote.External (remote) where

import Remote.External.Types
import Remote.External.AsyncExtension
import qualified Annex
import Annex.Common
import qualified Annex.ExternalAddonProcess as AddonProcess
import Types.Remote
import Types.Export
import Types.CleanupActions
import Types.UrlContents
import Types.ProposedAccepted
import qualified Git
import Config
import Git.Config (boolConfig)
import Annex.SpecialRemote.Config
import Remote.Helper.Special
import Remote.Helper.ExportImport
import Remote.Helper.ReadOnly
import Remote.Helper.Messages
import Utility.Metered
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
import System.Log.Logger (debugM)
import qualified Data.Map as M
import qualified Data.Set as S

remote :: RemoteType
remote = specialRemoteType $ RemoteType
	{ typename = "external"
	, enumerate = const (findSpecialRemotes "externaltype")
	, generate = gen
	, configParser = remoteConfigParser
	, setup = externalSetup
	, exportSupported = checkExportSupported
	, importSupported = importUnsupported
	}

externaltypeField :: RemoteConfigField
externaltypeField = Accepted "externaltype"

readonlyField :: RemoteConfigField
readonlyField = Accepted "readonly"

gen :: Git.Repo -> UUID -> RemoteConfig -> RemoteGitConfig -> RemoteStateHandle -> Annex (Maybe Remote)
gen r u rc gc rs
	-- readonly mode only downloads urls; does not use external program
	| externaltype == "readonly" = do
		c <- parsedRemoteConfig remote rc
		cst <- remoteCost gc expensiveRemoteCost
		mk c cst GloballyAvailable
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
		c <- parsedRemoteConfig remote rc
		external <- newExternal externaltype (Just u) c (Just gc) (Just rs)
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
		mk c cst avail
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
	mk c cst avail tostore toretrieve toremove tocheckkey towhereis togetinfo toclaimurl tocheckurl exportactions cheapexportsupported = do
		let rmt = Remote
			{ uuid = u
			, cost = cst
			, name = Git.repoDescribe r
			, storeKey = storeKeyDummy
			, retrieveKeyFile = retrieveKeyFileDummy
			, retrieveKeyFileCheap = Nothing
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
			, mkUnavailable = gen r u rc
				(gc { remoteAnnexExternalType = Just "!dne!" }) rs
			, getInfo = togetinfo
			, claimUrl = toclaimurl
			, checkUrl = tocheckurl
			, remoteStateHandle = rs
			}
		return $ Just $ specialRemote c
			tostore
			toretrieve
			toremove
			tocheckkey
			rmt
	externaltype = fromMaybe (giveup "missing externaltype") (remoteAnnexExternalType gc)

externalSetup :: SetupStage -> Maybe UUID -> Maybe CredPair -> RemoteConfig -> RemoteGitConfig -> Annex (RemoteConfig, UUID)
externalSetup _ mu _ c gc = do
	u <- maybe (liftIO genUUID) return mu
	pc <- either giveup return $ parseRemoteConfig c lenientRemoteConfigParser
	let readonlyconfig = getRemoteConfigValue readonlyField pc == Just True
	let externaltype = if readonlyconfig
		then "readonly"
		else fromMaybe (giveup "Specify externaltype=") $
			getRemoteConfigValue externaltypeField pc
	(c', _encsetup) <- encryptionSetup c gc

	c'' <- if readonlyconfig
		then do
			-- Setting annex-readonly is not really necessary
			-- anymore, but older versions of git-annex used
			-- this, not externaltype=readonly, so still set
			-- it.
			setConfig (remoteAnnexConfig (fromJust (lookupName c)) "readonly") (boolConfig True)
			return c'
		else do
			pc' <- either giveup return $ parseRemoteConfig c' lenientRemoteConfigParser
			external <- newExternal externaltype (Just u) pc' (Just gc) Nothing
			-- Now that we have an external, ask it to LISTCONFIGS, 
			-- and re-parse the RemoteConfig strictly, so we can
			-- error out if the user provided an unexpected config.
			_ <- either giveup return . parseRemoteConfig c' 
				=<< strictRemoteConfigParser external
			handleRequest external INITREMOTE Nothing $ \case
				INITREMOTE_SUCCESS -> result ()
				INITREMOTE_FAILURE errmsg -> Just $ giveup $
					respErrorMessage "INITREMOTE" errmsg
				_ -> Nothing
			-- Any config changes the external made before
			-- responding to INITREMOTE need to be applied to
			-- the RemoteConfig.
			changes <- withExternalState external $
				liftIO . atomically . readTVar . externalConfigChanges
			return (changes c')

	gitConfigSpecialRemote u c'' [("externaltype", externaltype)]
	return (M.delete readonlyField c'', u)

checkExportSupported :: ParsedRemoteConfig -> RemoteGitConfig -> Annex Bool
checkExportSupported c gc = do
	let externaltype = fromMaybe (giveup "Specify externaltype=") $
		remoteAnnexExternalType gc <|> getRemoteConfigValue externaltypeField c
	if externaltype == "readonly"
		then return False
		else checkExportSupported' 
			=<< newExternal externaltype Nothing c (Just gc) Nothing

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
	either giveup return =<< go k f p
  where
	go k f p = handleRequestKey external (\sk -> TRANSFER Upload sk f) k (Just p) $ \resp ->
		case resp of
			TRANSFER_SUCCESS Upload k' | k == k' ->
				result (Right ())
			TRANSFER_FAILURE Upload k' errmsg | k == k' ->
				result (Left (respErrorMessage "TRANSFER" errmsg))
			_ -> Nothing

retrieveKeyFileM :: External -> Retriever
retrieveKeyFileM external = fileRetriever $ \d k p ->
	either giveup return =<< go d k p
  where
	go d k p = handleRequestKey external (\sk -> TRANSFER Download sk d) k (Just p) $ \resp ->
		case resp of
			TRANSFER_SUCCESS Download k'
				| k == k' -> result $ Right ()
			TRANSFER_FAILURE Download k' errmsg
				| k == k' -> result $ Left $
					respErrorMessage "TRANSFER" errmsg
			_ -> Nothing

removeKeyM :: External -> Remover
removeKeyM external k = either giveup return =<< go
  where
	go = handleRequestKey external REMOVE k Nothing $ \resp ->
		case resp of
			REMOVE_SUCCESS k'
				| k == k' -> result $ Right ()
			REMOVE_FAILURE k' errmsg
				| k == k' -> result $ Left $
					respErrorMessage "REMOVE" errmsg
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
				| k' == k -> result $ Left $
					respErrorMessage "CHECKPRESENT" errmsg
			_ -> Nothing

whereisKeyM :: External -> Key -> Annex [String]
whereisKeyM external k = handleRequestKey external WHEREIS k Nothing $ \resp -> case resp of
	WHEREIS_SUCCESS s -> result [s]
	WHEREIS_FAILURE -> result []
	UNSUPPORTED_REQUEST -> result []
	_ -> Nothing

storeExportM :: External -> FilePath -> Key -> ExportLocation -> MeterUpdate -> Annex ()
storeExportM external f k loc p = either giveup return =<< go
  where
	go = handleRequestExport external loc req k (Just p) $ \resp -> case resp of
		TRANSFER_SUCCESS Upload k' | k == k' -> result $ Right ()
		TRANSFER_FAILURE Upload k' errmsg | k == k' ->
			result $ Left $ respErrorMessage "TRANSFER" errmsg
		UNSUPPORTED_REQUEST -> 
			result $ Left "TRANSFEREXPORT not implemented by external special remote"
		_ -> Nothing
	req sk = TRANSFEREXPORT Upload sk f

retrieveExportM :: External -> Key -> ExportLocation -> FilePath -> MeterUpdate -> Annex ()
retrieveExportM external k loc d p = either giveup return =<< go
  where
	go = handleRequestExport external loc req k (Just p) $ \resp -> case resp of
		TRANSFER_SUCCESS Download k'
			| k == k' -> result $ Right ()
		TRANSFER_FAILURE Download k' errmsg
			| k == k' -> result $ Left $ respErrorMessage "TRANSFER" errmsg
		UNSUPPORTED_REQUEST ->
			result $ Left "TRANSFEREXPORT not implemented by external special remote"
		_ -> Nothing
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
			| k' == k -> result $ Left $
				respErrorMessage "CHECKPRESENT" errmsg
		UNSUPPORTED_REQUEST -> result $
			Left "CHECKPRESENTEXPORT not implemented by external special remote"
		_ -> Nothing

removeExportM :: External -> Key -> ExportLocation -> Annex ()
removeExportM external k loc = either giveup return =<< go
  where
	go = handleRequestExport external loc REMOVEEXPORT k Nothing $ \resp -> case resp of
		REMOVE_SUCCESS k'
			| k == k' -> result $ Right ()
		REMOVE_FAILURE k' errmsg
			| k == k' -> result $ Left $ respErrorMessage "REMOVE" errmsg
		UNSUPPORTED_REQUEST -> result $
			Left $ "REMOVEEXPORT not implemented by external special remote"
		_ -> Nothing

removeExportDirectoryM :: External -> ExportDirectory -> Annex ()
removeExportDirectoryM external dir = either giveup return =<< go
  where
	go = handleRequest external req Nothing $ \resp -> case resp of
		REMOVEEXPORTDIRECTORY_SUCCESS -> result $ Right ()
		REMOVEEXPORTDIRECTORY_FAILURE -> result $
			Left "failed to remove directory"
		UNSUPPORTED_REQUEST -> result $ Right ()
		_ -> Nothing
	req = REMOVEEXPORTDIRECTORY dir

renameExportM :: External -> Key -> ExportLocation -> ExportLocation -> Annex (Maybe ())
renameExportM external k src dest = either giveup return =<< go
  where
	go = handleRequestExport external src req k Nothing $ \resp -> case resp of
		RENAMEEXPORT_SUCCESS k'
			| k' == k -> result $ Right (Just ())
		RENAMEEXPORT_FAILURE k' 
			| k' == k -> result $ Left "failed to rename exported file"
		UNSUPPORTED_REQUEST -> result (Right Nothing)
		_ -> Nothing
	req sk = RENAMEEXPORT sk dest

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
		sendMessage st (EXPORT loc)
	handleRequestKey external mkreq k mp responsehandler

handleRequest' :: ExternalState -> External -> Request -> Maybe MeterUpdate -> ResponseHandler a -> Annex a
handleRequest' st external req mp responsehandler
	| needsPREPARE req = do
		checkPrepared st external
		go
	| otherwise = go
  where
	go = do
		sendMessage st req
		loop
	loop = receiveMessage st external responsehandler
		(\rreq -> Just $ handleRemoteRequest rreq >> loop)
		(\msg -> Just $ handleExceptionalMessage msg >> loop)

	handleRemoteRequest (PROGRESS bytesprocessed) =
		maybe noop (\a -> liftIO $ a bytesprocessed) mp
	handleRemoteRequest (DIRHASH k) = 
		send $ VALUE $ fromRawFilePath $ hashDirMixed def k
	handleRemoteRequest (DIRHASH_LOWER k) = 
		send $ VALUE $ fromRawFilePath $ hashDirLower def k
	handleRemoteRequest (SETCONFIG setting value) =
		liftIO $ atomically $ do
			modifyTVar' (externalConfig st) $ \(ParsedRemoteConfig m c) ->
				let m' = M.insert
					(Accepted setting)
					(RemoteConfigValue (PassedThrough value))
					m
				    c' = M.insert
				    	(Accepted setting)
					(Accepted value)
					c
				in ParsedRemoteConfig m' c'
			modifyTVar' (externalConfigChanges st) $ \f ->
				M.insert (Accepted setting) (Accepted value) . f
	handleRemoteRequest (GETCONFIG setting) = do
		value <- maybe "" fromProposedAccepted
			. (M.lookup (Accepted setting))
			. unparsedRemoteConfig
			<$> liftIO (atomically $ readTVar $ externalConfig st)
		send $ VALUE value
	handleRemoteRequest (SETCREDS setting login password) = case (externalUUID external, externalGitConfig external) of
		(Just u, Just gc) -> do
			let v = externalConfig st
			pc <- liftIO $ atomically $ readTVar v
			pc' <- setRemoteCredPair' pc encryptionAlreadySetup gc
				(credstorage setting u)
				(Just (login, password))
			let configchanges = M.differenceWithKey
				(\_k a b -> if a == b then Nothing else Just a)
				(unparsedRemoteConfig pc')
				(unparsedRemoteConfig pc)
			void $ liftIO $ atomically $ do
				_ <- swapTVar v pc'
				modifyTVar' (externalConfigChanges st) $ \f ->
					M.union configchanges . f
		_ -> senderror "cannot send SETCREDS here"
	handleRemoteRequest (GETCREDS setting) = case (externalUUID external, externalGitConfig external) of
		(Just u, Just gc) -> do
			c <- liftIO $ atomically $ readTVar $ externalConfig st
			creds <- fromMaybe ("", "") <$> 
				getRemoteCredPair c gc (credstorage setting u)
			send $ CREDS (fst creds) (snd creds)
		_ -> senderror "cannot send GETCREDS here"
	handleRemoteRequest GETUUID = case externalUUID external of
		Just u -> send $ VALUE $ fromUUID u
		Nothing -> senderror "cannot send GETUUID here"
	handleRemoteRequest GETGITDIR = 
		send . VALUE . fromRawFilePath =<< fromRepo Git.localGitDir
	handleRemoteRequest (SETWANTED expr) = case externalUUID external of
		Just u -> preferredContentSet u expr
		Nothing -> senderror "cannot send SETWANTED here"
	handleRemoteRequest GETWANTED = case externalUUID external of
		Just u -> do
			expr <- fromMaybe "" . M.lookup u
				<$> preferredContentMapRaw
			send $ VALUE expr
		Nothing -> senderror "cannot send GETWANTED here"
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

	handleExceptionalMessage (ERROR err) = giveup $ "external special remote error: " ++ err

	send = sendMessage st
	senderror = sendMessage st . ERROR 

	credstorage setting u = CredPairStorage
		{ credPairFile = base
		, credPairEnvironment = (base ++ "login", base ++ "password")
		, credPairRemoteField = Accepted setting
		}
	  where
		base = replace "/" "_" $ fromUUID u ++ "-" ++ setting
			
	withurl mk uri = handleRemoteRequest $ mk $
		setDownloader (show uri) OtherDownloader

sendMessage :: (Sendable m, ToAsyncWrapped m) => ExternalState -> m -> Annex ()
sendMessage st m = liftIO $ externalSend st m

sendMessageAddonProcess :: Sendable m => AddonProcess.ExternalAddonProcess -> m -> IO ()
sendMessageAddonProcess p m = do
	AddonProcess.protocolDebug p True line
	hPutStrLn h line
	hFlush h
  where
	h = AddonProcess.externalSend p
	line = unwords $ formatMessage m

receiveMessageAddonProcess :: AddonProcess.ExternalAddonProcess -> IO (Maybe String)
receiveMessageAddonProcess p = do
	v <- catchMaybeIO $ hGetLine $ AddonProcess.externalReceive p
	maybe noop (AddonProcess.protocolDebug p False) v
	return v

shutdownAddonProcess :: AddonProcess.ExternalAddonProcess -> Bool -> IO ()
shutdownAddonProcess = AddonProcess.externalShutdown 

{- A response handler can yeild a result, or it can request that another
 - message be consumed from the external. -}
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
	-> (ExceptionalMessage -> Maybe (Annex a))
	-> Annex a
receiveMessage st external handleresponse handlerequest handleexceptional =
	go =<< liftIO (externalReceive st)
  where
	go Nothing = protocolError False "<EOF>"
	go (Just s) = case parseMessage s :: Maybe Response of
		Just resp -> case handleresponse resp of
			Nothing -> protocolError True s
			Just callback -> callback >>= \case
				Result a -> return a
				GetNextMessage handleresponse' ->
					receiveMessage st external handleresponse' handlerequest handleexceptional
		Nothing -> case parseMessage s :: Maybe RemoteRequest of
			Just req -> maybe (protocolError True s) id (handlerequest req)
			Nothing -> case parseMessage s :: Maybe ExceptionalMessage of
				Just msg -> maybe (protocolError True s) id (handleexceptional msg)
				Nothing -> protocolError False s
	protocolError parsed s = giveup $ "external special remote protocol error, unexpectedly received \"" ++ s ++ "\" " ++
		if parsed
			then "(command not allowed at this time)"
			else "(unable to parse command)"

{- While the action is running, the ExternalState provided to it will not
 - be available to any other calls.
 -
 - Starts up a new process if no ExternalStates are available.
 -
 - If the action is interrupted by an async exception, the external process
 - is in an unknown state, and may eg be still performing a transfer. So it
 - is killed. The action should not normally throw any exception itself,
 - unless perhaps there's a problem communicating with the external
 - process.
 -}
withExternalState :: External -> (ExternalState -> Annex a) -> Annex a
withExternalState external a = do
	st <- get
	r <- a st `onException` liftIO (externalShutdown st True)
	put st -- only when no exception is thrown
	return r
  where
	v = externalState external

	get = do
		ms <- liftIO $ atomically $ do
			l <- readTVar v
			case l of
				[] -> return Nothing
				(st:rest) -> do
					writeTVar v rest
					return (Just st)
		maybe (startExternal external) return ms
	
	put st = liftIO $ atomically $ modifyTVar' v (st:)

{- Starts an external remote process running, and checks VERSION and
 - exchanges EXTENSIONS.
 -
 - When the ASYNC extension is negotiated, a single process is used,
 - and this constructs a external state that communicates with a thread
 - that relays to it.
 -}
startExternal :: External -> Annex ExternalState
startExternal external =
	liftIO (atomically $ takeTMVar (externalAsync external)) >>= \case
		UncheckedExternalAsync -> do
			(st, extensions) <- startExternal' external
			if asyncExtensionEnabled extensions
				then do
					relay <- liftIO $ runRelayToExternalAsync external st
					st' <- liftIO $ asyncRelayExternalState relay
					store (ExternalAsync relay)
					return st'
				else do
					store NoExternalAsync
					return st
		v@NoExternalAsync -> do
			store v
			fst <$> startExternal' external
		v@(ExternalAsync relay) -> do
			store v
			liftIO $ asyncRelayExternalState relay
  where
	store = liftIO . atomically . putTMVar (externalAsync external)

startExternal' :: External -> Annex (ExternalState, ExtensionList)
startExternal' external = do
	pid <- liftIO $ atomically $ do
		n <- succ <$> readTVar (externalLastPid external)
		writeTVar (externalLastPid external) n
		return n
	AddonProcess.startExternalAddonProcess basecmd pid >>= \case
		Left (AddonProcess.ProgramFailure err) -> giveup err
		Left (AddonProcess.ProgramNotInstalled err) ->
			case (lookupName (unparsedRemoteConfig (externalDefaultConfig external)), remoteAnnexReadOnly <$> externalGitConfig external) of
				(Just rname, Just True) -> giveup $ unlines
					[ err
					, "This remote has annex-readonly=true, and previous versions of"
					, "git-annex would tried to download from it without"
					, "installing " ++ basecmd ++ ". If you want that, you need to set:"
					, "git config remote." ++ rname ++ ".annex-externaltype readonly"
					]
				_ -> giveup err
		Right p -> do
			cv <- liftIO $ newTVarIO $ externalDefaultConfig external
			ccv <- liftIO $ newTVarIO id
			pv <- liftIO $ newTVarIO Unprepared
			let st = ExternalState
				{ externalSend = sendMessageAddonProcess p
				, externalReceive = receiveMessageAddonProcess p
				, externalShutdown = shutdownAddonProcess p
				, externalPrepared = pv
				, externalConfig = cv
				, externalConfigChanges = ccv
				}
			extensions <- startproto st
			return (st, extensions)
  where
	basecmd = "git-annex-remote-" ++ externalType external
	startproto st = do
		receiveMessage st external
			(const Nothing)
			(checkVersion st)
			(const Nothing)
		sendMessage st (EXTENSIONS supportedExtensionList)
		-- It responds with a EXTENSIONS_RESPONSE; that extensions
		-- list is reserved for future expansion. UNSUPPORTED_REQUEST
		-- is also accepted.
		exwanted <- receiveMessage st external
			(\resp -> case resp of
				EXTENSIONS_RESPONSE l -> result l
				UNSUPPORTED_REQUEST -> result mempty
				_ -> Nothing
			)
			(const Nothing)
			(const Nothing)
		case filter (`notElem` fromExtensionList supportedExtensionList) (fromExtensionList exwanted) of
			[] -> return exwanted
			exrest -> giveup $ unwords $
				[ basecmd
				, "requested extensions that this version of git-annex does not support:"
				] ++ exrest

stopExternal :: External -> Annex ()
stopExternal external = liftIO $ do
	l <- atomically $ swapTVar (externalState external) []
	mapM_ (flip externalShutdown False) l

checkVersion :: ExternalState -> RemoteRequest -> Maybe (Annex ())
checkVersion st (VERSION v) = Just $
	if v `elem` supportedProtocolVersions
		then noop
		else sendMessage st (ERROR "unsupported VERSION")
checkVersion _ _ = Nothing

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
						let errmsg' = respErrorMessage "PREPARE" errmsg
						setprepared $ FailedPrepare errmsg'
						giveup errmsg'
					_ -> Nothing
  where
	setprepared status = liftIO $ atomically $ void $
		swapTVar (externalPrepared st) status

respErrorMessage :: String -> String -> String
respErrorMessage req err
	| null err = req ++ " failed with no reason given"
	| otherwise = err

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
			if null f then Nothing else Just f
		CHECKURL_MULTI l -> result $ UrlMulti $ map mkmulti l
		CHECKURL_FAILURE errmsg -> Just $ giveup $
			respErrorMessage "CHECKURL" errmsg
		UNSUPPORTED_REQUEST -> giveup "CHECKURL not implemented by external special remote"
		_ -> Nothing
  where
	mkmulti (u, s, f) = (u, s, f)

retrieveUrl :: Retriever
retrieveUrl = fileRetriever $ \f k p -> do
	us <- getWebUrls k
	unlessM (withUrlOptions $ downloadUrl k p us f) $
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

{- All unknown configs are passed through in case the external program
 - uses them. -}
lenientRemoteConfigParser :: RemoteConfigParser
lenientRemoteConfigParser =
	addRemoteConfigParser specialRemoteConfigParsers baseRemoteConfigParser

baseRemoteConfigParser :: RemoteConfigParser
baseRemoteConfigParser = RemoteConfigParser
	{ remoteConfigFieldParsers =
		[ optionalStringParser externaltypeField
			(FieldDesc "type of external special remote to use")
		, trueFalseParser readonlyField (Just False)
			(FieldDesc "enable readonly mode")
		]
	, remoteConfigRestPassthrough = Just
		( const True
		, [("*", FieldDesc "all other parameters are passed to external special remote program")]
		)
	}

{- When the remote supports LISTCONFIGS, only accept the ones it listed.
 - When it does not, accept all configs. -}
strictRemoteConfigParser :: External -> Annex RemoteConfigParser
strictRemoteConfigParser external = listConfigs external >>= \case
	Nothing -> return lenientRemoteConfigParser
	Just l -> do
		let s = S.fromList (map fst l)
		let listed f = S.member (fromProposedAccepted f) s
		return $ lenientRemoteConfigParser
			{ remoteConfigRestPassthrough = Just (listed, l) }

listConfigs :: External -> Annex (Maybe [(Setting, FieldDesc)])
listConfigs external = handleRequest external LISTCONFIGS Nothing (collect [])
  where
	collect l req = case req of
		CONFIG s d -> Just $ return $
			GetNextMessage $ collect ((s, FieldDesc d) : l)
		CONFIGEND -> result (Just (reverse l))
		UNSUPPORTED_REQUEST -> result Nothing
		_ -> Nothing

remoteConfigParser :: RemoteConfig -> Annex RemoteConfigParser
remoteConfigParser c
	-- No need to start the external when there is no config to parse,
	-- or when everything in the config was already accepted; in those
	-- cases the lenient parser will do the same thing as the strict
	-- parser.
	| M.null (M.filter isproposed c) = return lenientRemoteConfigParser
	| otherwise = case parseRemoteConfig c baseRemoteConfigParser of
		Left _ -> return lenientRemoteConfigParser
		Right pc -> case (getRemoteConfigValue externaltypeField pc, getRemoteConfigValue readonlyField pc) of
			(Nothing, _) -> return lenientRemoteConfigParser
			(_, Just True) -> return lenientRemoteConfigParser
			(Just externaltype, _) -> do
				external <- newExternal externaltype Nothing pc Nothing Nothing
				strictRemoteConfigParser external
  where
	isproposed (Accepted _) = False
	isproposed (Proposed _) = True
