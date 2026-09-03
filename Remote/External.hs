{- External special remote interface.
 -
 - Copyright 2013-2026 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RankNTypes #-}

module Remote.External where

import Remote.External.Types
import Remote.External.AsyncExtension
import qualified Annex
import Annex.Common
import qualified Annex.ExternalAddonProcess as AddonProcess
import Types.Remote
import Types.RemoteState
import Types.Import
import Types.Export
import Types.CleanupActions
import Types.UrlContents
import Types.ProposedAccepted
import Types.GitConfig
import qualified Git
import qualified Git.Construct
import Config
import Git.Config (boolConfig)
import Annex.SpecialRemote.Config
import Remote.Helper.Special
import Remote.Helper.ExportImport
import Remote.Helper.ReadOnly
import Utility.Metered
import Utility.Hash
import Utility.Tmp
import Types.Transfer
import Logs.PreferredContent.Raw
import Logs.RemoteState
import Logs.Web
import Logs.Remote
import Logs.File
import Config.Cost
import Annex.Content
import Annex.Url
import Annex.UUID
import Annex.Verify
import Annex.DisableRemote
import Annex.LockFile
import Creds
import Messages.Progress
import qualified Utility.FileIO as F

import Control.Concurrent.STM
import qualified Data.Map as M
import qualified Data.Set as S

remote :: RemoteType
remote = specialRemoteType $ RemoteType
	{ typename = "external"
	, enumerate = const (findSpecialRemotes "externaltype")
	, generate = gen remote Nothing
	, configParser = remoteConfigParser Nothing
	, setup = externalSetup Nothing Nothing
	, exportSupported = checkSupportedWith Nothing checkExportSupported
	, importSupported = checkSupportedWith Nothing checkImportSupported
	, exportImportSupported = exportImportUnsupported
	, thirdPartyPopulated = False
	}

externaltypeField :: RemoteConfigField
externaltypeField = Accepted "externaltype"

readonlyField :: RemoteConfigField
readonlyField = Accepted "readonly"

gen
	:: RemoteType
	-> Maybe ExternalProgram
	-> Git.Repo
	-> UUID
	-> RemoteConfig
	-> RemoteGitConfig
	-> RemoteStateHandle
	-> Annex (Maybe Remote)
gen rt externalprogram r u rc gc rs
	-- readonly mode only downloads urls; does not use external program
	| externalprogram' == ExternalType "readonly" = do
		c <- parsedRemoteConfig remote rc
		cst <- remoteCost gc c expensiveRemoteCost
		let rmt = mk c cst (pure True) (pure GloballyAvailable)
			Nothing
			(externalInfo externalprogram')
			Nothing
			Nothing
			exportUnsupported
			importUnsupported
			exportUnsupported
			importUnsupported
		return $ Just $ specialRemote c
			readonlyStorer
			(retrieveUrlReadOnly gc)
			readonlyRemoveKey
			(checkKeyUrlReadOnly gc)
			rmt
	| otherwise = do
		c <- parsedRemoteConfig remote rc
		external <- newExternal externalprogram' (Just u) c (Just gc)
			(Git.remoteName r) (Just rs)
		Annex.addCleanupAction (RemoteCleanup u) $ stopExternal external
		cst <- getCost external r gc c
		exportsupported <- if exportTree c
			then isExportSupported' <$> checkExportSupported (Just external)
			else return False
		importsupported <- if importTree c
			then isImportSupported' <$> checkImportSupported (Just external)
			else return False
		let exportactions = if exportsupported
			then ExportActions
				{ storeExport = storeExportM external
				, retrieveExport = retrieveExportM external gc
				, removeExport = removeExportM external
				, checkPresentExport = checkPresentExportM external gc
				, removeExportDirectory = Just $ removeExportDirectoryM external
				, renameExport = Just $ renameExportM external
				}
			else exportUnsupported
		let importactions = if importsupported
			then ImportActions
				{ listImportableContents = listImportableContentsM external
				, importKey = importKeyM external
				, retrieveImport = retrieveImportM external gc
				, checkPresentImport = checkPresentImportM external gc
				}
			else importUnsupported
		-- Replace the expensive checks now that we've already
		-- checked them.
		let cheapexportsupported = if exportsupported
			then exportIsSupported
			else exportUnsupported
		let cheapimportsupported = if importsupported
			then importIsSupported
			else importUnsupported
		let rmt = mk c cst
			(getOrdered external)
			(getAvailability external)
			(Just (whereisKeyM external))
			(getInfoM external)
			(Just (claimUrlM external))
			(Just (checkUrlM external))
			exportactions
			importactions
			cheapexportsupported
			cheapimportsupported
		return $ Just $ specialRemote c
			(storeKeyM external)
			(retrieveKeyFileM external gc)
			(removeKeyM external)
			(checkPresentM external gc)
			rmt
  where
	mk c cst ordered avail towhereis togetinfo toclaimurl tocheckurl exportactions importactions cheapexportsupported cheapimportsupported =
		Remote
			{ uuid = u
			, cost = cst
			, name = Git.repoDescribe r
			, storeKey = storeKeyDummy
			, retrieveKeyFile = retrieveKeyFileDummy
			, retrieveKeyFileInOrder = ordered
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
			, importActions = importactions
			, exportImportActions = exportImportUnsupported
			, whereisKey = towhereis
			, remoteFsck = Nothing
			, repairKey = Nothing
			, repairRepo = Nothing
			, config = c
			, localpath = Nothing
			, getRepo = return r
			, gitconfig = gc
			, readonly = False
			, appendonly = False
			, untrustworthy = False
			, availability = avail
			, remotetype = rt 
				{ exportSupported = cheapexportsupported
				, importSupported = cheapimportsupported
				}
			, mkUnavailable =
				let dneprogram = case externalprogram of
					Just (ExternalCommand _ _) -> Just (ExternalType "!dne!")
					_ -> Nothing
				    dnegc = gc { remoteAnnexExternalType = Just "!dne!" }
				in gen rt dneprogram r u rc dnegc rs
			, getInfo = togetinfo
			, claimUrl = toclaimurl
			, checkUrl = tocheckurl
			, remoteStateHandle = rs
			}
	externalprogram' = case externalprogram of
		Just p -> p
		Nothing -> ExternalType $ 
			fromMaybe (giveup "missing externaltype")
				(remoteAnnexExternalType gc)

externalSetup
	:: Maybe ExternalProgram
	-> Maybe (String, String)
	-> SetupStage
	-> Maybe UUID
	-> RemoteName
	-> Maybe CredPair
	-> RemoteConfig
	-> RemoteGitConfig
	-> Annex (RemoteConfig, UUID)
externalSetup externalprogram setgitconfig ss mu remotename _ c gc = do
	u <- maybe (liftIO genUUID) return mu
	pc <- either giveup return $ parseRemoteConfig c (lenientRemoteConfigParser externalprogram)
	let readonlyconfig = getRemoteConfigValue readonlyField pc == Just True
	let externaltype = if readonlyconfig
		then "readonly"
		else fromMaybe (giveup "Specify externaltype=") $
			getRemoteConfigValue externaltypeField pc
	(c', _encsetup) <- encryptionSetup ss c gc

	c'' <- if readonlyconfig
		then do
			-- Setting annex-readonly is not really necessary
			-- anymore, but older versions of git-annex used
			-- this, not externaltype=readonly, so still set
			-- it.
			setConfig (remoteAnnexConfig (fromJust (lookupName c)) "readonly") (boolConfig True)
			return c'
		else do
			pc' <- either giveup return $ parseRemoteConfig c' (lenientRemoteConfigParser externalprogram)
			let p = fromMaybe (ExternalType externaltype) externalprogram
			external <- newExternal p (Just u) pc' (Just gc) (Just remotename) Nothing
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
				liftIO . atomically . readTMVar . externalConfigChanges
			return (changes c')

	gitConfigSpecialRemote u c''
		[ fromMaybe ("externaltype", externaltype) setgitconfig ]
	return (M.delete readonlyField c'', u)

checkSupportedWith
	:: Maybe ExternalProgram
	-> (Maybe External -> Annex a)
	-> ParsedRemoteConfig
	-> RemoteGitConfig
	-> Annex a
checkSupportedWith Nothing checker c gc = do
	let externaltype = fromMaybe (giveup "Specify externaltype=") $
		remoteAnnexExternalType gc <|> getRemoteConfigValue externaltypeField c
	if externaltype == "readonly"
		then checker Nothing
		else checkSupportedWith (Just (ExternalType externaltype)) checker c gc
checkSupportedWith (Just externalprogram) checker c gc = 
	checker . Just
		=<< newExternal externalprogram Nothing c (Just gc) Nothing Nothing

checkExportSupported :: Maybe External -> Annex ExportSupported
checkExportSupported (Just external) = go
	`catchNonAsync` (const (return (ExportSupported False)))
  where
	go = handleRequest external EXPORTSUPPORTED Nothing $ \resp -> case resp of
		EXPORTSUPPORTED_SUCCESS -> result (ExportSupported True)
		EXPORTSUPPORTED_FAILURE -> result (ExportSupported False)
		UNSUPPORTED_REQUEST -> result (ExportSupported False)
		_ -> Nothing
checkExportSupported Nothing = return (ExportSupported False)

checkImportSupported :: Maybe External -> Annex ImportSupported
checkImportSupported (Just external) = go
	`catchNonAsync` (const (return (ImportSupported False)))
  where
	go = handleRequest external IMPORTSUPPORTED Nothing $ \resp -> case resp of
		IMPORTSUPPORTED_SUCCESS -> result (ImportSupported True)
		IMPORTSUPPORTED_FAILURE -> result (ImportSupported False)
		IMPORTREQUIRED -> result ImportRequired
		UNSUPPORTED_REQUEST -> result (ImportSupported False)
		_ -> Nothing
checkImportSupported Nothing = return (ImportSupported False)

storeKeyM :: External -> Storer
storeKeyM external = fileStorer $ \k f p ->
	either giveup return =<< go k f p
		(\sk -> TRANSFER Upload sk (fromOsPath f))
  where
	go k f p mkreq = handleRequestKey external mkreq k (Just p) $ \resp ->
		case resp of
			TRANSFER_SUCCESS Upload k' | k == k' ->
				result (Right ())
			TRANSFER_FAILURE Upload k' errmsg | k == k' ->
				result (Left (respErrorMessage "TRANSFER" errmsg))
			DELEGATE ps -> getResult $ do
				delegate <- getDelegateRemote external ps
				storeKey delegate k (AssociatedFile Nothing) (Just f) p	
				return (Right ())
			_ -> Nothing

retrieveKeyFileM :: External -> RemoteGitConfig -> Retriever
retrieveKeyFileM external gc = fileRetriever $ \dest k p ->
	either giveup return =<< watchFileSize dest p (go dest k)
  where
	go dest k p = handleRequestKey external (\sk -> TRANSFER Download sk (fromOsPath dest)) k (Just p) $ \resp ->
		case resp of
			TRANSFER_SUCCESS Download k'
				| k == k' -> result $ Right ()
			TRANSFER_FAILURE Download k' errmsg
				| k == k' -> result $ Left $
					respErrorMessage "TRANSFER" errmsg
			TRANSFER_RETRIEVE_URL k' url
				| k == k' -> getResult $ retrieveUrl gc url dest k p
			DELEGATE ps -> getResult $ do
				delegate <- getDelegateRemote external ps
				_ <- retrieveKeyFile delegate k
					(AssociatedFile Nothing) dest p
					NoVerify
				return (Right ())
			_ -> Nothing

removeKeyM :: External -> Remover
removeKeyM external proof k = either giveup return =<< go
  where
	go = handleRequestKey external REMOVE k Nothing $ \resp ->
		case resp of
			REMOVE_SUCCESS k'
				| k == k' -> result $ Right ()
			REMOVE_FAILURE k' errmsg
				| k == k' -> result $ Left $
					respErrorMessage "REMOVE" errmsg
			DELEGATE ps -> getResult $ do
				delegate <- getDelegateRemote external ps
				_ <- removeKey delegate proof k
				return (Right ())
			_ -> Nothing

checkPresentM :: External -> RemoteGitConfig -> CheckPresent
checkPresentM external gc k = either giveup id <$> go
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
			CHECKPRESENT_URL k' url
				| k == k' -> checkKeyUrl gc k url
			DELEGATE ps -> Just $ do
				delegate <- getDelegateRemote external ps
				Result . Right <$> checkPresent delegate k
			_ -> Nothing

whereisKeyM :: External -> Key -> Annex [String]
whereisKeyM external k = handleRequestKey external WHEREIS k Nothing $ \resp -> case resp of
	WHEREIS_SUCCESS s -> result [s]
	WHEREIS_FAILURE -> result []
	DELEGATE ps -> Just $ do
		delegate <- getDelegateRemote external ps
		case whereisKey delegate of
			Just a -> Result <$> a k
			Nothing -> return (Result [])
	UNSUPPORTED_REQUEST -> result []
	_ -> Nothing

storeExportM
	:: External
	-> OsPath
	-> Key
	-> ExportLocation
	-> MeterUpdate
	-> Annex ()
storeExportM external f k loc p = either giveup return =<< go
  where
	go = handleRequestExport external loc req k (Just p) $ \resp -> case resp of
		TRANSFER_SUCCESS Upload k' | k == k' -> result $ Right ()
		TRANSFER_FAILURE Upload k' errmsg | k == k' ->
			result $ Left $ respErrorMessage "TRANSFER" errmsg
		DELEGATE ps -> getResult $ do
			delegate <- getDelegateRemote external ps
			_ <- storeExport (exportActions delegate) f k loc p
			return (Right ())
		UNSUPPORTED_REQUEST -> 
			result $ Left "TRANSFEREXPORT not implemented by external special remote"
		_ -> Nothing
	req sk = TRANSFEREXPORT Upload sk (fromOsPath f)

retrieveExportM
	:: External
	-> RemoteGitConfig
	-> Key
	-> ExportLocation
	-> OsPath
	-> MeterUpdate
	-> Annex Verification
retrieveExportM external gc k loc dest p = do
	verifyKeyContentIncrementally AlwaysVerify k $ \iv ->
		tailVerify iv dest $
			either giveup return =<< go
  where
	go = handleRequestExport external loc req k (Just p) $ \resp -> case resp of
		TRANSFER_SUCCESS Download k'
			| k == k' -> result $ Right ()
		TRANSFER_FAILURE Download k' errmsg
			| k == k' -> result $ Left $ respErrorMessage "TRANSFER" errmsg
		TRANSFER_RETRIEVE_URL k' url
			| k == k' -> Just $ Result <$> retrieveUrl gc url dest k p
		DELEGATE ps -> getResult $ do
			delegate <- getDelegateRemote external ps
			_ <- retrieveExport (exportActions delegate) k loc dest p
			return (Right ())
		UNSUPPORTED_REQUEST ->
			result $ Left "TRANSFEREXPORT not implemented by external special remote"
		_ -> Nothing
	req sk = TRANSFEREXPORT Download sk (fromOsPath dest)

retrieveImportM
	:: External
	-> RemoteGitConfig
	-> ImportLocation
	-> [ContentIdentifier]
	-> OsPath
	-> Either Key (Annex Key)
	-> MeterUpdate
	-> Annex (Key, Verification)
retrieveImportM external gc loc cids dest gk p =
	case gk of
		Right _ -> do
			k <- go Nothing
			return (k, UnVerified)
		Left k -> verifyKeyContentIncrementally' AlwaysVerify k go
  where
	go iv = tailVerify iv dest $
		either giveup return =<< go'
	go' = handleRequestImport' external loc req (Just p) $ \resp -> case resp of
		RETRIEVEIMPORT_SUCCESS -> getResult $
			Right <$> either pure id gk
		RETRIEVEIMPORT_FAILURE errmsg -> 
			result $ Left $ respErrorMessage "RETRIEVEIMPORT" errmsg
		RETRIEVEIMPORT_URL url -> getResult $ do
			retrieveUrl gc url dest UnknownSize p >>= \case
				Right () -> Right <$> either pure id gk
				Left msg -> pure (Left msg)
		DELEGATE ps -> getResult $ do
			delegate <- getDelegateRemote external ps
			Right . fst <$> retrieveImport (importActions delegate) loc cids dest gk p
		UNSUPPORTED_REQUEST ->
			result $ Left "RETRIEVEIMPORT not implemented by external special remote"
		_ -> Nothing
	req = RETRIEVEIMPORT (fromOsPath dest)

checkPresentExportM
	:: External
	-> RemoteGitConfig
	-> Key
	-> ExportLocation
	-> Annex Bool
checkPresentExportM = checkPresentExportImport
	CHECKPRESENTEXPORT
	"CHECKPRESENTEXPORT"
	(checkPresentExport . exportActions)
	handleRequestExport

checkPresentImportM
	:: External
	-> RemoteGitConfig
	-> Key
	-> ExportLocation
	-> Annex Bool
checkPresentImportM = checkPresentExportImport
	CHECKPRESENTIMPORT
	"CHECKPRESENTIMPORT"
	(checkPresentImport . importActions)
	handleRequestImport

checkPresentExportImport
	:: (SafeKey -> Request)
	-> String
	-> (Remote -> Key -> ExportLocation -> Annex Bool)
	-> (External -> ImportLocation -> (SafeKey -> Request) -> Key -> Maybe MeterUpdate -> ResponseHandler (Either String Bool) -> Annex (Either String Bool))
	-> External
	-> RemoteGitConfig
	-> Key
	-> ExportLocation
	-> Annex Bool
checkPresentExportImport request srequest delegateaction handlereq external gc k loc = either giveup id <$> go
  where
	go = handlereq external loc request k Nothing $ \resp -> case resp of
		CHECKPRESENT_SUCCESS k'
			| k' == k -> result $ Right True
		CHECKPRESENT_FAILURE k'
			| k' == k -> result $ Right False
		CHECKPRESENT_UNKNOWN k' errmsg
			| k' == k -> result $ Left $
				respErrorMessage srequest errmsg
		CHECKPRESENT_URL k' url
			| k == k' -> checkKeyUrl gc k url
		DELEGATE ps -> Just $ do
			delegate <- getDelegateRemote external ps
			Result . Right <$> delegateaction delegate k loc
		UNSUPPORTED_REQUEST -> result $
			Left $ srequest ++ " not implemented by external special remote"
		_ -> Nothing

removeExportM :: External -> Key -> ExportLocation -> Annex ()
removeExportM external k loc = either giveup return =<< go
  where
	go = handleRequestExport external loc REMOVEEXPORT k Nothing $ \resp -> case resp of
		REMOVE_SUCCESS k'
			| k == k' -> result $ Right ()
		REMOVE_FAILURE k' errmsg
			| k == k' -> result $ Left $ respErrorMessage "REMOVE" errmsg
		DELEGATE ps -> getResult $ do
			delegate <- getDelegateRemote external ps
			_ <- removeExport (exportActions delegate) k loc
			return (Right ())
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
		DELEGATE ps -> getResult $ do
			delegate <- getDelegateRemote external ps
			case removeExportDirectory (exportActions delegate) of
				Just a -> a dir
				Nothing -> return ()
			return (Right ())
		UNSUPPORTED_REQUEST -> result $ Right ()
		_ -> Nothing
	req = REMOVEEXPORTDIRECTORY dir

renameExportM
	:: External
	-> Key
	-> ExportLocation
	-> ExportLocation
	-> Annex (Maybe ())
renameExportM external k src dest = either giveup return =<< go
  where
	go = handleRequestExport external src req k Nothing $ \resp -> case resp of
		RENAMEEXPORT_SUCCESS k'
			| k' == k -> result $ Right (Just ())
		RENAMEEXPORT_FAILURE k' 
			| k' == k -> result $ Left "failed to rename exported file"
		DELEGATE ps -> getResult $ do
			delegate <- getDelegateRemote external ps
			case renameExport (exportActions delegate) of
				Just a -> Right <$> a k src dest
				Nothing -> return $ Right Nothing
		UNSUPPORTED_REQUEST -> result (Right Nothing)
		_ -> Nothing
	req sk = RENAMEEXPORT sk dest

listImportableContentsM
	:: External
	-> Annex (Maybe (ImportableContentsChunkable Annex (ContentIdentifier, ByteSize)))
listImportableContentsM external =
	handleRequest external LISTIMPORTABLECONTENTS Nothing
		(go [] Nothing)
  where
	go c _ (IMPORTABLECONTENT sz loc) = 
		let loc' = mkImportLocation (toOsPath loc)
		in Just $ return $ GetNextMessage $
			go c (Just (sz, loc'))
	go c (Just (sz, loc)) (IMPORTABLECONTENTIDENTIFIER cid) =
		Just $ return $ GetNextMessage $ 
			go ((loc, (cid, sz)):c) Nothing
	go c _ LISTIMPORTABLECONTENTS_SUCCESS =
		result $ Just $
			ImportableContentsComplete $ ImportableContents
				{ importableContents = c
				, importableHistory = []
				}
	go _ _ (LISTIMPORTABLECONTENTS_FAILURE err) =
		giveup err
	go _ _ (DELEGATE ps) = Just $ do
		delegate <- getDelegateRemote external ps
		Result <$> listImportableContents (importActions delegate)
	go _ _ UNSUPPORTED_REQUEST = result Nothing
	go _ _ _ = Nothing

importKeyM
	:: External
	-> Annex (Maybe (ImportLocation -> ContentIdentifier -> ByteSize -> MeterUpdate -> Annex (Maybe Key)))
importKeyM external = 
	withExternalState external $ \st ->
		return $ if importKeyExtensionEnabled (externalExtensions st)
			then Just go
			else Nothing
  where
	go loc cid sz p =
		handleRequestImport' external loc (IMPORTKEY sz cid) Nothing $ \case
			IMPORTKEY_SUCCESS k ->
				result (Just k)
			IMPORTKEY_FAILURE err ->
				giveup err
			IMPORTKEY_SKIP ->
				result Nothing
			DELEGATE ps -> Just $ do
				delegate <- getDelegateRemote external ps
				importKey (importActions delegate) >>= \case
					Just a -> Result <$> a loc cid sz p
					Nothing -> giveup "IMPORTKEY delegated to a special remote that does not support it"
			UNSUPPORTED_REQUEST ->
				giveup "IMPORTKEY not implemented by external special remote, but it claimed to support it"
			_ -> Nothing

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
handleRequest
	:: External
	-> Request
	-> Maybe MeterUpdate
	-> ResponseHandler a
	-> Annex a
handleRequest external req mp responsehandler = 
	withExternalState external $ \st -> 
		handleRequest' st external req mp responsehandler

handleRequestKey
	:: External
	-> (SafeKey -> Request)
	-> Key
	-> Maybe MeterUpdate
	-> ResponseHandler a
	-> Annex a
handleRequestKey external mkreq k mp responsehandler = 
	withSafeKey k $ \sk -> handleRequest external (mkreq sk) mp responsehandler

withSafeKey :: Key -> (SafeKey -> Annex a) -> Annex a
withSafeKey k a = case mkSafeKey k of
	Right sk -> a sk
	Left e -> giveup e

handleRequestExport
	:: External
	-> ExportLocation
	-> (SafeKey -> Request)
	-> Key
	-> Maybe MeterUpdate
	-> ResponseHandler a
	-> Annex a
handleRequestExport = handleRequestExportImport EXPORT

handleRequestImport
	:: External
	-> ImportLocation
	-> (SafeKey -> Request)
	-> Key
	-> Maybe MeterUpdate
	-> ResponseHandler a
	-> Annex a
handleRequestImport = handleRequestExportImport IMPORT

handleRequestImport'
	:: External
	-> ImportLocation
	-> Request
	-> Maybe MeterUpdate
	-> ResponseHandler a
	-> Annex a
handleRequestImport' = handleRequestExportImport' IMPORT

handleRequestExportImport
	:: (ExportLocation -> Request)
	-> External
	-> ImportLocation
	-> (SafeKey -> Request)
	-> Key
	-> Maybe MeterUpdate
	-> ResponseHandler a
	-> Annex a
handleRequestExportImport mklocrequest external loc mkreq k mp responsehandler = 
	withSafeKey k $ \sk ->
		handleRequestExportImport' mklocrequest external loc (mkreq sk) mp responsehandler

handleRequestExportImport'
	:: (ExportLocation -> Request)
	-> External
	-> ImportLocation
	-> Request
	-> Maybe MeterUpdate
	-> ResponseHandler a
	-> Annex a
handleRequestExportImport' mklocrequest external loc req mp responsehandler = 
	-- Both the location request and subsequent request must be
	-- sent to the same external process, so run both with the
	-- same external state.
	withExternalState external $ \st -> do
		checkPrepared st external
		sendMessage st (mklocrequest loc)
		handleRequest' st external req mp responsehandler

handleRequest'
	:: ExternalState
	-> External
	-> Request
	-> Maybe MeterUpdate
	-> ResponseHandler a
	-> Annex a
handleRequest' st external req mp responsehandler
	| needsPREPARE req = do
		checkPrepared st external
		go
	| otherwise = go
  where
	go = do
		sendMessage st req
		cleanupv <- liftIO $ atomically $ newTMVar []
		loop cleanupv
			`finally` cleanup cleanupv
	
	loop cleanupv = receiveMessage st external responsehandler
		(\rreq -> Just $ handleRemoteRequest cleanupv rreq >> loop cleanupv)
		(\msg -> Just $ handleExceptionalMessage msg >> loop cleanupv)

	cleanup cleanupv = liftIO $
		sequence =<< atomically (takeTMVar cleanupv)

	handleRemoteRequest _ (PROGRESS bytesprocessed) =
		maybe noop (\a -> liftIO $ a bytesprocessed) mp
	handleRemoteRequest _ (DIRHASH k) = 
		send $ VALUE $ fromOsPath $ hashDirMixed def k
	handleRemoteRequest _ (DIRHASH_LOWER k) = 
		send $ VALUE $ fromOsPath $ hashDirLower def k
	handleRemoteRequest _ (SETCONFIG setting value) =
		liftIO $ atomically $ do
			ParsedRemoteConfig m c <- takeTMVar (externalConfig st)
			let !m' = M.insert
				(Accepted setting)
				(RemoteConfigValue (PassedThrough value))
				m
			let !c' = M.insert
			    	(Accepted setting)
				(Accepted value)
				c
			putTMVar (externalConfig st) (ParsedRemoteConfig m' c')
			f <- takeTMVar (externalConfigChanges st)
			let !f' = M.insert (Accepted setting) (Accepted value) . f
			putTMVar (externalConfigChanges st) f'
	handleRemoteRequest _ (GETCONFIG setting) = do
		value <- maybe "" fromProposedAccepted
			. (M.lookup (Accepted setting))
			. unparsedRemoteConfig
			<$> liftIO (atomically $ readTMVar $ externalConfig st)
		send $ VALUE value
	handleRemoteRequest _ (SETCREDS setting login password) = case (externalUUID external, externalGitConfig external) of
		(Just u, Just gc) -> do
			pc <- liftIO $ atomically $ takeTMVar (externalConfig st)
			pc' <- setRemoteCredPair' pc encryptionAlreadySetup gc
				(credstorage setting u)
				(Just (login, password))
			let configchanges = M.differenceWithKey
				(\_k a b -> if a == b then Nothing else Just a)
				(unparsedRemoteConfig pc')
				(unparsedRemoteConfig pc)
			void $ liftIO $ atomically $ do
				putTMVar (externalConfig st) pc'
				f <- takeTMVar (externalConfigChanges st)
				let !f' = M.union configchanges . f
				putTMVar (externalConfigChanges st) f'
		_ -> senderror "cannot send SETCREDS here"
	handleRemoteRequest _ (GETCREDS setting) = case (externalUUID external, externalGitConfig external) of
		(Just u, Just gc) -> do
			c <- liftIO $ atomically $ readTMVar $ externalConfig st
			creds <- fromMaybe ("", "") <$> 
				getRemoteCredPair c gc (credstorage setting u)
			send $ CREDS (fst creds) (snd creds)
		_ -> senderror "cannot send GETCREDS here"
	handleRemoteRequest _ GETUUID = case externalUUID external of
		Just u -> send $ VALUE $ fromUUID u
		Nothing -> senderror "cannot send GETUUID here"
	handleRemoteRequest _ GETGITDIR = 
		send . VALUE . fromOsPath =<< fromRepo Git.localGitDir
	handleRemoteRequest _ GETGITREMOTENAME =
		case externalRemoteName external of
			Just n -> send $ VALUE n
			Nothing -> senderror "git remote name not known"
	handleRemoteRequest _ (SETWANTED expr) = case externalUUID external of
		Just u -> preferredContentSet u expr
		Nothing -> senderror "cannot send SETWANTED here"
	handleRemoteRequest _ GETWANTED = case externalUUID external of
		Just u -> do
			expr <- fromMaybe "" . M.lookup u
				<$> preferredContentMapRaw
			send $ VALUE expr
		Nothing -> senderror "cannot send GETWANTED here"
	handleRemoteRequest _ (SETSTATE key state) =
		case externalRemoteStateHandle external of
			Just h -> setRemoteState h key state
			Nothing -> senderror "cannot send SETSTATE here"
	handleRemoteRequest _ (GETSTATE key) =
		case externalRemoteStateHandle external of
			Just h -> do
				state <- fromMaybe ""
					<$> getRemoteState h key
				send $ VALUE state
			Nothing -> senderror "cannot send GETSTATE here"
	handleRemoteRequest _ (SETURLPRESENT key url) =
		setUrlPresent key url
	handleRemoteRequest _ (SETURLMISSING key url) =
		setUrlMissing key url
	handleRemoteRequest cleanupv (SETURIPRESENT key uri) =
		withurl cleanupv (SETURLPRESENT key) uri
	handleRemoteRequest cleanupv (SETURIMISSING key uri) =
		withurl cleanupv (SETURLMISSING key) uri
	handleRemoteRequest _ (GETURLS key prefix) = do
		mapM_ (send . VALUE) =<< getUrlsWithPrefix key prefix
		send (VALUE "") -- end of list
	handleRemoteRequest _ (DEBUG msg) = fastDebug "Remote.External" msg
	handleRemoteRequest _ (INFO msg) = showInfo (UnquotedString msg)
	handleRemoteRequest cleanupv (DOWNLOAD_URL url) = do
		case externalGitConfig external of
			Just gc -> do
				(tmpf, h) <- liftIO $ do
					tmpdir <- systemTmpDirectory
					openTmpFileIn tmpdir (literalOsPath "url")
				liftIO $ hClose h
				liftIO $ atomically $ do
					l <- takeTMVar cleanupv
					putTMVar cleanupv (removeTmpFile tmpf:l)
				res <- withUrlOptionsPromptingCreds (Just gc) $
					downloadUrl' False UnknownSize 
						nullMeterUpdate Nothing [url]
						tmpf
				case res of
					Right True -> 
						send $ DOWNLOAD_URL_SUCCESS (fromOsPath tmpf)
					Left err -> 
						send $ DOWNLOAD_URL_FAILURE err
					Right False -> 
						send $ DOWNLOAD_URL_FAILURE "download failed"
			_ -> senderror "cannot send DOWNLOAD-URL here"
	handleRemoteRequest _ (VERSION _) = senderror "too late to send VERSION"

	handleExceptionalMessage (ERROR err) = giveup $ "external special remote error: " ++ err

	send = sendMessage st
	senderror = sendMessage st . ERROR 

	credstorage setting u = CredPairStorage
		{ credPairFile = toOsPath base
		, credPairEnvironment = (base ++ "login", base ++ "password")
		, credPairRemoteField = Accepted setting
		}
	  where
		base = replace "/" "_" $ fromUUID u ++ "-" ++ setting
	
	withurl cleanupv mk uri = handleRemoteRequest cleanupv $ mk $
		setDownloader (show uri) OtherDownloader

sendMessage
	:: (Sendable m, ToAsyncWrapped m)
	=> ExternalState
	-> m
	-> Annex ()
sendMessage st m = liftIO $ externalSend st m

sendMessageAddonProcess
	:: Sendable m
	=> AddonProcess.ExternalAddonProcess
	-> m
	-> IO ()
sendMessageAddonProcess p m = do
	AddonProcess.protocolDebug p True line
	hPutStrLn h line
	hFlush h
  where
	h = AddonProcess.externalSend p
	line = genMessage m

receiveMessageAddonProcess
	:: AddonProcess.ExternalAddonProcess
	-> IO (Maybe String)
receiveMessageAddonProcess p = do
	v <- catchMaybeIO $ hGetLine $ AddonProcess.externalReceive p
	maybe noop (AddonProcess.protocolDebug p False) v
	return v

shutdownAddonProcess :: AddonProcess.ExternalAddonProcess -> Bool -> IO ()
shutdownAddonProcess = AddonProcess.externalShutdown 

{- A response handler can yield a result, or it can request that another
 - message be consumed from the external. -}
data ResponseHandlerResult a
	= Result a
	| GetNextMessage (ResponseHandler a)

type ResponseHandler a = Response -> Maybe (Annex (ResponseHandlerResult a))

result :: a -> Maybe (Annex (ResponseHandlerResult a))
result = Just . return . Result

getResult :: Annex a -> Maybe (Annex (ResponseHandlerResult a))
getResult a = Just $ Result <$> a

{- Waits for a message from the external remote, and passes it to the
 - appropriate handler. 
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
	protocolError parsed s = do
		warning $ UnquotedString $ "external special remote protocol error, unexpectedly received \"" ++ s ++ "\" " ++
			if parsed
				then "(command not allowed at this time)"
				else "(unable to parse command)"
		giveup "unable to use special remote due to protocol error"

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
				`onException` storeasync UncheckedExternalAsync
			if asyncExtensionEnabled extensions
				then do
					annexrunner <- Annex.makeRunner
					relay <- liftIO $ runRelayToExternalAsync external st annexrunner
					st' <- liftIO $ asyncRelayExternalState relay
					storeasync (ExternalAsync relay)
					return st'
				else do
					storeasync NoExternalAsync
					return st
		v@NoExternalAsync -> do
			storeasync v
			fst <$> startExternal' external
		v@(ExternalAsync relay) -> do
			storeasync v
			liftIO $ asyncRelayExternalState relay
  where
	storeasync = liftIO . atomically . putTMVar (externalAsync external)

startExternal' :: External -> Annex (ExternalState, ExtensionList)
startExternal' external = do
	pid <- liftIO $ atomically $ do
		n <- succ <$> readTVar (externalLastPid external)
		writeTVar (externalLastPid external) n
		return n
	AddonProcess.startExternalAddonProcessProtocol externalcmd externalparams pid >>= \case
		Left (AddonProcess.ProgramFailure err) -> do
			unusable err
		Left (AddonProcess.ProgramNotInstalled err) ->
			case (lookupName (unparsedRemoteConfig (externalDefaultConfig external)), remoteAnnexReadOnly <$> externalGitConfig external) of
				(Just rname, Just True) -> unusable $ unlines
					[ err
					, "This remote has annex-readonly=true, and previous versions of"
					, "git-annex would try to download from it without"
					, "installing " ++ externalcmd ++ ". If you want that, you need to set:"
					, "git config remote." ++ rname ++ ".annex-externaltype readonly"
					]
				_ -> unusable err
		Right p -> do
			cv <- liftIO $ newTMVarIO $ externalDefaultConfig external
			ccv <- liftIO $ newTMVarIO id
			pv <- liftIO $ newTMVarIO Unprepared
			let st = ExternalState
				{ externalSend = sendMessageAddonProcess p
				, externalReceive = receiveMessageAddonProcess p
				, externalShutdown = shutdownAddonProcess p
				, externalPrepared = pv
				, externalConfig = cv
				, externalConfigChanges = ccv
				, externalExtensions = ExtensionList []
				}
			extensions <- startproto st
			return (st { externalExtensions = extensions }, extensions)
  where
	(externalcmd, externalparams) = case externalProgram external of
		ExternalType t -> ("git-annex-remote-" ++ t, [])
		ExternalCommand c ps -> (c, ps)
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
			exrest -> unusable $ unwords $
				[ externalcmd
				, "requested extensions that this version of git-annex does not support:"
				] ++ exrest

	unusable msg = do
		warning (UnquotedString msg)
		giveup ("unable to use external special remote " ++ externalcmd)

stopExternal :: External -> Annex ()
stopExternal external = do
	liftIO $ do
		l <- atomically $ swapTVar (externalState external) []
		mapM_ (flip externalShutdown False) l
	removeEphemeralDelegates external

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
	v <- liftIO $ atomically $ takeTMVar $ externalPrepared st
	case v of
		Prepared -> setprepared Prepared
		FailedPrepare errmsg -> do
			setprepared (FailedPrepare errmsg)
			giveup errmsg
		Unprepared ->
			handleRequest' st external PREPARE Nothing $ \resp ->
				case resp of
					PREPARE_SUCCESS -> getResult $
						setprepared Prepared
					PREPARE_FAILURE errmsg -> Just $ do
						let errmsg' = respErrorMessage "PREPARE" errmsg
						setprepared $ FailedPrepare errmsg'
						giveup errmsg'
					_ -> Nothing
  where
	setprepared status = liftIO $ atomically $
		putTMVar (externalPrepared st) status

respErrorMessage :: String -> String -> String
respErrorMessage req err
	| null err = req ++ " failed with no reason given"
	| otherwise = err

{- Caches the cost in the git config to avoid needing to start up an
 - external special remote every time time just to ask it what its
 - cost is. -}
getCost :: External -> Git.Repo -> RemoteGitConfig -> ParsedRemoteConfig -> Annex Cost
getCost external r gc pc =
	(go =<< remoteCost' gc pc) `catchNonAsync` const (pure defcst)
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

{- Most remotes do not bother to implement a reply to this request;
 - globally available is the default.
 -}
getAvailability :: External -> Annex Availability
getAvailability external = catchNonAsync query (const (pure defavail))
  where
	query = handleRequest external GETAVAILABILITY Nothing $ \req -> case req of
		AVAILABILITY avail -> result avail
		UNSUPPORTED_REQUEST -> result defavail
		_ -> Nothing
	defavail = GloballyAvailable

getOrdered :: External -> Annex Bool
getOrdered external = catchNonAsync query (const (pure False))
  where
	query = handleRequest external GETORDERED Nothing $ \req -> case req of
		ORDERED -> result True
		UNORDERED -> result False
		_ -> result False

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
			if null f then Nothing else Just (toOsPath f)
		CHECKURL_MULTI l -> result $ UrlMulti $ map mkmulti l
		CHECKURL_FAILURE errmsg -> Just $ giveup $
			respErrorMessage "CHECKURL" errmsg
		UNSUPPORTED_REQUEST -> giveup "CHECKURL not implemented by external special remote"
		_ -> Nothing
  where
	mkmulti (u, s, f) = (u, s, toOsPath f)

retrieveUrlReadOnly :: RemoteGitConfig -> Retriever
retrieveUrlReadOnly gc = fileRetriever' $ \f k p iv -> do
	us <- getWebUrls k
	unlessM (withUrlOptions (Just gc) $ downloadUrl True k p iv us f) $
		giveup downloadFailed

retrieveUrl :: MeterSize sizer => RemoteGitConfig -> URLString -> OsPath -> sizer -> MeterUpdate -> Annex (Either String ())
retrieveUrl gc url dest sizer p = 
	withUrlOptionsPromptingCreds (Just gc) $ \uo ->
		downloadUrl' False sizer p Nothing [url] dest uo >>= return . \case
			Left msg -> Left msg
			Right True -> Right ()
			Right False -> Left downloadFailed

downloadFailed :: String
downloadFailed = "failed to download content"

checkKeyUrlReadOnly :: RemoteGitConfig -> CheckPresent
checkKeyUrlReadOnly gc k = do
	us <- getWebUrls k
	anyM (\u -> withUrlOptions (Just gc) $ checkBoth u (fromKey keySize k)) us

checkKeyUrl :: RemoteGitConfig -> Key -> URLString -> Maybe (Annex (ResponseHandlerResult (Either String Bool)))
checkKeyUrl gc k url = 
	Just $ withUrlOptionsPromptingCreds (Just gc) $ \uo ->
		Result <$> checkBoth' url (fromKey keySize k) uo

getWebUrls :: Key -> Annex [URLString]
getWebUrls key = filter supported <$> getUrls key
  where
	supported u = snd (getDownloader u) == WebDownloader
			
externalInfo :: ExternalProgram -> Annex [(String, String)]
externalInfo (ExternalType et) = return [("externaltype", et)]
externalInfo (ExternalCommand _ _) = return []

getInfoM :: External -> Annex [(String, String)]
getInfoM external = (++)
	<$> externalInfo (externalProgram external)
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
lenientRemoteConfigParser :: Maybe ExternalProgram -> RemoteConfigParser
lenientRemoteConfigParser externalprogram =
	addRemoteConfigParser specialRemoteConfigParsers (baseRemoteConfigParser externalprogram)

baseRemoteConfigParser :: Maybe ExternalProgram -> RemoteConfigParser
baseRemoteConfigParser externalprogram = RemoteConfigParser
	{ remoteConfigFieldParsers = if isJust extcommand
		then []
		else 
			[ optionalStringParser externaltypeField
				(FieldDesc "type of external special remote to use")
			, trueFalseParser readonlyField (Just False)
				(FieldDesc "enable readonly mode")
			]
	, remoteConfigRestPassthrough = Just
		( const True
		, [("*", FieldDesc $ "all other parameters are passed to " ++ fromMaybe "external special remote program" extcommand)]
		)
	}
  where
	extcommand = case externalprogram of
		Just (ExternalCommand c _) -> Just c
		_ -> Nothing

{- When the remote supports LISTCONFIGS, only accept the ones it listed.
 - When it does not, accept all configs. -}
strictRemoteConfigParser :: External -> Annex RemoteConfigParser
strictRemoteConfigParser external = listConfigs external >>= \case
	Nothing -> return lcp
	Just l -> do
		let s = S.fromList (map fst l)
		let listed f = S.member (fromProposedAccepted f) s
		return $ lcp { remoteConfigRestPassthrough = Just (listed, l) }
  where
	lcp = lenientRemoteConfigParser (Just (externalProgram external))

listConfigs :: External -> Annex (Maybe [(Setting, FieldDesc)])
listConfigs external = handleRequest external LISTCONFIGS Nothing (collect [])
  where
	collect l req = case req of
		CONFIG s d -> Just $ return $
			GetNextMessage $ collect ((s, FieldDesc d) : l)
		CONFIGEND -> result (Just (reverse l))
		UNSUPPORTED_REQUEST -> result Nothing
		_ -> Nothing

remoteConfigParser :: Maybe ExternalProgram -> RemoteConfig -> Annex RemoteConfigParser
remoteConfigParser externalprogram c
	-- No need to start the external when there is no config to parse,
	-- or when everything in the config was already accepted; in those
	-- cases the lenient parser will do the same thing as the strict
	-- parser.
	| M.null (M.filter isproposed c) = return (lenientRemoteConfigParser externalprogram)
	| otherwise = case parseRemoteConfig c (baseRemoteConfigParser externalprogram) of
		Left _ -> return (lenientRemoteConfigParser externalprogram)
		Right pc -> case (getRemoteConfigValue externaltypeField pc, getRemoteConfigValue readonlyField pc) of
			(Nothing, _) -> return (lenientRemoteConfigParser externalprogram)
			(_, Just True) -> return (lenientRemoteConfigParser externalprogram)
			(Just externaltype, _) -> do
				let p = fromMaybe (ExternalType externaltype) externalprogram
				external <- newExternal p Nothing pc Nothing Nothing Nothing
				strictRemoteConfigParser external
  where
	isproposed (Accepted _) = False
	isproposed (Proposed _) = True

getDelegateRemote :: External -> [String] -> Annex Remote
getDelegateRemote external ps = do
	rs <- Annex.getState Annex.remotes
	case filter (\r -> name r == delegatename) rs of
		(r:_) -> return r
		_ -> do
			lockfile <- fromRepo $ gitAnnexRemoteLockFile externalu
			r <- withExclusiveLock lockfile $ do
				r <- gendelegate
				when isephemeral $ do
					statefile <- fromRepo $ gitAnnexRemoteStateFile externalu
					appendLogFile' statefile (encodeBL delegatename)
				return r
			when isephemeral $
				registerephemeral r
			return r
  where
	registerephemeral r = do
		-- Take a shared lock of the state file to indicate the
		-- remote is in use.
		statefile <- fromRepo $ gitAnnexRemoteStateFile externalu
		let lckvar = externalEphemeralDelegateLock external
		liftIO (atomically (takeTMVar lckvar)) >>= \case
			Just lck -> liftIO $ atomically $
				putTMVar lckvar (Just lck)
			Nothing -> do
				lck <- takeSharedLock statefile
				liftIO $ atomically $
					putTMVar lckvar (Just lck)
		liftIO $ atomically $ do
			l <- takeTMVar (externalEphemeralDelegates external)
			putTMVar (externalEphemeralDelegates external) (r:l)

	externalu = case externalUUID external of
		Just u -> u
		Nothing -> error "internal"

	(ps', isephemeral) = checkephemeral ps [] False

	checkephemeral [] c b = (reverse c, b)
	checkephemeral (p:rest) c b
		| p == "ephemeral=yes" = checkephemeral rest c True
		| p == "ephemeral=no" = checkephemeral rest c False
		| otherwise = checkephemeral rest (p:c) b

	-- Hash the configuration of the delegate remote, so
	-- re-using the same configuration yields the same name.
	delegatename = concat
		[ fromMaybe "external" (externalRemoteName external)
		, "-delegate-"
		, show $ digestToHash $ md5s $ encodeBS $ show ps
		]
	
	gendelegate = do
		c <- newConfig delegatename (Just (Sameas externalu))
			(keyValToConfig Proposed ps')
			<$> remoteConfigMap
		remotetypes <- Annex.getState Annex.remotetypes
		t <- either giveup return (findType' remotetypes c)
		dummycfg <- liftIO dummyRemoteGitConfig
		(c', u) <- setup t Init (Just externalu) delegatename Nothing c dummycfg
		
		setRemotePrivate c' True
		cu <- liftIO genUUID
		setRemoteConfigUUID c' cu
		Logs.Remote.configSet cu c'
		
		setRemoteSkipFetchAll c' True
		setRemoteIgnore c' True

		g <- liftIO $ Git.Construct.remoteNamed delegatename
			(pure Git.Construct.fromUnknown)
		gc <- Annex.getRemoteGitConfig g
		let rs = RemoteStateHandle cu
		r <- generate t g u c' gc rs >>= \case
			Nothing -> error "Failed to generate a delegate remote"
			Just r -> adjustExportImport r rs
		Annex.changeState $ \s -> s 
			{ Annex.remotes = r : Annex.remotes s
			}
		return r

removeEphemeralDelegates :: External -> Annex ()
removeEphemeralDelegates external = do
	let lckvar = externalEphemeralDelegateLock external
	liftIO (atomically (takeTMVar lckvar)) >>= \case
		Just sharedlck -> do
			liftIO $ dropLock sharedlck
			case externalUUID external of
				Just externalu -> go externalu
				Nothing -> return ()
		Nothing -> return ()
	liftIO $ atomically $ putTMVar lckvar Nothing
  where
	go externalu = do
		statefile <- fromRepo $ gitAnnexRemoteStateFile externalu
		lockfile <- fromRepo $ gitAnnexRemoteLockFile externalu
		-- Only remove them when no other process has a shared
		-- lock of the state file. (And when no other process
		-- is also removing them.)
		void $ tryExclusiveLock statefile $
			withExclusiveLock lockfile $ do
				ds <- liftIO $ nub . map decodeBL . fileLines
					<$> F.readFile statefile
				rs <- Annex.getState Annex.remotes
				rs' <- liftIO $ atomically $ readTMVar (externalEphemeralDelegates external)
				let rs'' = rs'++rs
				forM_ ds $ \delegatename ->
					case filter (\r -> name r == delegatename) rs'' of
						(r:_) -> disable r delegatename rs''
						_ -> return ()
				writeLogFile statefile ""
	
	disable r delegatename rs = 
		tryNonAsync (disableRemote r delegatename rs) >>= \case
			Right () -> return ()
			Left err -> do
				warning $ UnquotedString $ 
					"Unable to remove ephemeral delegate remote " ++ delegatename ++ ": " ++ show err
				return ()
