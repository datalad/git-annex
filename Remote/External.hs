{- External special remote interface.
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Remote.External (remote) where

import Remote.External.Types
import qualified Annex
import Common.Annex
import Types.Remote
import qualified Git
import Config
import Remote.Helper.Special
import Remote.Helper.Encryptable
import Crypto
import Utility.Metered
import Logs.Transfer
import Config.Cost
import Annex.Content
import Annex.UUID
import Annex.Exception

import Control.Concurrent.STM
import System.Process (std_in, std_out, std_err)
import qualified Data.Map as M

remote :: RemoteType
remote = RemoteType {
	typename = "external",
	enumerate = findSpecialRemotes "externaltype",
	generate = gen,
	setup = externalSetup
}

gen :: Git.Repo -> UUID -> RemoteConfig -> RemoteGitConfig -> Annex (Maybe Remote)
gen r u c gc = do
	external <- newExternal externaltype
	Annex.addCleanup (fromUUID u) $ stopExternal external
	cst <- getCost external r gc
	return $ Just $ encryptableRemote c
		(storeEncrypted external $ getGpgEncParams (c,gc))
		(retrieveEncrypted external)
		Remote {
			uuid = u,
			cost = cst,
			name = Git.repoDescribe r,
			storeKey = store external,
			retrieveKeyFile = retrieve external,
			retrieveKeyFileCheap = retrieveCheap,
			removeKey = remove external,
			hasKey = checkPresent external,
			hasKeyCheap = False,
			whereisKey = Nothing,
			remoteFsck = Nothing,
			repairRepo = Nothing,
			config = c,
			localpath = Nothing,
			repo = r,
			gitconfig = gc,
			readonly = False,
			globallyAvailable = False,
			remotetype = remote
		}
  where
	externaltype = fromMaybe (error "missing externaltype") $ remoteAnnexExternalType gc

externalSetup :: Maybe UUID -> RemoteConfig -> Annex (RemoteConfig, UUID)
externalSetup mu c = do
	u <- maybe (liftIO genUUID) return mu
	let externaltype = fromMaybe (error "Specify externaltype=") $
		M.lookup "externaltype" c
	c' <- encryptionSetup c

	external <- newExternal externaltype
	handleRequest external INITREMOTE Nothing $ \resp -> case resp of
		INITREMOTE_SUCCESS -> Just noop
		INITREMOTE_FAILURE errmsg -> Just $ error errmsg
		_ -> Nothing

	gitConfigSpecialRemote u c' "externaltype" externaltype
	return (c', u)

store :: External -> Key -> AssociatedFile -> MeterUpdate -> Annex Bool
store external k _f p = safely $ sendAnnex k rollback $ \f ->
	handleRequest external (TRANSFER Upload k f) (Just p) $ \resp ->
		case resp of
			TRANSFER_SUCCESS Upload k'
				| k == k' -> Just $ return True
			TRANSFER_FAILURE Upload k' errmsg
				| k == k' -> Just $ do
					warning errmsg
					return False
			_ -> Nothing
  where
	rollback = void $ remove external k

storeEncrypted :: External -> [CommandParam] -> (Cipher, Key) -> Key -> MeterUpdate -> Annex Bool
storeEncrypted external gpgOpts (cipher, enck) k _p = safely $ undefined

retrieve :: External -> Key -> AssociatedFile -> FilePath -> MeterUpdate -> Annex Bool
retrieve external k _f d p = safely $
	handleRequest external (TRANSFER Download k d) (Just p) $ \resp ->
		case resp of
			TRANSFER_SUCCESS Download k'
				| k == k' -> Just $ return True
			TRANSFER_FAILURE Download k' errmsg
				| k == k' -> Just $ do
					warning errmsg
					return False
			_ -> Nothing

retrieveCheap :: Key -> FilePath -> Annex Bool
retrieveCheap _ _ = return False

retrieveEncrypted :: External -> (Cipher, Key) -> Key -> FilePath -> MeterUpdate -> Annex Bool
retrieveEncrypted external (cipher, enck) _ f _p = safely $ undefined

remove :: External -> Key -> Annex Bool
remove external k = safely $ 
	handleRequest external (REMOVE k) Nothing $ \resp ->
		case resp of
			REMOVE_SUCCESS k'
				| k == k' -> Just $ return True
			REMOVE_FAILURE k' errmsg
				| k == k' -> Just $ do
					warning errmsg
					return False
			_ -> Nothing

checkPresent :: External -> Key -> Annex (Either String Bool)
checkPresent external k = either (Left . show) id <$> tryAnnex go
  where
	go = handleRequest external (CHECKPRESENT k) Nothing $ \resp ->
		case resp of
			CHECKPRESENT_SUCCESS k'
				| k' == k -> Just $ return $ Right True
			CHECKPRESENT_FAILURE k'
				| k' == k -> Just $ return $ Right False
			CHECKPRESENT_UNKNOWN k' errmsg
				| k' == k -> Just $ return $ Left errmsg
			_ -> Nothing

safely :: Annex Bool -> Annex Bool
safely a = go =<< tryAnnex a
  where
	go (Right r) = return r
	go (Left e) = do
		warning $ show e
		return False

{- Sends a Request to the external remote, and waits for it to generate
 - a Response that the responsehandler accepts.
 -
 - While the external remote is processing the Request, it may send
 - any number of RemoteRequests, that are handled here.
 -
 - Only one request can be made at a time, so locking is used.
 -
 - May throw exceptions, for example on protocol errors.
 -}
handleRequest :: External -> Request -> Maybe MeterUpdate -> (Response -> Maybe (Annex a)) -> Annex a
handleRequest external req mp responsehandler = 
	withExternalLock external $ \lck ->
		handleRequest' lck external req mp responsehandler

handleRequest' :: ExternalLock -> External -> Request -> Maybe MeterUpdate -> (Response -> Maybe (Annex a)) -> Annex a
handleRequest' lck external req mp responsehandler = do
	sendMessage lck external req
	loop
  where
	loop = receiveMessage lck external responsehandler
		(\rreq -> Just $ handleRemoteRequest rreq >> loop)
		(\msg -> Just $ handleAsyncMessage msg >> loop)

	handleRemoteRequest (PROGRESS bytesprocessed) =
		maybe noop (\a -> liftIO $ a bytesprocessed) mp
	handleRemoteRequest (DIRHASH k) = 
		sendMessage lck external (VALUE $ hashDirMixed k)
	handleRemoteRequest (SETCONFIG setting value) = error "TODO"
	handleRemoteRequest (GETCONFIG setting) = error "TODO"
	handleRemoteRequest (SETSTATE k value) = error "TODO"
	handleRemoteRequest (GETSTATE k) = error "TODO"
	handleRemoteRequest (VERSION _) =
		sendMessage lck external (ERROR "too late to send VERSION")

	handleAsyncMessage (ERROR err) = error $ "external special remote error: " ++ err

sendMessage :: Sendable m => ExternalLock -> External -> m -> Annex ()
sendMessage lck external m = 
	fromExternal lck external externalSend $ \h ->
		liftIO $ hPutStrLn h $ unwords $ formatMessage m

{- Waits for a message from the external remote, and passes it to the
 - apppropriate handler. 
 -
 - If the handler returns Nothing, this is a protocol error.-}
receiveMessage
	:: ExternalLock
	-> External 
	-> (Response -> Maybe (Annex a))
	-> (RemoteRequest -> Maybe (Annex a))
	-> (AsyncMessage -> Maybe (Annex a))
	-> Annex a
receiveMessage lck external handleresponse handlerequest handleasync = do
	s <- fromExternal lck external externalReceive $ liftIO . hGetLine
	case parseMessage s :: Maybe Response of
		Just resp -> maybe (protocolError s) id (handleresponse resp)
		Nothing -> case parseMessage s :: Maybe RemoteRequest of
			Just req -> maybe (protocolError s) id (handlerequest req)
			Nothing -> case parseMessage s :: Maybe AsyncMessage of
				Just msg -> maybe (protocolError s) id (handleasync msg)
				Nothing -> protocolError s
  where
	protocolError s = error $ "external special remote protocol error, unexpectedly received \"" ++ s ++ "\""

{- Starts up the external remote if it's not yet running,
 - and passes a value extracted from its state to an action.
 -}
fromExternal :: ExternalLock -> External -> (ExternalState -> v) -> (v -> Annex a) -> Annex a
fromExternal lck external extractor a =
	go =<< liftIO (atomically (tryReadTMVar v))
  where
	go (Just st) = run st
	go Nothing = do
		st <- startExternal $ externalType external
		void $ liftIO $ atomically $ swapTMVar v st

		{- Handle initial protocol startup; check the VERSION
		 - the remote sends, and send it the PREPARE request. -}
		receiveMessage lck external
			(const Nothing)
			(checkVersion lck external)
			(const Nothing)
		handleRequest' lck external PREPARE Nothing $ \resp -> 
			case resp of
				PREPARE_SUCCESS -> Just $ run st
				_ -> Nothing

	run st = a $ extractor st
	v = externalState external

{- Starts an external remote process running, but does not handle checking
 - VERSION, etc. -}
startExternal :: ExternalType -> Annex ExternalState
startExternal externaltype = liftIO $ do
	(Just hin, Just hout, _, pid) <- createProcess $
		(proc (externalRemoteProgram externaltype) [])
			{ std_in = CreatePipe
			, std_out = CreatePipe
			, std_err = Inherit
			}
	fileEncoding hin
	fileEncoding hout
	return $ ExternalState
		{ externalSend = hin
		, externalReceive = hout
		, externalPid = pid
		}

stopExternal :: External -> Annex ()
stopExternal external = liftIO $ stop =<< atomically (tryReadTMVar v)
  where
	stop Nothing = noop
	stop (Just st) = do
		void $ atomically $ tryTakeTMVar v
		hClose $ externalSend st
		hClose $ externalReceive st
		void $ waitForProcess $ externalPid st
	v = externalState external

externalRemoteProgram :: ExternalType -> String
externalRemoteProgram externaltype = "git-annex-remote-" ++ externaltype

checkVersion :: ExternalLock -> External -> RemoteRequest -> Maybe (Annex ())
checkVersion lck external (VERSION v) = Just $
	if v `elem` supportedProtocolVersions
		then noop
		else sendMessage lck external (ERROR "unsupported VERSION")
checkVersion _ _ _ = Nothing

{- Caches the cost in the git config to avoid needing to start up an
 - external special remote every time time just to ask it what its
 - cost is. -}
getCost :: External -> Git.Repo -> RemoteGitConfig -> Annex Cost
getCost external r gc = go =<< remoteCost' gc
  where
	go (Just c) = return c
	go Nothing = do
		c <- handleRequest external GETCOST Nothing $ \req -> case req of
			COST c -> Just $ return c
			COST_UNKNOWN -> Just $ return expensiveRemoteCost
			_ -> Nothing
		setRemoteCost r c
		return c
