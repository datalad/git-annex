{- External special remote interface.
 -
 - Copyright 2013 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Remote.External (remote) where

import Remote.External.Types
import qualified Annex
import Common.Annex
import Types.Remote
import Types.CleanupActions
import Types.UrlContents
import qualified Git
import Config
import Remote.Helper.Special
import Utility.Metered
import Messages.Progress
import Logs.Transfer
import Logs.PreferredContent.Raw
import Logs.RemoteState
import Logs.Web
import Config.Cost
import Annex.UUID
import Creds

import Control.Concurrent.STM
import Control.Concurrent.Async
import System.Log.Logger (debugM)
import qualified Data.Map as M

remote :: RemoteType
remote = RemoteType {
	typename = "external",
	enumerate = const (findSpecialRemotes "externaltype"),
	generate = gen,
	setup = externalSetup
}

gen :: Git.Repo -> UUID -> RemoteConfig -> RemoteGitConfig -> Annex (Maybe Remote)
gen r u c gc = do
	external <- newExternal externaltype u c
	Annex.addCleanup (RemoteCleanup u) $ stopExternal external
	cst <- getCost external r gc
	avail <- getAvailability external r gc
	return $ Just $ specialRemote c
		(simplyPrepare $ store external)
		(simplyPrepare $ retrieve external)
		(simplyPrepare $ remove external)
		(simplyPrepare $ checkKey external)
		Remote
			{ uuid = u
			, cost = cst
			, name = Git.repoDescribe r
			, storeKey = storeKeyDummy
			, retrieveKeyFile = retreiveKeyFileDummy
			, retrieveKeyFileCheap = \_ _ _ -> return False
			, removeKey = removeKeyDummy
			, checkPresent = checkPresentDummy
			, checkPresentCheap = False
			, whereisKey = Just $ whereis external
			, remoteFsck = Nothing
			, repairRepo = Nothing
			, config = c
			, localpath = Nothing
			, repo = r
			, gitconfig = gc
			, readonly = False
			, availability = avail
			, remotetype = remote
			, mkUnavailable = gen r u c $
				gc { remoteAnnexExternalType = Just "!dne!" }
			, getInfo = return [("externaltype", externaltype)]
			, claimUrl = Just (claimurl external)
			, checkUrl = Just (checkurl external)
			}
  where
	externaltype = fromMaybe (error "missing externaltype") (remoteAnnexExternalType gc)

externalSetup :: Maybe UUID -> Maybe CredPair -> RemoteConfig -> Annex (RemoteConfig, UUID)
externalSetup mu _ c = do
	u <- maybe (liftIO genUUID) return mu
	let externaltype = fromMaybe (error "Specify externaltype=") $
		M.lookup "externaltype" c
	(c', _encsetup) <- encryptionSetup c

	external <- newExternal externaltype u c'
	handleRequest external INITREMOTE Nothing $ \resp -> case resp of
		INITREMOTE_SUCCESS -> Just noop
		INITREMOTE_FAILURE errmsg -> Just $ error errmsg
		_ -> Nothing
	c'' <- liftIO $ atomically $ readTMVar $ externalConfig external

	gitConfigSpecialRemote u c'' "externaltype" externaltype
	return (c'', u)

store :: External -> Storer
store external = fileStorer $ \k f p ->
	handleRequest external (TRANSFER Upload k f) (Just p) $ \resp ->
		case resp of
			TRANSFER_SUCCESS Upload k' | k == k' ->
				Just $ return True
			TRANSFER_FAILURE Upload k' errmsg | k == k' ->
				Just $ do
					warning errmsg
					return False
			_ -> Nothing

retrieve :: External -> Retriever
retrieve external = fileRetriever $ \d k p -> 
	handleRequest external (TRANSFER Download k d) (Just p) $ \resp ->
		case resp of
			TRANSFER_SUCCESS Download k'
				| k == k' -> Just $ return ()
			TRANSFER_FAILURE Download k' errmsg
				| k == k' -> Just $ do
					error errmsg
			_ -> Nothing

remove :: External -> Remover
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

checkKey :: External -> CheckPresent
checkKey external k = either error id <$> go
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

whereis :: External -> Key -> Annex [String]
whereis external k = handleRequest external (WHEREIS k) Nothing $ \resp -> case resp of
	WHEREIS_SUCCESS s -> Just $ return [s]
	WHEREIS_FAILURE -> Just $ return []
	UNSUPPORTED_REQUEST -> Just $ return []
	_ -> Nothing

safely :: Annex Bool -> Annex Bool
safely a = go =<< tryNonAsync a
  where
	go (Right r) = return r
	go (Left e) = do
		warning $ show e
		return False

{- Sends a Request to the external remote, and waits for it to generate
 - a Response. That is fed into the responsehandler, which should return
 - the action to run for it (or Nothing if there's a protocol error).
 -
 - While the external remote is processing the Request, it may send
 - any number of RemoteRequests, that are handled here.
 -
 - Only one request can be made at a time, so locking is used.
 -
 - May throw exceptions, for example on protocol errors, or
 - when the repository cannot be used.
 -}
handleRequest :: External -> Request -> Maybe MeterUpdate -> (Response -> Maybe (Annex a)) -> Annex a
handleRequest external req mp responsehandler = 
	withExternalLock external $ \lck ->
		handleRequest' lck external req mp responsehandler

handleRequest' :: ExternalLock -> External -> Request -> Maybe MeterUpdate -> (Response -> Maybe (Annex a)) -> Annex a
handleRequest' lck external req mp responsehandler
	| needsPREPARE req = do
		checkPrepared lck external
		go
	| otherwise = go
  where
	go = do
		sendMessage lck external req
		loop
	loop = receiveMessage lck external responsehandler
		(\rreq -> Just $ handleRemoteRequest rreq >> loop)
		(\msg -> Just $ handleAsyncMessage msg >> loop)

	handleRemoteRequest (PROGRESS bytesprocessed) =
		maybe noop (\a -> liftIO $ a bytesprocessed) mp
	handleRemoteRequest (DIRHASH k) = 
		send $ VALUE $ hashDirMixed def k
	handleRemoteRequest (SETCONFIG setting value) =
		liftIO $ atomically $ do
			let v = externalConfig external
			m <- takeTMVar v
			putTMVar v $ M.insert setting value m
	handleRemoteRequest (GETCONFIG setting) = do
		value <- fromMaybe "" . M.lookup setting
			<$> liftIO (atomically $ readTMVar $ externalConfig external)
		send $ VALUE value
	handleRemoteRequest (SETCREDS setting login password) = do
		c <- liftIO $ atomically $ readTMVar $ externalConfig external
		c' <- setRemoteCredPair encryptionAlreadySetup c (credstorage setting) $
			Just (login, password)
		void $ liftIO $ atomically $ swapTMVar (externalConfig external) c'
	handleRemoteRequest (GETCREDS setting) = do
		c <- liftIO $ atomically $ readTMVar $ externalConfig external
		creds <- fromMaybe ("", "") <$> 
			getRemoteCredPair c (credstorage setting)
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
		setRemoteState (externalUUID external) key state
	handleRemoteRequest (GETSTATE key) = do
		state <- fromMaybe ""
			<$> getRemoteState (externalUUID external) key
		send $ VALUE state
	handleRemoteRequest (SETURLPRESENT key url) =
		setUrlPresent (externalUUID external) key url
	handleRemoteRequest (SETURLMISSING key url) =
		setUrlMissing (externalUUID external) key url
	handleRemoteRequest (SETURIPRESENT key uri) =
		withurl (SETURLPRESENT key) uri
	handleRemoteRequest (SETURIMISSING key uri) =
		withurl (SETURLMISSING key) uri
	handleRemoteRequest (GETURLS key prefix) = do
		mapM_ (send . VALUE) =<< getUrlsWithPrefix key prefix
		send (VALUE "") -- end of list
	handleRemoteRequest (DEBUG msg) = liftIO $ debugM "external" msg
	handleRemoteRequest (VERSION _) =
		sendMessage lck external $ ERROR "too late to send VERSION"

	handleAsyncMessage (ERROR err) = error $ "external special remote error: " ++ err

	send = sendMessage lck external

	credstorage setting = CredPairStorage
		{ credPairFile = base
		, credPairEnvironment = (base ++ "login", base ++ "password")
		, credPairRemoteKey = Just setting
		}
	  where
		base = replace "/" "_" $ fromUUID (externalUUID external) ++ "-" ++ setting
			
	withurl mk uri = handleRemoteRequest $ mk $
		setDownloader (show uri) OtherDownloader

sendMessage :: Sendable m => ExternalLock -> External -> m -> Annex ()
sendMessage lck external m = 
	fromExternal lck external externalSend $ \h ->
		liftIO $ do
			protocolDebug external True line
			hPutStrLn h line
			hFlush h
  where
	line = unwords $ formatMessage m

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
receiveMessage lck external handleresponse handlerequest handleasync =
	go =<< fromExternal lck external externalReceive
		(liftIO . catchMaybeIO . hGetLine)
  where
	go Nothing = protocolError False ""
	go (Just s) = do
		liftIO $ protocolDebug external False s
		case parseMessage s :: Maybe Response of
			Just resp -> maybe (protocolError True s) id (handleresponse resp)
			Nothing -> case parseMessage s :: Maybe RemoteRequest of
				Just req -> maybe (protocolError True s) id (handlerequest req)
				Nothing -> case parseMessage s :: Maybe AsyncMessage of
					Just msg -> maybe (protocolError True s) id (handleasync msg)
					Nothing -> protocolError False s
	protocolError parsed s = error $ "external special remote protocol error, unexpectedly received \"" ++ s ++ "\" " ++
		if parsed then "(command not allowed at this time)" else "(unable to parse command)"

protocolDebug :: External -> Bool -> String -> IO ()
protocolDebug external sendto line = debugM "external" $ unwords
	[ externalRemoteProgram (externalType external)
	, if sendto then "<--" else "-->"
	, line
	]

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
		void $ liftIO $ atomically $ do
			void $ tryReadTMVar v
			putTMVar v st

		{- Handle initial protocol startup; check the VERSION
		 - the remote sends. -}
		receiveMessage lck external
			(const Nothing)
			(checkVersion lck external)
			(const Nothing)

		run st

	run st = a $ extractor st
	v = externalState external

{- Starts an external remote process running, but does not handle checking
 - VERSION, etc. -}
startExternal :: ExternalType -> Annex ExternalState
startExternal externaltype = do
	errrelayer <- mkStderrRelayer
	liftIO $ do
		(Just hin, Just hout, Just herr, pid) <- createProcess $
			(proc cmd [])
				{ std_in = CreatePipe
				, std_out = CreatePipe
				, std_err = CreatePipe
				}
		fileEncoding hin
		fileEncoding hout
		fileEncoding herr
		stderrelay <- async $ errrelayer herr
		checkearlytermination =<< getProcessExitCode pid
		return $ ExternalState
			{ externalSend = hin
			, externalReceive = hout
			, externalShutdown = do
				cancel stderrelay
				void $ waitForProcess pid
			, externalPrepared = Unprepared
			}
  where
	cmd = externalRemoteProgram externaltype

	checkearlytermination Nothing = noop
	checkearlytermination (Just exitcode) = ifM (inPath cmd)
		( error $ unwords [ "failed to run", cmd, "(" ++ show exitcode ++ ")" ]
		, do
			path <- intercalate ":" <$> getSearchPath
			error $ cmd ++ " is not installed in PATH (" ++ path ++ ")"
		)

stopExternal :: External -> Annex ()
stopExternal external = liftIO $ stop =<< atomically (tryReadTMVar v)
  where
	stop Nothing = noop
	stop (Just st) = do
		void $ atomically $ tryTakeTMVar v
		hClose $ externalSend st
		hClose $ externalReceive st
		externalShutdown st
	v = externalState external

externalRemoteProgram :: ExternalType -> String
externalRemoteProgram externaltype = "git-annex-remote-" ++ externaltype

checkVersion :: ExternalLock -> External -> RemoteRequest -> Maybe (Annex ())
checkVersion lck external (VERSION v) = Just $
	if v `elem` supportedProtocolVersions
		then noop
		else sendMessage lck external (ERROR "unsupported VERSION")
checkVersion _ _ _ = Nothing

{- If repo has not been prepared, sends PREPARE.
 -
 - If the repo fails to prepare, or failed before, throws an exception with
 - the error message. -}
checkPrepared :: ExternalLock -> External -> Annex ()
checkPrepared lck external = 
	fromExternal lck external externalPrepared $ \prepared ->
		case prepared of
			Prepared -> noop
			FailedPrepare errmsg -> error errmsg
			Unprepared -> 
				handleRequest' lck external PREPARE Nothing $ \resp ->
					case resp of
						PREPARE_SUCCESS -> Just $
							setprepared Prepared
						PREPARE_FAILURE errmsg -> Just $ do
							setprepared $ FailedPrepare errmsg
							error errmsg
						_ -> Nothing
  where
	setprepared status = liftIO . atomically $ do
		let v = externalState external
		st <- takeTMVar v
		void $ putTMVar v $ st { externalPrepared = status }

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
			UNSUPPORTED_REQUEST -> Just $ return expensiveRemoteCost
			_ -> Nothing
		setRemoteCost r c
		return c

{- Caches the availability in the git config to avoid needing to start up an
 - external special remote every time time just to ask it what its
 - availability is.
 -
 - Most remotes do not bother to implement a reply to this request;
 - globally available is the default.
 -}
getAvailability :: External -> Git.Repo -> RemoteGitConfig -> Annex Availability
getAvailability external r gc = maybe query return (remoteAnnexAvailability gc)
  where
	query = do
		avail <- handleRequest external GETAVAILABILITY Nothing $ \req -> case req of
			AVAILABILITY avail -> Just $ return avail
			UNSUPPORTED_REQUEST -> Just $ return GloballyAvailable
			_ -> Nothing
		setRemoteAvailability r avail
		return avail

claimurl :: External -> URLString -> Annex Bool
claimurl external url =
	handleRequest external (CLAIMURL url) Nothing $ \req -> case req of
		CLAIMURL_SUCCESS -> Just $ return True
		CLAIMURL_FAILURE -> Just $ return False
		UNSUPPORTED_REQUEST -> Just $ return False
		_ -> Nothing

checkurl :: External -> URLString -> Annex UrlContents
checkurl external url = 
	handleRequest external (CHECKURL url) Nothing $ \req -> case req of
		CHECKURL_CONTENTS sz f -> Just $ return $ UrlContents sz
			(if null f then Nothing else Just $ mkSafeFilePath f)
		-- Treat a single item multi response specially to
		-- simplify the external remote implementation.
		CHECKURL_MULTI ((_, sz, f):[]) ->
			Just $ return $ UrlContents sz $ Just $ mkSafeFilePath f
		CHECKURL_MULTI l -> Just $ return $ UrlMulti $ map mkmulti l
		CHECKURL_FAILURE errmsg -> Just $ error errmsg
		UNSUPPORTED_REQUEST -> error "CHECKURL not implemented by external special remote"
		_ -> Nothing
  where
	mkmulti (u, s, f) = (u, s, mkSafeFilePath f)
