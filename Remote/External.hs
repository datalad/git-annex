{- External special remote interface.
 -
 - Copyright 2013-2016 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Remote.External (remote) where

import Remote.External.Types
import qualified Annex
import Annex.Common
import Types.Remote
import Types.CleanupActions
import Types.UrlContents
import qualified Git
import Config
import Git.Config (isTrue, boolConfig)
import Git.Env
import Remote.Helper.Special
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
remote = RemoteType {
	typename = "external",
	enumerate = const (findSpecialRemotes "externaltype"),
	generate = gen,
	setup = externalSetup
}

gen :: Git.Repo -> UUID -> RemoteConfig -> RemoteGitConfig -> Annex (Maybe Remote)
gen r u c gc
	-- readonly mode only downloads urls; does not use external program
	| remoteAnnexReadOnly gc = do
		cst <- remoteCost gc expensiveRemoteCost
		mk cst GloballyAvailable
			readonlyStorer
			retrieveUrl
			readonlyRemoveKey
			(checkKeyUrl r)
			Nothing
			Nothing
			Nothing
	| otherwise = do
		external <- newExternal externaltype u c gc
		Annex.addCleanup (RemoteCleanup u) $ stopExternal external
		cst <- getCost external r gc
		avail <- getAvailability external r gc
		mk cst avail
			(store external)
			(retrieve external)
			(remove external)
			(checkKey external)
			(Just (whereis external))
			(Just (claimurl external))
			(Just (checkurl external))
  where
	mk cst avail tostore toretrieve toremove tocheckkey towhereis toclaimurl tocheckurl = do
		let rmt = Remote
			{ uuid = u
			, cost = cst
			, name = Git.repoDescribe r
			, storeKey = storeKeyDummy
			, retrieveKeyFile = retreiveKeyFileDummy
			, retrieveKeyFileCheap = \_ _ _ -> return False
			, removeKey = removeKeyDummy
			, lockContent = Nothing
			, checkPresent = checkPresentDummy
			, checkPresentCheap = False
			, whereisKey = towhereis
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
			, claimUrl = toclaimurl
			, checkUrl = tocheckurl
			}
		return $ Just $ specialRemote c
			(simplyPrepare tostore)
			(simplyPrepare toretrieve)
			(simplyPrepare toremove)
			(simplyPrepare tocheckkey)
			rmt
	externaltype = fromMaybe (error "missing externaltype") (remoteAnnexExternalType gc)

externalSetup :: Maybe UUID -> Maybe CredPair -> RemoteConfig -> RemoteGitConfig -> Annex (RemoteConfig, UUID)
externalSetup mu _ c gc = do
	u <- maybe (liftIO genUUID) return mu
	let externaltype = fromMaybe (error "Specify externaltype=") $
		M.lookup "externaltype" c
	(c', _encsetup) <- encryptionSetup c gc

	c'' <- case M.lookup "readonly" c of
		Just v | isTrue v == Just True -> do
			setConfig (remoteConfig (fromJust (M.lookup "name" c)) "readonly") (boolConfig True)
			return c'
		_ -> do
			external <- newExternal externaltype u c' gc
			handleRequest external INITREMOTE Nothing $ \resp -> case resp of
				INITREMOTE_SUCCESS -> Just noop
				INITREMOTE_FAILURE errmsg -> Just $ error errmsg
				_ -> Nothing
			withExternalState external $
				liftIO . atomically . readTMVar . externalConfig

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
 - An external remote process can only handle one request at a time.
 - Concurrent requests will start up additional processes.
 -
 - May throw exceptions, for example on protocol errors, or
 - when the repository cannot be used.
 -}
handleRequest :: External -> Request -> Maybe MeterUpdate -> (Response -> Maybe (Annex a)) -> Annex a
handleRequest external req mp responsehandler = 
	withExternalState external $ \st -> 
		handleRequest' st external req mp responsehandler

handleRequest' :: ExternalState -> External -> Request -> Maybe MeterUpdate -> (Response -> Maybe (Annex a)) -> Annex a
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
		liftIO $ atomically $ do
			let v = externalConfig st
			m <- takeTMVar v
			putTMVar v $ M.insert setting value m
	handleRemoteRequest (GETCONFIG setting) = do
		value <- fromMaybe "" . M.lookup setting
			<$> liftIO (atomically $ readTMVar $ externalConfig st)
		send $ VALUE value
	handleRemoteRequest (SETCREDS setting login password) = do
		let v = externalConfig st
		c <- liftIO $ atomically $ readTMVar v
		let gc = externalGitConfig external
		c' <- setRemoteCredPair encryptionAlreadySetup c gc
			(credstorage setting)
			(Just (login, password))
		void $ liftIO $ atomically $ swapTMVar v c'
	handleRemoteRequest (GETCREDS setting) = do
		c <- liftIO $ atomically $ readTMVar $ externalConfig st
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
		sendMessage st external (ERROR "too late to send VERSION")

	handleAsyncMessage (ERROR err) = error $ "external special remote error: " ++ err

	send = sendMessage st external

	credstorage setting = CredPairStorage
		{ credPairFile = base
		, credPairEnvironment = (base ++ "login", base ++ "password")
		, credPairRemoteKey = Just setting
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

{- Waits for a message from the external remote, and passes it to the
 - apppropriate handler. 
 -
 - If the handler returns Nothing, this is a protocol error.-}
receiveMessage
	:: ExternalState
	-> External 
	-> (Response -> Maybe (Annex a))
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
			Just resp -> maybe (protocolError True s) id (handleresponse resp)
			Nothing -> case parseMessage s :: Maybe RemoteRequest of
				Just req -> maybe (protocolError True s) id (handlerequest req)
				Nothing -> case parseMessage s :: Maybe AsyncMessage of
					Just msg -> maybe (protocolError True s) id (handleasync msg)
					Nothing -> protocolError False s
	protocolError parsed s = error $ "external special remote protocol error, unexpectedly received \"" ++ s ++ "\" " ++
		if parsed then "(command not allowed at this time)" else "(unable to parse command)"

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
			l <- takeTMVar v
			case l of
				[] -> do
					putTMVar v l
					return Nothing
				(st:rest) -> do
					putTMVar v rest
					return (Just st)
		maybe (startExternal external) return ms
	
	dealloc st = liftIO $ atomically $ do
		l <- takeTMVar v
		putTMVar v (st:l)

{- Starts an external remote process running, and checks VERSION. -}
startExternal :: External -> Annex ExternalState
startExternal external = do
	errrelayer <- mkStderrRelayer
	st <- start errrelayer =<< Annex.gitRepo
	receiveMessage st external
		(const Nothing)
		(checkVersion st external)
		(const Nothing)
	return st
  where
	start errrelayer g = liftIO $ do
		(cmd, ps) <- findShellCommand basecmd
		let basep = (proc cmd (toCommand ps))
			{ std_in = CreatePipe
			, std_out = CreatePipe
			, std_err = CreatePipe
			}
		p <- propgit g basep
		(Just hin, Just hout, Just herr, ph) <- 
			createProcess p `catchIO` runerr
		fileEncoding hin
		fileEncoding hout
		fileEncoding herr
		stderrelay <- async $ errrelayer herr
		checkearlytermination =<< getProcessExitCode ph
		cv <- newTMVarIO $ externalDefaultConfig external
		pv <- newTMVarIO Unprepared
		pid <- atomically $ do
			n <- succ <$> takeTMVar (externalLastPid external)
			putTMVar (externalLastPid external) n
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

	runerr _ = error ("Cannot run " ++ basecmd ++ " -- Make sure it's in your PATH and is executable.")

	checkearlytermination Nothing = noop
	checkearlytermination (Just exitcode) = ifM (inPath basecmd)
		( error $ unwords [ "failed to run", basecmd, "(" ++ show exitcode ++ ")" ]
		, do
			path <- intercalate ":" <$> getSearchPath
			error $ basecmd ++ " is not installed in PATH (" ++ path ++ ")"
		)

stopExternal :: External -> Annex ()
stopExternal external = liftIO $ do
	l <- atomically $ do
		l <- takeTMVar v
		putTMVar v []
		return l
	mapM_ stop l
  where
	stop st = do
		hClose $ externalSend st
		hClose $ externalReceive st
		externalShutdown st
	v = externalState external

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
	v <- liftIO $ atomically $ readTMVar $ externalPrepared st
	case v of
		Prepared -> noop
		FailedPrepare errmsg -> error errmsg
		Unprepared ->
			handleRequest' st external PREPARE Nothing $ \resp ->
				case resp of
					PREPARE_SUCCESS -> Just $
						setprepared Prepared
					PREPARE_FAILURE errmsg -> Just $ do
						setprepared $ FailedPrepare errmsg
						error errmsg
					_ -> Nothing
  where
	setprepared status = liftIO $ atomically $ void $
		swapTMVar (externalPrepared st) status

{- Caches the cost in the git config to avoid needing to start up an
 - external special remote every time time just to ask it what its
 - cost is. -}
getCost :: External -> Git.Repo -> RemoteGitConfig -> Annex Cost
getCost external r gc = catchNonAsync (go =<< remoteCost' gc) (const defcst)
  where
	go (Just c) = return c
	go Nothing = do
		c <- handleRequest external GETCOST Nothing $ \req -> case req of
			COST c -> Just $ return c
			UNSUPPORTED_REQUEST -> Just defcst
			_ -> Nothing
		setRemoteCost r c
		return c
	defcst = return expensiveRemoteCost

{- Caches the availability in the git config to avoid needing to start up an
 - external special remote every time time just to ask it what its
 - availability is.
 -
 - Most remotes do not bother to implement a reply to this request;
 - globally available is the default.
 -}
getAvailability :: External -> Git.Repo -> RemoteGitConfig -> Annex Availability
getAvailability external r gc = 
	maybe (catchNonAsync query (const defavail)) return (remoteAnnexAvailability gc)
  where
	query = do
		avail <- handleRequest external GETAVAILABILITY Nothing $ \req -> case req of
			AVAILABILITY avail -> Just $ return avail
			UNSUPPORTED_REQUEST -> Just defavail
			_ -> Nothing
		setRemoteAvailability r avail
		return avail
	defavail = return GloballyAvailable

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

retrieveUrl :: Retriever
retrieveUrl = fileRetriever $ \f k p -> do
	us <- getWebUrls k
	unlessM (downloadUrl k p us f) $
		error "failed to download content"

checkKeyUrl :: Git.Repo -> CheckPresent
checkKeyUrl r k = do
	showChecking r
	us <- getWebUrls k
	anyM (\u -> withUrlOptions $ checkBoth u (keySize k)) us

getWebUrls :: Key -> Annex [URLString]
getWebUrls key = filter supported <$> getUrls key
  where
	supported u = snd (getDownloader u) == WebDownloader
