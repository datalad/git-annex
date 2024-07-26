{- P2P protocol over HTTP, server state
 -
 - https://git-annex.branchable.com/design/p2p_protocol_over_http/
 -
 - Copyright 2024 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module P2P.Http.State where

import Annex.Common
import qualified Annex
import P2P.Http.Types
import qualified P2P.Protocol as P2P
import qualified P2P.IO as P2P
import P2P.IO
import P2P.Annex
import Annex.UUID
import Types.NumCopies
import Types.WorkerPool
import Annex.WorkerPool
import Types.Cluster
import CmdLine.Action (startConcurrency)
import Utility.ThreadScheduler
import Utility.HumanTime
import Annex.Proxy
import qualified P2P.Proxy as Proxy
import qualified Types.Remote as Remote

import Servant
import qualified Data.Map as M
import qualified Data.Set as S
import Control.Concurrent.Async
import Control.Concurrent.STM

data P2PHttpServerState = P2PHttpServerState
	{ acquireP2PConnection :: AcquireP2PConnection
	, annexWorkerPool :: AnnexWorkerPool
	, getServerMode :: GetServerMode
	, openLocks :: TMVar (M.Map LockID Locker)
	}

type AnnexWorkerPool = TMVar (WorkerPool (Annex.AnnexState, Annex.AnnexRead))

-- Nothing when the server is not allowed to serve any requests.
type GetServerMode = IsSecure -> Maybe Auth -> Maybe P2P.ServerMode

mkP2PHttpServerState :: AcquireP2PConnection -> AnnexWorkerPool -> GetServerMode -> IO P2PHttpServerState
mkP2PHttpServerState acquireconn annexworkerpool getservermode = P2PHttpServerState
	<$> pure acquireconn
	<*> pure annexworkerpool
	<*> pure getservermode
	<*> newTMVarIO mempty

data ActionClass = ReadAction | WriteAction | RemoveAction
	deriving (Eq)

withP2PConnection
	:: APIVersion v
	=> v
	-> P2PHttpServerState
	-> B64UUID ClientSide
	-> B64UUID ServerSide
	-> [B64UUID Bypass]
	-> IsSecure
	-> Maybe Auth
	-> ActionClass
	-> (ConnectionParams -> ConnectionParams)
	-> (P2PConnectionPair -> Handler (Either ProtoFailure a))
	-> Handler a
withP2PConnection apiver st cu su bypass sec auth actionclass fconnparams connaction =
	withP2PConnection' apiver st cu su bypass sec auth actionclass fconnparams connaction'
  where
	connaction' conn = connaction conn >>= \case
		Right r -> return r
		Left err -> throwError $
			err500 { errBody = encodeBL (describeProtoFailure err) }

withP2PConnection'
	:: APIVersion v
	=> v
	-> P2PHttpServerState
	-> B64UUID ClientSide
	-> B64UUID ServerSide
	-> [B64UUID Bypass]
	-> IsSecure
	-> Maybe Auth
	-> ActionClass
	-> (ConnectionParams -> ConnectionParams)
	-> (P2PConnectionPair -> Handler a)
	-> Handler a
withP2PConnection' apiver st cu su bypass sec auth actionclass fconnparams connaction = do
	conn <- getP2PConnection apiver st cu su bypass sec auth actionclass fconnparams
	connaction conn
		`finally` liftIO (releaseP2PConnection conn)

getP2PConnection
	:: APIVersion v
	=> v
	-> P2PHttpServerState
	-> B64UUID ClientSide
	-> B64UUID ServerSide
	-> [B64UUID Bypass]
	-> IsSecure
	-> Maybe Auth
	-> ActionClass
	-> (ConnectionParams -> ConnectionParams)
	-> Handler P2PConnectionPair
getP2PConnection apiver st cu su bypass sec auth actionclass fconnparams =
	checkAuthActionClass st sec auth actionclass go
  where
	go servermode = liftIO (acquireP2PConnection st cp) >>= \case
		Left (ConnectionFailed err) -> 
			throwError err502 { errBody = encodeBL err }
		Left TooManyConnections ->
			throwError err503
		Right v -> return v
	  where
		cp = fconnparams $ ConnectionParams
			{ connectionProtocolVersion = protocolVersion apiver
			, connectionServerUUID = fromB64UUID su
			, connectionClientUUID = fromB64UUID cu
			, connectionBypass = map fromB64UUID bypass
			, connectionServerMode = servermode
			, connectionWaitVar = True
			}

checkAuthActionClass
	:: P2PHttpServerState
	-> IsSecure
	-> Maybe Auth
	-> ActionClass
	-> (P2P.ServerMode -> Handler a)
	-> Handler a
checkAuthActionClass st sec auth actionclass go =
	case (getServerMode st sec auth, actionclass) of
		(Just P2P.ServeReadWrite, _) -> go P2P.ServeReadWrite
		(Just P2P.ServeAppendOnly, RemoveAction) -> throwError err403
		(Just P2P.ServeAppendOnly, _) -> go P2P.ServeAppendOnly
		(Just P2P.ServeReadOnly, ReadAction) -> go P2P.ServeReadOnly
		(Just P2P.ServeReadOnly, _) -> throwError err403
		(Nothing, _) -> throwError basicAuthRequired

basicAuthRequired :: ServerError
basicAuthRequired = err401 { errHeaders = [(h, v)] }
  where
	h = "WWW-Authenticate"
	v = "Basic realm=\"git-annex\", charset=\"UTF-8\""

data ConnectionParams = ConnectionParams
	{ connectionProtocolVersion :: P2P.ProtocolVersion
	, connectionServerUUID :: UUID
	, connectionClientUUID :: UUID
	, connectionBypass :: [UUID]
	, connectionServerMode :: P2P.ServerMode
	, connectionWaitVar :: Bool
	}
	deriving (Show, Eq, Ord)

data ConnectionProblem
	= ConnectionFailed String
	| TooManyConnections
	deriving (Show, Eq)

proxyClientNetProto :: P2PConnectionPair -> P2P.Proto a -> IO (Either P2P.ProtoFailure a)
proxyClientNetProto conn = runNetProto
	(clientRunState conn) (clientP2PConnection conn)

type AcquireP2PConnection
	= ConnectionParams
	-> IO (Either ConnectionProblem P2PConnectionPair)

withP2PConnections :: AnnexWorkerPool -> (AcquireP2PConnection -> Annex a) -> Annex a
withP2PConnections workerpool a = do
	myuuid <- getUUID
	reqv <- liftIO newEmptyTMVarIO
	relv <- liftIO newEmptyTMVarIO
	endv <- liftIO newEmptyTMVarIO
	proxypool <- liftIO $ newTMVarIO mempty
	asyncservicer <- liftIO $ async $
		servicer myuuid proxypool reqv relv endv
	let endit = do
		liftIO $ atomically $ putTMVar endv ()
		liftIO $ wait asyncservicer
	a (acquireconn reqv) `finally` endit
  where
	acquireconn reqv connparams = do
		respvar <- newEmptyTMVarIO
		atomically $ putTMVar reqv (connparams, respvar)
		atomically $ takeTMVar respvar

	servicer myuuid proxypool reqv relv endv = do
		reqrel <- liftIO $
			atomically $ 
				(Right <$> takeTMVar reqv)
					`orElse` 
				(Left . Right <$> takeTMVar relv)
					`orElse` 
				(Left . Left <$> takeTMVar endv)
		case reqrel of
			Right (connparams, respvar) -> do
				servicereq myuuid proxypool relv connparams
					>>= atomically . putTMVar respvar
				servicer myuuid proxypool reqv relv endv
			Left (Right releaseconn) -> do
				releaseconn
				servicer myuuid proxypool reqv relv endv
			Left (Left ()) -> return ()
	
	servicereq myuuid proxypool relv connparams
		| connectionServerUUID connparams == myuuid =
			localConnection relv connparams workerpool
		| otherwise =
			atomically (getProxyConnectionFromPool proxypool connparams) >>= \case
				Just conn -> proxyConnection relv connparams workerpool conn
				Nothing -> checkcanproxy myuuid proxypool relv connparams

	checkcanproxy myuuid proxypool relv connparams = 
		inAnnexWorker' workerpool
			(checkCanProxy' (connectionServerUUID connparams) myuuid)
		>>= \case
			Right (Left reason) -> return $ Left $
				ConnectionFailed $ 
					fromMaybe "unknown uuid" reason
			Right (Right (Right proxyremote)) ->
				openProxyConnectionToRemote workerpool
					(connectionProtocolVersion connparams)
					(connectionBypass connparams)
					proxyremote
					>>= \case
						Right conn -> proxyConnection relv connparams workerpool conn
						Left ex -> return $ Left $
							ConnectionFailed $ show ex
			Right (Right (Left clusteruuid)) ->
				undefined -- XXX todo
				{-
				openProxyConnectionToCluster clusteruuid
					>>= proxyConnection clusteruuid relv connparams workerpool
				-}
			Left ex -> return $ Left $
				ConnectionFailed $ show ex

data P2PConnectionPair = P2PConnectionPair
	{ clientRunState :: RunState
	, clientP2PConnection :: P2PConnection
	, serverP2PConnection :: Maybe P2PConnection
	, releaseP2PConnection :: IO ()
	-- ^ Releases a P2P connection, which can be reused for other
	-- requests.
	, closeP2PConnection :: IO ()
	-- ^ Closes a P2P connection, which is in a state where it is no
	-- longer usable.
	}

localConnection
	:: TMVar (IO ())
	-> ConnectionParams
	-> AnnexWorkerPool
	-> IO (Either ConnectionProblem P2PConnectionPair)
localConnection relv connparams workerpool = 
	localP2PConnectionPair connparams relv $ \serverrunst serverconn ->
		inAnnexWorker' workerpool $
			void $ runFullProto serverrunst serverconn $
				P2P.serveOneCommandAuthed
					(connectionServerMode connparams)
					(connectionServerUUID connparams)

localP2PConnectionPair
	:: ConnectionParams
	-> TMVar (IO ())
	-> (RunState -> P2PConnection -> IO (Either SomeException ()))
	-> IO (Either ConnectionProblem P2PConnectionPair)
localP2PConnectionPair connparams relv startworker = do
	(clientconn, serverconn) <- mkP2PConnectionPair connparams
		("http client", "http server")
	clientrunst <- mkClientRunState connparams
	serverrunst <- mkServerRunState connparams
	asyncworker <- async $
		startworker serverrunst serverconn
	let releaseconn = atomically $ void $ tryPutTMVar relv $
		liftIO $ wait asyncworker
			>>= either throwM return
	return $ Right $ P2PConnectionPair
		{ clientRunState = clientrunst
		, clientP2PConnection = clientconn
		, serverP2PConnection = Just serverconn
		, releaseP2PConnection = releaseconn
		, closeP2PConnection = releaseconn
		}

mkP2PConnectionPair
	:: ConnectionParams
	-> (String, String)
	-> IO (P2PConnection, P2PConnection)
mkP2PConnectionPair connparams (n1, n2) = do
	hdl1 <- newEmptyTMVarIO
	hdl2 <- newEmptyTMVarIO
	wait1 <- newEmptyTMVarIO
	wait2 <- newEmptyTMVarIO
	let h1 = P2PHandleTMVar hdl1 $ 
		if connectionWaitVar connparams
			then Just wait1
			else Nothing
	let h2 = P2PHandleTMVar hdl2 $
		if connectionWaitVar connparams
			then Just wait2
			else Nothing
	let clientconn = P2PConnection Nothing
		(const True) h2 h1
		(ConnIdent (Just n1))
	let serverconn = P2PConnection Nothing
		(const True) h1 h2
		(ConnIdent (Just n2))
	return (clientconn, serverconn)

mkServerRunState :: ConnectionParams -> IO RunState
mkServerRunState connparams = do
	prototvar <- newTVarIO $ connectionProtocolVersion connparams
	mkRunState $ const $ Serving 
		(connectionClientUUID connparams)
		Nothing
		prototvar
	
mkClientRunState :: ConnectionParams -> IO RunState
mkClientRunState connparams = do
	prototvar <- newTVarIO $ connectionProtocolVersion connparams
	mkRunState $ const $ Client prototvar

proxyConnection
	:: TMVar (IO ())
	-> ConnectionParams
	-> AnnexWorkerPool
	-> ProxyConnection
	-> IO (Either ConnectionProblem P2PConnectionPair)
proxyConnection relv connparams workerpool proxyconn = do
	(clientconn, proxyfromclientconn) <- mkP2PConnectionPair connparams
		("http client", "proxy")
	clientrunst <- mkClientRunState connparams
	proxyfromclientrunst <- mkClientRunState connparams
	asyncworker <- async $
		inAnnexWorker' workerpool $ do
			proxystate <- liftIO Proxy.mkProxyState
			concurrencyconfig <- Proxy.noConcurrencyConfig
			-- TODO run remote protocol to get its version and
			-- take minimum of that and connectionProtocolVersion
			let protocolversion = connectionProtocolVersion connparams
			let proxyparams = Proxy.ProxyParams
				{ Proxy.proxyMethods = mkProxyMethods
				, Proxy.proxyState = proxystate
				, Proxy.proxyServerMode = connectionServerMode connparams
				, Proxy.proxyClientSide = Proxy.ClientSide proxyfromclientrunst proxyfromclientconn
				, Proxy.proxyUUID = proxyConnectionRemoteUUID proxyconn
				, Proxy.proxySelector = Proxy.singleProxySelector $
					proxyConnectionRemoteSide proxyconn
				, Proxy.proxyConcurrencyConfig = concurrencyconfig
				, Proxy.proxyProtocolVersion = protocolversion
				}
			let proxy mrequestmessage = case mrequestmessage of
				Just requestmessage -> do
					Proxy.proxyRequest proxydone proxyparams
						requestcomplete requestmessage protoerrhandler
				Nothing -> return ()
			protoerrhandler proxy $
				liftIO $ runNetProto proxyfromclientrunst proxyfromclientconn $
					P2P.net P2P.receiveMessage
	
	let releaseconn returntopool = 
		atomically $ void $ tryPutTMVar relv $
			liftIO $ wait asyncworker
				>>= either throwM return
	
	return $ Right $ P2PConnectionPair
		{ clientRunState = clientrunst
		, clientP2PConnection = clientconn
		, serverP2PConnection = Nothing
		, releaseP2PConnection = releaseconn True
		, closeP2PConnection = releaseconn False
		}
  where
	protoerrhandler cont a = a >>= \case
		-- TODO protocol error, or client hung up, release the p2p
		-- connection
		Left err -> do
			liftIO $ hPutStrLn stderr ("protoerrhandler: " ++ show err)
			return ()
		Right v -> do
			liftIO $ print "protoerrhandler returned"
			cont v
	proxydone = return ()
	requestcomplete () = return ()

data Locker = Locker
	{ lockerThread :: Async ()
	, lockerVar :: TMVar Bool
	-- ^ Left empty until the thread has taken the lock
	-- (or failed to do so), then True while the lock is held,
	-- and setting to False causes the lock to be released.
	, lockerTimeoutDisable :: TMVar ()
	-- ^ Until this is filled, the lock will be subject to timeout.
	-- Once filled the lock will remain held until explicitly dropped.
	}

mkLocker :: (IO (Maybe a)) -> (a -> IO ()) -> IO (Maybe (Locker, LockID))
mkLocker lock unlock = do
	lv <- newEmptyTMVarIO
	timeoutdisablev <- newEmptyTMVarIO
	let setlocked = putTMVar lv
	locktid <- async $ lock >>= \case
		Nothing ->
			atomically $ setlocked False
		Just st -> do
			atomically $ setlocked True
			atomically $ do
				v <- takeTMVar lv
				if v
					then retry
					else setlocked False
			unlock st
	locksuccess <- atomically $ readTMVar lv
	if locksuccess
		then do
			timeouttid <- async $ do
				threadDelaySeconds $ Seconds $ fromIntegral $
					durationSeconds p2pDefaultLockContentRetentionDuration
				atomically (tryReadTMVar timeoutdisablev) >>= \case
					Nothing -> void $ atomically $
						writeTMVar lv False
					Just () -> noop
			tid <- async $ do
				wait locktid
				cancel timeouttid
			lckid <- B64UUID <$> genUUID
			return (Just (Locker tid lv timeoutdisablev, lckid))
		else do
			wait locktid
			return Nothing

storeLock :: LockID -> Locker -> P2PHttpServerState -> IO ()
storeLock lckid locker st = atomically $ do
	m <- takeTMVar (openLocks st)
	let !m' = M.insert lckid locker m
	putTMVar (openLocks st) m'

keepingLocked :: LockID -> P2PHttpServerState -> IO ()
keepingLocked lckid st = do
	m <- atomically $ readTMVar (openLocks st)
	case M.lookup lckid m of
		Nothing -> return ()
		Just locker ->
			atomically $ void $ 
				tryPutTMVar (lockerTimeoutDisable locker) ()

dropLock :: LockID -> P2PHttpServerState -> IO ()
dropLock lckid st = do
	v <- atomically $ do
		m <- takeTMVar (openLocks st)
		let (mlocker, !m') =
			M.updateLookupWithKey (\_ _ -> Nothing) lckid m
		putTMVar (openLocks st) m'
		case mlocker of
			Nothing -> return Nothing
			-- Signal to the locker's thread that it can
			-- release the lock.
			Just locker -> do
				_ <- swapTMVar (lockerVar locker) False
				return (Just locker)
	case v of
		Nothing -> return ()
		Just locker -> wait (lockerThread locker)

getAnnexWorkerPool :: (AnnexWorkerPool -> Annex a) -> Annex a
getAnnexWorkerPool a = startConcurrency transferStages $
	Annex.getState Annex.workers >>= \case
		Nothing -> giveup "Use -Jn or set annex.jobs to configure the number of worker threads."
		Just wp -> a wp

inAnnexWorker :: P2PHttpServerState -> Annex a -> IO (Either SomeException a)
inAnnexWorker st = inAnnexWorker' (annexWorkerPool st)

inAnnexWorker' :: AnnexWorkerPool -> Annex a -> IO (Either SomeException a)
inAnnexWorker' poolv annexaction = do
	(workerstrd, workerstage) <- atomically $ waitStartWorkerSlot poolv
	resv <- newEmptyTMVarIO
	aid <- async $ do
		(res, strd) <- Annex.run workerstrd annexaction
		atomically $ putTMVar resv res
		return strd
	atomically $ do
		pool <- takeTMVar poolv
		let !pool' = addWorkerPool (ActiveWorker aid workerstage) pool
		putTMVar poolv pool'
	(res, workerstrd') <- waitCatch aid >>= \case
		Right strd -> do
			r <- atomically $ takeTMVar resv
			return (Right r, strd)
		Left err -> return (Left err, workerstrd)
	atomically $ do
		pool <- takeTMVar poolv
		let !pool' = deactivateWorker pool aid workerstrd'
		putTMVar poolv pool'
	return res

data ProxyConnection = ProxyConnection
	{ proxyConnectionRemoteUUID :: UUID
	, proxyConnectionRemoteSide :: Proxy.RemoteSide
	}

openProxyConnectionToRemote
	:: AnnexWorkerPool
	-> P2P.ProtocolVersion
	-> [UUID]
	-> Remote
	-> IO (Either SomeException ProxyConnection)
openProxyConnectionToRemote workerpool protoversion bypass remote =
	inAnnexWorker' workerpool (proxyRemoteSide protoversion bypass' remote) >>= \case
		Left ex -> return (Left ex)
		Right remoteside -> return $ Right $
			ProxyConnection (Remote.uuid remote) remoteside
  where
	bypass' = P2P.Bypass (S.fromList bypass)

openProxyConnectionToCluster :: ClusterUUID -> IO ProxyConnection
openProxyConnectionToCluster cu = error "XXX" -- TODO

type ProxyConnectionPool = 
	M.Map (UUID, UUID, P2P.ProtocolVersion) [ProxyConnection]

getProxyConnectionFromPool
	:: TMVar ProxyConnectionPool
	-> ConnectionParams
	-> STM (Maybe ProxyConnection)
getProxyConnectionFromPool proxypool connparams = do
	m <- takeTMVar proxypool
	case M.lookup k m of
		Nothing -> do
			putTMVar proxypool m
			return Nothing
		Just [] -> do
			putTMVar proxypool $ M.insert k [] m
			return Nothing
		Just (c:cs) -> do
			putTMVar proxypool $ M.insert k cs m
			return (Just c)
  where
	k = 
		( connectionServerUUID connparams
		, connectionClientUUID connparams
		, connectionProtocolVersion connparams
		)
