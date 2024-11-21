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
{-# LANGUAGE TupleSections #-}

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
import Annex.BranchState
import Types.Cluster
import CmdLine.Action (startConcurrency)
import Utility.ThreadScheduler
import Utility.HumanTime
import Logs.Proxy
import Annex.Proxy
import Annex.Cluster
import qualified P2P.Proxy as Proxy
import qualified Types.Remote as Remote
import Utility.STM

import Servant
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Control.Concurrent.Async
import Data.Time.Clock.POSIX

data P2PHttpServerState = P2PHttpServerState
	{ servedRepos :: M.Map UUID PerRepoServerState
	}

data PerRepoServerState = PerRepoServerState
	{ acquireP2PConnection :: AcquireP2PConnection
	, annexWorkerPool :: AnnexWorkerPool
	, getServerMode :: GetServerMode
	, openLocks :: TMVar (M.Map LockID Locker)
	}

type AnnexWorkerPool = TMVar (WorkerPool (Annex.AnnexState, Annex.AnnexRead))

type GetServerMode = IsSecure -> Maybe Auth -> ServerMode

data ServerMode
	= ServerMode
		{ serverMode :: P2P.ServerMode
		, unauthenticatedLockingAllowed :: Bool
		, authenticationAllowed :: Bool
		}
	| CannotServeRequests

mkPerRepoServerState :: AcquireP2PConnection -> AnnexWorkerPool -> GetServerMode -> IO PerRepoServerState
mkPerRepoServerState acquireconn annexworkerpool getservermode = PerRepoServerState
	<$> pure acquireconn
	<*> pure annexworkerpool
	<*> pure getservermode
	<*> newTMVarIO mempty

data ActionClass = ReadAction | WriteAction | RemoveAction | LockAction
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
	-> ((P2PConnectionPair, PerRepoServerState) -> Handler (Either ProtoFailure a))
	-> Handler a
withP2PConnection apiver mst cu su bypass sec auth actionclass fconnparams connaction =
	withP2PConnection' apiver mst cu su bypass sec auth actionclass fconnparams connaction'
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
	-> ((P2PConnectionPair, PerRepoServerState) -> Handler a)
	-> Handler a
withP2PConnection' apiver mst cu su bypass sec auth actionclass fconnparams connaction = do
	(conn, st) <- getP2PConnection apiver mst cu su bypass sec auth actionclass fconnparams
	connaction (conn, st)
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
	-> Handler (P2PConnectionPair, PerRepoServerState)
getP2PConnection apiver mst cu su bypass sec auth actionclass fconnparams =
	checkAuthActionClass mst su sec auth actionclass go
  where
	go st servermode = liftIO (acquireP2PConnection st cp) >>= \case
		Left (ConnectionFailed err) -> 
			throwError err502 { errBody = encodeBL err }
		Left TooManyConnections ->
			throwError err503
		Right v -> return (v, st)
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
	-> B64UUID ServerSide
	-> IsSecure
	-> Maybe Auth
	-> ActionClass
	-> (PerRepoServerState -> P2P.ServerMode -> Handler a)
	-> Handler a
checkAuthActionClass mst su sec auth actionclass go =
	case M.lookup (fromB64UUID su) (servedRepos mst) of
		Just st -> select st
		Nothing -> throwError err404
  where
	select st = case (sm, actionclass) of
		(ServerMode { serverMode = P2P.ServeReadWrite }, _) ->
			go st P2P.ServeReadWrite
		(ServerMode { unauthenticatedLockingAllowed = True }, LockAction) ->
			go st P2P.ServeReadOnly
		(ServerMode { serverMode = P2P.ServeAppendOnly }, RemoveAction) -> 
			throwError $ forbiddenWithoutAuth sm
		(ServerMode { serverMode = P2P.ServeAppendOnly }, _) ->
			go st P2P.ServeAppendOnly
		(ServerMode { serverMode = P2P.ServeReadOnly }, ReadAction) ->
			go st P2P.ServeReadOnly
		(ServerMode { serverMode = P2P.ServeReadOnly }, _) -> 
			throwError $ forbiddenWithoutAuth sm
		(CannotServeRequests, _) -> throwError basicAuthRequired
	  where
		sm = getServerMode st sec auth

forbiddenAction :: ServerError
forbiddenAction = err403

basicAuthRequired :: ServerError
basicAuthRequired = err401 { errHeaders = [(h, v)] }
  where
	h = "WWW-Authenticate"
	v = "Basic realm=\"git-annex\", charset=\"UTF-8\""

forbiddenWithoutAuth :: ServerMode -> ServerError
forbiddenWithoutAuth sm
	| authenticationAllowed sm = basicAuthRequired
	| otherwise = forbiddenAction

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

withP2PConnections
	:: AnnexWorkerPool
	-> ProxyConnectionPoolSize
	-> ClusterConcurrency
	-> ([UUID] -> AcquireP2PConnection -> Annex a)
	-> Annex a
withP2PConnections workerpool proxyconnectionpoolsize clusterconcurrency a = do
	enableInteractiveBranchAccess
	myuuid <- getUUID
	myproxies <- M.lookup myuuid <$> getProxies
	reqv <- liftIO newEmptyTMVarIO
	relv <- liftIO newEmptyTMVarIO
	endv <- liftIO newEmptyTMVarIO
	proxypool <- liftIO $ newTMVarIO (0, mempty)
	asyncservicer <- liftIO $ async $
		servicer myuuid myproxies proxypool reqv relv endv
	let endit = do
		liftIO $ atomically $ putTMVar endv ()
		liftIO $ wait asyncservicer
	let servinguuids = myuuid : map proxyRemoteUUID (maybe [] S.toList myproxies)
	a servinguuids (acquireconn reqv) `finally` endit
  where
	acquireconn reqv connparams = do
		respvar <- newEmptyTMVarIO
		atomically $ putTMVar reqv (connparams, respvar)
		atomically $ takeTMVar respvar

	servicer myuuid myproxies proxypool reqv relv endv = do
		reqrel <- liftIO $
			atomically $ 
				(Right <$> takeTMVar reqv)
					`orElse` 
				(Left . Right <$> takeTMVar relv)
					`orElse` 
				(Left . Left <$> takeTMVar endv)
		case reqrel of
			Right (connparams, respvar) -> do
				servicereq myuuid myproxies proxypool relv connparams
					>>= atomically . putTMVar respvar
				servicer myuuid myproxies proxypool reqv relv endv
			Left (Right releaseconn) -> do
				void $ tryNonAsync releaseconn
				servicer myuuid myproxies proxypool reqv relv endv
			Left (Left ()) -> return ()
	
	servicereq myuuid myproxies proxypool relv connparams
		| connectionServerUUID connparams == myuuid =
			localConnection relv connparams workerpool
		| otherwise =
			atomically (getProxyConnectionPool proxypool connparams) >>= \case
				Just conn -> proxyConnection proxyconnectionpoolsize relv connparams workerpool proxypool conn
				Nothing -> checkcanproxy myproxies proxypool relv connparams

	checkcanproxy myproxies proxypool relv connparams = 
		inAnnexWorker' workerpool
			(checkCanProxy' myproxies (connectionServerUUID connparams))
		>>= \case
			Right (Left reason) -> return $ Left $
				ConnectionFailed $ 
					fromMaybe "unknown uuid" reason
			Right (Right (Right proxyremote)) -> proxyconnection $
				openProxyConnectionToRemote workerpool
					(connectionProtocolVersion connparams)
					bypass proxyremote
			Right (Right (Left clusteruuid)) -> proxyconnection $
				openProxyConnectionToCluster workerpool
					(connectionProtocolVersion connparams)
					bypass clusteruuid clusterconcurrency
			Left ex -> return $ Left $
				ConnectionFailed $ show ex
	  where
		bypass = P2P.Bypass $ S.fromList $ connectionBypass connparams
		proxyconnection openconn = openconn >>= \case
			Right conn -> proxyConnection proxyconnectionpoolsize
				relv connparams workerpool proxypool conn
			Left ex -> return $ Left $
				ConnectionFailed $ show ex

data P2PConnectionPair = P2PConnectionPair
	{ clientRunState :: RunState
	, clientP2PConnection :: P2PConnection
	, serverP2PConnection :: P2PConnection
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
		, serverP2PConnection = serverconn
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
	closed1 <- newEmptyTMVarIO
	closed2 <- newEmptyTMVarIO
	let h1 = P2PHandleTMVar hdl1
		(if connectionWaitVar connparams then Just wait1 else Nothing)
		closed1
	let h2 = P2PHandleTMVar hdl2
		(if connectionWaitVar connparams then Just wait2 else Nothing)
		closed2
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
	:: ProxyConnectionPoolSize
	-> TMVar (IO ())
	-> ConnectionParams
	-> AnnexWorkerPool
	-> TMVar ProxyConnectionPool
	-> ProxyConnection
	-> IO (Either ConnectionProblem P2PConnectionPair)
proxyConnection proxyconnectionpoolsize relv connparams workerpool proxypool proxyconn = do
	(clientconn, proxyfromclientconn) <- 
		mkP2PConnectionPair connparams ("http client", "proxy")
	clientrunst <- mkClientRunState connparams
	proxyfromclientrunst <- mkClientRunState connparams
	asyncworker <- async $
		inAnnexWorker' workerpool $ do
			proxystate <- liftIO Proxy.mkProxyState
			let proxyparams = Proxy.ProxyParams
				{ Proxy.proxyMethods = mkProxyMethods
				, Proxy.proxyState = proxystate
				, Proxy.proxyServerMode = connectionServerMode connparams
				, Proxy.proxyClientSide = Proxy.ClientSide proxyfromclientrunst proxyfromclientconn
				, Proxy.proxyUUID = proxyConnectionRemoteUUID proxyconn
				, Proxy.proxySelector = proxyConnectionSelector proxyconn
				, Proxy.proxyConcurrencyConfig = proxyConnectionConcurrency proxyconn
				, Proxy.proxyClientProtocolVersion = connectionProtocolVersion connparams
				}
			let proxy mrequestmessage = case mrequestmessage of
				Just requestmessage -> do
					Proxy.proxyRequest proxydone proxyparams
						requestcomplete requestmessage protoerrhandler
				Nothing -> return ()
			protoerrhandler proxy $
				liftIO $ runNetProto proxyfromclientrunst proxyfromclientconn $
					P2P.net P2P.receiveMessage
	
	let closebothsides = do
		liftIO $ closeConnection proxyfromclientconn
		liftIO $ closeConnection clientconn

	let releaseconn connstillusable = do
		atomically $ void $ tryPutTMVar relv $ do
			unless connstillusable
				closebothsides
			r <- liftIO $ wait asyncworker
			when connstillusable
				closebothsides
			if connstillusable
				then liftIO $ do
					now <- getPOSIXTime
					evicted <- atomically $ putProxyConnectionPool proxypool proxyconnectionpoolsize connparams $
						proxyconn { proxyConnectionLastUsed = now }
					maybe noop closeproxyconnection evicted
				else closeproxyconnection proxyconn
			either throwM return r
				
	return $ Right $ P2PConnectionPair
		{ clientRunState = clientrunst
		, clientP2PConnection = clientconn
		, serverP2PConnection = proxyfromclientconn
		, releaseP2PConnection = releaseconn True
		, closeP2PConnection = releaseconn False
		}
  where
	protoerrhandler cont a = a >>= \case
		Left _ -> proxyConnectionCloser proxyconn
		Right v -> cont v
	
	proxydone = return ()
	
	requestcomplete () = return ()
	
	closeproxyconnection = 
		void . inAnnexWorker' workerpool . proxyConnectionCloser

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

storeLock :: LockID -> Locker -> PerRepoServerState -> IO ()
storeLock lckid locker st = atomically $ do
	m <- takeTMVar (openLocks st)
	let !m' = M.insert lckid locker m
	putTMVar (openLocks st) m'

keepingLocked :: LockID -> PerRepoServerState -> IO ()
keepingLocked lckid st = do
	m <- atomically $ readTMVar (openLocks st)
	case M.lookup lckid m of
		Nothing -> return ()
		Just locker ->
			atomically $ void $ 
				tryPutTMVar (lockerTimeoutDisable locker) ()

dropLock :: LockID -> PerRepoServerState -> IO ()
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

inAnnexWorker :: PerRepoServerState -> Annex a -> IO (Either SomeException a)
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
	, proxyConnectionSelector :: Proxy.ProxySelector
	, proxyConnectionCloser :: Annex ()
	, proxyConnectionConcurrency :: Proxy.ConcurrencyConfig
	, proxyConnectionLastUsed :: POSIXTime
	}

instance Show ProxyConnection where
	show pc = unwords
		[ "ProxyConnection"
		, show (proxyConnectionRemoteUUID pc)
		, show (proxyConnectionLastUsed pc)
		]

openedProxyConnection
	:: UUID
	-> String
	-> Proxy.ProxySelector
	-> Annex ()
	-> Proxy.ConcurrencyConfig
	-> Annex ProxyConnection
openedProxyConnection u desc selector closer concurrency = do
	now <- liftIO getPOSIXTime
	fastDebug "P2P.Http" ("Opened proxy connection to " ++ desc)
	return $ ProxyConnection u selector closer' concurrency now
  where
	closer' = do
		fastDebug "P2P.Http" ("Closing proxy connection to " ++ desc)
		closer
		fastDebug "P2P.Http" ("Closed proxy connection to " ++ desc)

openProxyConnectionToRemote
	:: AnnexWorkerPool
	-> P2P.ProtocolVersion
	-> P2P.Bypass
	-> Remote
	-> IO (Either SomeException ProxyConnection)
openProxyConnectionToRemote workerpool clientmaxversion bypass remote =
	inAnnexWorker' workerpool $ do
		remoteside <- proxyRemoteSide clientmaxversion bypass remote
		concurrencyconfig <- Proxy.noConcurrencyConfig
		openedProxyConnection (Remote.uuid remote)
			("remote " ++ Remote.name remote)
			(Proxy.singleProxySelector remoteside)
			(Proxy.closeRemoteSide remoteside)
			concurrencyconfig

type ClusterConcurrency = Int

openProxyConnectionToCluster
	:: AnnexWorkerPool
	-> P2P.ProtocolVersion
	-> P2P.Bypass
	-> ClusterUUID
	-> ClusterConcurrency
	-> IO (Either SomeException ProxyConnection)
openProxyConnectionToCluster workerpool clientmaxversion bypass clusteruuid concurrency =
	inAnnexWorker' workerpool $ do
		(proxyselector, closenodes) <-
			clusterProxySelector clusteruuid clientmaxversion bypass
		concurrencyconfig <- Proxy.mkConcurrencyConfig concurrency
		openedProxyConnection (fromClusterUUID clusteruuid)
			("cluster " ++ fromUUID (fromClusterUUID clusteruuid))
			proxyselector closenodes concurrencyconfig

type ProxyConnectionPool = (Integer, M.Map ProxyConnectionPoolKey [ProxyConnection])

type ProxyConnectionPoolSize = Integer

-- Returns any older ProxyConnection that was evicted from the pool.
putProxyConnectionPool
	:: TMVar ProxyConnectionPool
	-> ProxyConnectionPoolSize
	-> ConnectionParams
	-> ProxyConnection
	-> STM (Maybe ProxyConnection)
putProxyConnectionPool proxypool maxsz connparams conn = do
	(sz, m) <- takeTMVar proxypool
	let ((sz', m'), evicted) = case M.lookup k m of
		Nothing -> ((succ sz, M.insert k [conn] m), Nothing)
		Just [] -> ((succ sz, M.insert k [conn] m), Nothing)
		Just cs -> if sz >= maxsz
			then ((sz, M.insert k (conn : dropFromEnd 1 cs) m), lastMaybe cs)
			else ((sz, M.insert k (conn : cs) m), Nothing)
	let ((sz'', m''), evicted') = if sz' > maxsz
		then removeOldestProxyConnectionPool (sz', m')
		else ((sz', m'), Nothing)
	putTMVar proxypool (sz'', m'')
	return (evicted <|> evicted')
  where
	k = proxyConnectionPoolKey connparams

removeOldestProxyConnectionPool :: ProxyConnectionPool -> (ProxyConnectionPool, Maybe ProxyConnection)
removeOldestProxyConnectionPool (sz, m) = 
	((pred sz, m'), snd <$> headMaybe l)
  where
	m' = M.fromListWith (++) $ map (\(k', v) -> (k', [v])) (drop 1 l)
	l = sortOn (proxyConnectionLastUsed . snd) $
		concatMap (\(k', pl) -> map (k', ) pl) $
			M.toList m

getProxyConnectionPool
	:: TMVar ProxyConnectionPool
	-> ConnectionParams
	-> STM (Maybe ProxyConnection)
getProxyConnectionPool proxypool connparams = do
	(sz, m) <- takeTMVar proxypool
	case M.lookup k m of
		Just (c:cs) -> do
			putTMVar proxypool (sz-1, M.insert k cs m)
			return (Just c)
		_ -> do
			putTMVar proxypool (sz, m)
			return Nothing
  where
	k = proxyConnectionPoolKey connparams

type ProxyConnectionPoolKey = (UUID, UUID, [UUID], P2P.ProtocolVersion)

proxyConnectionPoolKey :: ConnectionParams -> ProxyConnectionPoolKey
proxyConnectionPoolKey connparams =
	( connectionServerUUID connparams
	, connectionClientUUID connparams
	, connectionBypass connparams
	, connectionProtocolVersion connparams
	)
