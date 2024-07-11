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
import Types.WorkerPool
import Annex.WorkerPool
import CmdLine.Action (startConcurrency)

import Servant
import qualified Data.Map as M
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
	-> (P2PConnectionPair -> Handler (Either ProtoFailure a))
	-> Handler a
withP2PConnection apiver st cu su bypass sec auth actionclass connaction = do
	conn <- getP2PConnection apiver st cu su bypass sec auth actionclass
	connaction' conn
		`finally` liftIO (releaseP2PConnection conn)
  where
	connaction' conn = connaction conn >>= \case
		Right r -> return r
		Left err -> throwError $
			err500 { errBody = encodeBL (describeProtoFailure err) }

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
	-> Handler P2PConnectionPair
getP2PConnection apiver st cu su bypass sec auth actionclass =
	case (getServerMode st sec auth, actionclass) of
		(Just P2P.ServeReadWrite, _) -> go P2P.ServeReadWrite
		(Just P2P.ServeAppendOnly, RemoveAction) -> throwError err403
		(Just P2P.ServeAppendOnly, _) -> go P2P.ServeAppendOnly
		(Just P2P.ServeReadOnly, ReadAction) -> go P2P.ServeReadOnly
		(Just P2P.ServeReadOnly, _) -> throwError err403
		(Nothing, _) -> throwError basicAuthRequired
  where
	go servermode = liftIO (acquireP2PConnection st cp) >>= \case
		Left (ConnectionFailed err) -> 
			throwError err502 { errBody = encodeBL err }
		Left TooManyConnections ->
			throwError err503
		Right v -> return v
	  where
		cp = ConnectionParams
			{ connectionProtocolVersion = protocolVersion apiver
			, connectionServerUUID = fromB64UUID su
			, connectionClientUUID = fromB64UUID cu
			, connectionBypass = map fromB64UUID bypass
			, connectionServerMode = servermode
			}

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
	}
	deriving (Show, Eq, Ord)

data ConnectionProblem
	= ConnectionFailed String
	| TooManyConnections
	deriving (Show, Eq)

data P2PConnectionPair = P2PConnectionPair
	{ clientRunState :: RunState
	, clientP2PConnection :: P2PConnection
	, serverP2PConnection :: P2PConnection
	, releaseP2PConnection :: IO ()
	}

proxyClientNetProto :: P2PConnectionPair -> P2P.Proto a -> IO (Either P2P.ProtoFailure a)
proxyClientNetProto conn = runNetProto
	(clientRunState conn) (clientP2PConnection conn)

type AcquireP2PConnection
	= ConnectionParams
	-> IO (Either ConnectionProblem P2PConnectionPair)

{- Acquire P2P connections to the local repository. -}
withLocalP2PConnections :: AnnexWorkerPool -> (AcquireP2PConnection -> Annex a) -> Annex a
withLocalP2PConnections workerpool a = do
	myuuid <- getUUID
	reqv <- liftIO newEmptyTMVarIO
	relv <- liftIO newEmptyTMVarIO
	asyncservicer <- liftIO $ async $ servicer myuuid reqv relv
	a (acquireconn reqv) `finally` join (liftIO (wait asyncservicer))
  where
	acquireconn reqv connparams = do
		respvar <- newEmptyTMVarIO
		atomically $ putTMVar reqv (connparams, respvar)
		atomically $ takeTMVar respvar

	servicer myuuid reqv relv = do
		reqrel <- liftIO $
			atomically $ 
				(Right <$> takeTMVar reqv)
					`orElse` 
				(Left <$> takeTMVar relv)
		case reqrel of
			Right (connparams, respvar) ->
				servicereq myuuid relv connparams
					>>= atomically . putTMVar respvar
			Left releaseconn -> releaseconn
		servicer myuuid reqv relv
	
	servicereq myuuid relv connparams
		| connectionServerUUID connparams /= myuuid =
			return $ Left $ ConnectionFailed "unknown uuid"
		| otherwise = mkP2PConnectionPair connparams relv $ 
			\serverrunst serverconn -> inAnnexWorker' workerpool $
				void $ runFullProto serverrunst serverconn $
					P2P.serveOneCommandAuthed
						(connectionServerMode connparams)
						(connectionServerUUID connparams)

mkP2PConnectionPair
	:: ConnectionParams
	-> TMVar (IO ())
	-> (RunState -> P2PConnection -> IO (Either SomeException ()))
	-> IO (Either ConnectionProblem P2PConnectionPair)
mkP2PConnectionPair connparams relv startworker = do
	hdl1 <- newEmptyTMVarIO
	hdl2 <- newEmptyTMVarIO
	wait1 <- newEmptyTMVarIO
	wait2 <- newEmptyTMVarIO
	let h1 = P2PHandleTMVar hdl1 wait1
	let h2 = P2PHandleTMVar hdl2 wait2
	let serverconn = P2PConnection Nothing
		(const True) h1 h2
		(ConnIdent (Just "http server"))
	let clientconn = P2PConnection Nothing
		(const True) h2 h1
		(ConnIdent (Just "http client"))
	clientrunst <- mkclientrunst
	serverrunst <- mkserverrunst
	asyncworker <- async $
		startworker serverrunst serverconn
	let releaseconn = atomically $ putTMVar relv $
		liftIO $ wait asyncworker
			>>= either throwM return
	return $ Right $ P2PConnectionPair
		{ clientRunState = clientrunst
		, clientP2PConnection = clientconn
		, serverP2PConnection = serverconn
		, releaseP2PConnection = releaseconn
		}
  where
	mkserverrunst = do
		prototvar <- newTVarIO $ connectionProtocolVersion connparams
		mkRunState $ const $ Serving 
			(connectionClientUUID connparams)
			Nothing
			prototvar
	
	mkclientrunst = do
		prototvar <- newTVarIO $ connectionProtocolVersion connparams
		mkRunState $ const $ Client prototvar

data Locker = Locker
	{ lockerThread :: Async ()
	, lockerVar :: TMVar Bool
	-- ^ Left empty until the thread has taken the lock
	-- (or failed to do so), then True while the lock is held,
	-- and setting to False causes the lock to be released.
	}

mkLocker :: IO () -> IO () -> IO (Maybe (Locker, LockID))
mkLocker lock unlock = do
	lv <- newEmptyTMVarIO
	let setlocked = putTMVar lv
	tid <- async $
		tryNonAsync lock >>= \case
			Left _ -> do
				atomically $ setlocked False
				unlock
			Right () -> do
				atomically $ setlocked True
				atomically $ do
					v <- takeTMVar lv
					if v
						then retry
						else setlocked False
				unlock
	locksuccess <- atomically $ readTMVar lv
	if locksuccess
		then do
			lckid <- B64UUID <$> genUUID
			return (Just (Locker tid lv, lckid))
		else do
			wait tid
			return Nothing

storeLock :: LockID -> Locker -> P2PHttpServerState -> IO ()
storeLock lckid locker st = atomically $ do
	m <- takeTMVar (openLocks st)
	let !m' = M.insert lckid locker m
	putTMVar (openLocks st) m'

dropLock :: LockID -> P2PHttpServerState -> IO ()
dropLock lckid st = do
	v <- atomically $ do
		m <- takeTMVar (openLocks st)
		let (mlocker, !m') =
			M.updateLookupWithKey (\_ _ -> Nothing) lckid m
		putTMVar (openLocks st) m'
		case mlocker of
			Nothing -> return Nothing
			-- Signal to the locker's thread that it can release the lock.
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
