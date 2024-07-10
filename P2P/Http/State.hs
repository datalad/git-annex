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

module P2P.Http.State where

import Annex.Common
import P2P.Http.Types
import qualified P2P.Protocol as P2P
import P2P.IO
import P2P.Annex
import Annex.UUID
import Annex.Concurrent

import Servant
import qualified Data.Map as M
import Control.Concurrent.Async
import Control.Concurrent.STM

data P2PHttpServerState = P2PHttpServerState
	{ acquireP2PConnection :: AcquireP2PConnection
	, getServerMode :: GetServerMode
	, openLocks :: TMVar (M.Map LockID Locker)
	}

mkP2PHttpServerState :: AcquireP2PConnection -> GetServerMode -> IO P2PHttpServerState
mkP2PHttpServerState acquireconn getservermode = P2PHttpServerState
	<$> pure acquireconn
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
	-> (RunState -> P2PConnection -> Handler (Either ProtoFailure a))
	-> Handler a
withP2PConnection apiver st cu su bypass sec auth actionclass connaction =
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
		Right (runst, conn, releaseconn) ->
			connaction' runst conn
				`finally` liftIO releaseconn
	  where
		cp = ConnectionParams
			{ connectionProtocolVersion = protocolVersion apiver
			, connectionServerUUID = fromB64UUID su
			, connectionClientUUID = fromB64UUID cu
			, connectionBypass = map fromB64UUID bypass
			, connectionServerMode = servermode
			}
	
	connaction' runst conn = connaction runst conn >>= \case
		Right r -> return r
		Left err -> throwError $
			err500 { errBody = encodeBL (describeProtoFailure err) }

basicAuthRequired :: ServerError
basicAuthRequired = err401 { errHeaders = [(h, v)] }
  where
	h = "WWW-Authenticate"
	v = "Basic realm=\"git-annex\", charset=\"UTF-8\""

-- Nothing when the server is not allowed to serve any requests.
type GetServerMode = IsSecure -> Maybe Auth -> Maybe P2P.ServerMode

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

type AcquireP2PConnection = 
	ConnectionParams -> IO
		( Either ConnectionProblem
			( RunState
			, P2PConnection
			, IO () -- ^ release connection
			)
		)

{- Runs P2P actions in the local repository only. -}
-- TODO need worker pool, this can only service a single request at
-- a time.
-- TODO proxies
-- TODO clusters
withLocalP2PConnections :: (AcquireP2PConnection -> Annex a) -> Annex a
withLocalP2PConnections a = do
	reqv <- liftIO newEmptyTMVarIO
	relv <- liftIO newEmptyTMVarIO
	asyncservicer <- liftIO . async =<< forkState (servicer reqv relv)
	a (acquireconn reqv) `finally` join (liftIO (wait asyncservicer))
  where
	acquireconn reqv connparams = do
		respvar <- newEmptyTMVarIO
		liftIO $ atomically $ putTMVar reqv (connparams, respvar)
		liftIO $ atomically $ takeTMVar respvar

	servicer reqv relv = do
		reqrel <- liftIO $
			atomically $ 
				(Right <$> takeTMVar reqv)
					`orElse` 
				(Left <$> takeTMVar relv)
		case reqrel of
			Right (connparams, respvar) ->
				servicereq relv connparams respvar
			Left releaseconn -> releaseconn
		servicer reqv relv
	
	servicereq relv connparams respvar = do
		myuuid <- getUUID
		resp <- if connectionServerUUID connparams /= myuuid
			then return $ Left $ ConnectionFailed "unknown uuid"
			else do
				hdl1 <- liftIO newEmptyTMVarIO
				hdl2 <- liftIO newEmptyTMVarIO
				waitv1 <- liftIO newEmptyTMVarIO
				waitv2 <- liftIO newEmptyTMVarIO
				let h1 = P2PHandleTMVar hdl1 waitv1
				let h2 = P2PHandleTMVar hdl2 waitv2 
				let serverconn = P2PConnection Nothing
					(const True) h1 h2
					(ConnIdent (Just "http server"))
				let clientconn = P2PConnection Nothing
					(const True) h2 h1
					(ConnIdent (Just "http client"))
				runst <- liftIO $ mkrunst connparams
				let server = P2P.serveOneCommandAuthed
					(connectionServerMode connparams)
					(connectionServerUUID connparams)
				let protorunner = void $
					runFullProto runst serverconn server
				asyncworker <- liftIO . async 
					=<< forkState protorunner
				let releaseconn = atomically $ putTMVar relv $
					join (liftIO (wait asyncworker))
				return $ Right (runst, clientconn, releaseconn)
		liftIO $ atomically $ putTMVar respvar resp

	mkrunst connparams = do
		prototvar <- newTVarIO $ connectionProtocolVersion connparams
		mkRunState $ const $ Serving 
			(connectionClientUUID connparams)
			Nothing
			prototvar

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
