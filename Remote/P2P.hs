{- git remotes using the git-annex P2P protocol
 -
 - Copyright 2016-2018 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Remote.P2P (
	remote,
	chainGen
) where

import Annex.Common
import qualified Annex
import qualified P2P.Protocol as P2P
import P2P.Address
import P2P.Annex
import P2P.IO
import P2P.Auth
import Types.Remote
import qualified Git
import Annex.UUID
import Config
import Config.Cost
import Remote.Helper.Git
import Remote.Helper.ExportImport
import Remote.Helper.P2P
import Utility.AuthToken
import Annex.SpecialRemote.Config

import Control.Concurrent.STM

remote :: RemoteType
remote = RemoteType
	{ typename = "p2p"
	-- Remote.Git takes care of enumerating P2P remotes,
	-- and will call chainGen on them.
	, enumerate = const (return [])
	, generate = \_ _ _ _ _ -> return Nothing
	, configParser = mkRemoteConfigParser []
	, setup = error "P2P remotes are set up using git-annex p2p"
	, exportSupported = exportUnsupported
	, importSupported = importUnsupported
	}

chainGen :: P2PAddress -> Git.Repo -> UUID -> RemoteConfig -> RemoteGitConfig -> RemoteStateHandle -> Annex (Maybe Remote)
chainGen addr r u rc gc rs = do
	c <- parsedRemoteConfig remote rc
	connpool <- mkConnectionPool
	cst <- remoteCost gc veryExpensiveRemoteCost
	let protorunner = runProto u addr connpool
	let withconn = withConnection u addr connpool
	let this = Remote 
		{ uuid = u
		, cost = cst
		, name = Git.repoDescribe r
		, storeKey = store (const protorunner)
		, retrieveKeyFile = retrieve (const protorunner)
		, retrieveKeyFileCheap = Nothing
		, retrievalSecurityPolicy = RetrievalAllKeysSecure
		, removeKey = remove protorunner
		, lockContent = Just $ lock withconn runProtoConn u 
		, checkPresent = checkpresent protorunner
		, checkPresentCheap = False
		, exportActions = exportUnsupported
		, importActions = importUnsupported
		, whereisKey = Nothing
		, remoteFsck = Nothing
		, repairRepo = Nothing
		, config = c
		, localpath = Nothing
		, getRepo = return r
		, gitconfig = gc
		, readonly = False
		, appendonly = False
		, availability = GloballyAvailable
		, remotetype = remote
		, mkUnavailable = return Nothing
		, getInfo = gitRepoInfo this
		, claimUrl = Nothing
		, checkUrl = Nothing
		, remoteStateHandle = rs
	}
	return (Just this)

-- | A connection to the peer, which can be closed.
type Connection = ClosableConnection (RunState, P2PConnection)

type ConnectionPool = TVar [Connection]

mkConnectionPool :: Annex ConnectionPool
mkConnectionPool = liftIO $ newTVarIO []

-- Runs the Proto action.
runProto :: UUID -> P2PAddress -> ConnectionPool -> P2P.Proto a -> Annex (Maybe a)
runProto u addr connpool a = withConnection u addr connpool (runProtoConn a)

runProtoConn :: P2P.Proto a -> Connection -> Annex (Connection, Maybe a)
runProtoConn _ ClosedConnection = return (ClosedConnection, Nothing)
runProtoConn a c@(OpenConnection (runst, conn)) = do
	v <- runFullProto runst conn a
	-- When runFullProto fails, the connection is no longer usable,
	-- so close it.
	case v of
		Left e -> do
			warning $ "Lost connection to peer (" ++ describeProtoFailure e ++ ")"
			liftIO $ closeConnection conn
			return (ClosedConnection, Nothing)
		Right r -> return (c, Just r)

-- Uses an open connection if one is available in the ConnectionPool;
-- otherwise opens a new connection.
--
-- Once the action is done, the connection is added back to the
-- ConnectionPool, unless it's no longer open.
withConnection :: UUID -> P2PAddress -> ConnectionPool -> (Connection -> Annex (Connection, a)) -> Annex a
withConnection u addr connpool a = bracketOnError get cache go
  where
	get = do
		mc <- liftIO $ atomically $ do
			l <- readTVar connpool
			case l of
				[] -> do
					writeTVar connpool []
					return Nothing
				(c:cs) -> do
					writeTVar connpool cs
					return (Just c)
		maybe (openConnection u addr) return mc
	
	cache ClosedConnection = return ()
	cache conn = liftIO $ atomically $ modifyTVar' connpool (conn:)

	go conn = do
		(conn', r) <- a conn
		cache conn'
		return r

openConnection :: UUID -> P2PAddress -> Annex Connection
openConnection u addr = do
	g <- Annex.gitRepo
	v <- liftIO $ tryNonAsync $ connectPeer g addr
	case v of
		Right conn -> do
			myuuid <- getUUID
			authtoken <- fromMaybe nullAuthToken
				<$> loadP2PRemoteAuthToken addr
			let proto = P2P.auth myuuid authtoken $
				-- Before 6.20180312, the protocol server
				-- had a bug that made negotiating the
				-- protocol version terminate the
				-- connection. So, this must stay disabled
				-- until the old version is not in use
				-- anywhere.
				--P2P.negotiateProtocolVersion P2P.maxProtocolVersion
				return ()
			runst <- liftIO $ mkRunState Client
			res <- liftIO $ runNetProto runst conn proto
			case res of
				Right (Just theiruuid)
					| u == theiruuid -> return (OpenConnection (runst, conn))
					| otherwise -> do
						liftIO $ closeConnection conn
						warning "Remote peer uuid seems to have changed."
						return ClosedConnection
				Right Nothing -> do
					warning "Unable to authenticate with peer."
					liftIO $ closeConnection conn
					return ClosedConnection
				Left e -> do
					warning $ "Problem communicating with peer. (" ++ describeProtoFailure e ++ ")"
					liftIO $ closeConnection conn
					return ClosedConnection
		Left e -> do
			warning $ "Unable to connect to peer. (" ++ show e ++ ")"
			return ClosedConnection
