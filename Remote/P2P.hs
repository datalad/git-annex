{- git remotes using the git-annex P2P protocol
 -
 - Copyright 2016 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
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
import Types.GitConfig
import qualified Git
import Annex.UUID
import Config
import Config.Cost
import Remote.Helper.Git
import Utility.Metered
import Utility.AuthToken
import Types.NumCopies

import Control.Concurrent
import Control.Concurrent.STM

remote :: RemoteType
remote = RemoteType {
	typename = "p2p",
	-- Remote.Git takes care of enumerating P2P remotes,
	-- and will call chainGen on them.
	enumerate = const (return []),
	generate = \_ _ _ _ -> return Nothing,
	setup = error "P2P remotes are set up using git-annex p2p"
}

chainGen :: P2PAddress -> Git.Repo -> UUID -> RemoteConfig -> RemoteGitConfig -> Annex (Maybe Remote)
chainGen addr r u c gc = do
	connpool <- mkConnectionPool
	cst <- remoteCost gc expensiveRemoteCost
	let this = Remote 
		{ uuid = u
		, cost = cst
		, name = Git.repoDescribe r
		, storeKey = store addr connpool
		, retrieveKeyFile = retrieve addr connpool
		, retrieveKeyFileCheap = \_ _ _ -> return False
		, removeKey = remove addr connpool
		, lockContent = Just (lock u addr connpool)
		, checkPresent = checkpresent addr connpool
		, checkPresentCheap = False
		, whereisKey = Nothing
		, remoteFsck = Nothing
		, repairRepo = Nothing
		, config = c
		, localpath = Nothing
		, repo = r
		, gitconfig = gc { remoteGitConfig = Just $ extractGitConfig r }
		, readonly = False
		, availability = GloballyAvailable
		, remotetype = remote
		, mkUnavailable = return Nothing
		, getInfo = gitRepoInfo this
		, claimUrl = Nothing
		, checkUrl = Nothing
	}
	return (Just this)

-- TODO update progress
store :: P2PAddress -> ConnectionPool -> Key -> AssociatedFile -> MeterUpdate -> Annex Bool
store addr connpool k af p = fromMaybe False
	<$> runProto addr connpool (P2P.put k af)

retrieve :: P2PAddress -> ConnectionPool -> Key -> AssociatedFile -> FilePath -> MeterUpdate -> Annex (Bool, Verification)
retrieve addr connpool k af dest _p = unVerified $ fromMaybe False 
	<$> runProto addr connpool (P2P.get dest k af)

remove :: P2PAddress -> ConnectionPool -> Key -> Annex Bool
remove addr connpool k = fromMaybe False
	<$> runProto addr connpool (P2P.remove k)

checkpresent :: P2PAddress -> ConnectionPool -> Key -> Annex Bool
checkpresent addr connpool k = maybe unavail return
	=<< runProto addr connpool (P2P.checkPresent k)
  where
	unavail = giveup "can't connect to peer"

lock :: UUID -> P2PAddress -> ConnectionPool -> Key -> (VerifiedCopy -> Annex r) -> Annex r
lock theiruuid addr connpool k callback =
	withConnection addr connpool $ \conn -> do
		connv <- liftIO $ newMVar conn
		let runproto d p = do
			c <- liftIO $ takeMVar connv
			(c', mr) <- runProto' p c
			liftIO $ putMVar connv c'
			return (fromMaybe d mr)
		r <- P2P.lockContentWhile runproto k go
		conn' <- liftIO $ takeMVar connv
		return (conn', r)
  where
	go False = giveup "can't lock content"
	go True = withVerifiedCopy LockedCopy theiruuid (return True) callback

-- | A connection to the peer.
data Connection 
	= OpenConnection P2PConnection
	| ClosedConnection

type ConnectionPool = TVar [Connection]

mkConnectionPool :: Annex ConnectionPool
mkConnectionPool = liftIO $ newTVarIO []

-- Runs the Proto action.
runProto :: P2PAddress -> ConnectionPool -> P2P.Proto a -> Annex (Maybe a)
runProto addr connpool a = withConnection addr connpool (runProto' a)

runProto' :: P2P.Proto a -> Connection -> Annex (Connection, Maybe a)
runProto' _ ClosedConnection = return (ClosedConnection, Nothing)
runProto' a (OpenConnection conn) = do
	r <- runFullProto Client conn a
	-- When runFullProto fails, the connection is no longer usable,
	-- so close it.
	if isJust r
		then return (OpenConnection conn, r)
		else do
			liftIO $ closeConnection conn
			return (ClosedConnection, r)

-- Uses an open connection if one is available in the ConnectionPool;
-- otherwise opens a new connection.
--
-- Once the action is done, the connection is added back to the
-- ConnectionPool, unless it's no longer open.
withConnection :: P2PAddress -> ConnectionPool -> (Connection -> Annex (Connection, a)) -> Annex a
withConnection addr connpool a = bracketOnError get cache go
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
		maybe (openConnection addr) return mc
	
	cache ClosedConnection = return ()
	cache conn = liftIO $ atomically $ modifyTVar' connpool (conn:)

	go conn = do
		(conn', r) <- a conn
		cache conn'
		return r

openConnection :: P2PAddress -> Annex Connection
openConnection addr = do
	g <- Annex.gitRepo
	v <- liftIO $ tryNonAsync $ connectPeer g addr
	case v of
		Right conn -> do
			myuuid <- getUUID
			authtoken <- fromMaybe nullAuthToken
				<$> loadP2PRemoteAuthToken addr
			res <- liftIO $ runNetProto conn $
				P2P.auth myuuid authtoken
			case res of
				Just (Just _theiruuid) ->
					return (OpenConnection conn)
				_ -> do
					liftIO $ closeConnection conn
					return ClosedConnection
		Left _e -> return ClosedConnection
