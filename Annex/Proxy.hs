{- proxying
 -
 - Copyright 2024 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Annex.Proxy where

import Annex.Common
import qualified Annex
import qualified Remote
import qualified Types.Remote as Remote
import qualified Remote.Git
import P2P.Proxy
import P2P.Protocol
import P2P.IO
import Remote.Helper.Ssh (openP2PShellConnection', closeP2PShellConnection)
import Annex.Content
import Annex.Concurrent
import Annex.Tmp
import Logs.Proxy
import Logs.Cluster
import Logs.UUID
import Logs.Location
import Utility.Tmp.Dir
import Utility.Metered

import Control.Concurrent.STM
import Control.Concurrent.Async
import qualified Data.ByteString.Lazy as L
import qualified System.FilePath.ByteString as P
import qualified Data.Map as M
import qualified Data.Set as S

proxyRemoteSide :: ProtocolVersion -> Bypass -> Remote -> Annex RemoteSide
proxyRemoteSide clientmaxversion bypass r
	| Remote.remotetype r == Remote.Git.remote = 
		proxyGitRemoteSide clientmaxversion bypass r
	| otherwise =
		proxySpecialRemoteSide clientmaxversion r

proxyGitRemoteSide :: ProtocolVersion -> Bypass -> Remote -> Annex RemoteSide
proxyGitRemoteSide clientmaxversion bypass r = mkRemoteSide r $
	openP2PShellConnection' r clientmaxversion bypass >>= \case
		Just conn@(OpenConnection (remoterunst, remoteconn, _)) ->
			return $ Just 
				( remoterunst
				, remoteconn
				, void $ liftIO $ closeP2PShellConnection conn
				)
		_  -> return Nothing

proxySpecialRemoteSide :: ProtocolVersion -> Remote -> Annex RemoteSide
proxySpecialRemoteSide clientmaxversion r = mkRemoteSide r $ do
	let protoversion = min clientmaxversion maxProtocolVersion
	remoterunst <- Serving (Remote.uuid r) Nothing <$>
		liftIO (newTVarIO protoversion)
	ihdl <- liftIO newEmptyTMVarIO
	ohdl <- liftIO newEmptyTMVarIO
	iwaitv <- liftIO newEmptyTMVarIO
	owaitv <- liftIO newEmptyTMVarIO
	iclosedv <- liftIO newEmptyTMVarIO
	oclosedv <- liftIO newEmptyTMVarIO
	endv <- liftIO newEmptyTMVarIO
	worker <- liftIO . async =<< forkState
		(proxySpecialRemote protoversion r ihdl ohdl owaitv endv)
	let remoteconn = P2PConnection
		{ connRepo = Nothing
		, connCheckAuth = const False
		, connIhdl = P2PHandleTMVar ihdl (Just iwaitv) iclosedv
		, connOhdl = P2PHandleTMVar ohdl (Just owaitv) oclosedv
		, connIdent = ConnIdent (Just (Remote.name r))
		}
	let closeremoteconn = do
		liftIO $ atomically $ putTMVar endv ()
		join $ liftIO (wait worker)
	return $ Just
		( remoterunst
		, remoteconn
		, closeremoteconn
		)

-- Proxy for the special remote, speaking the P2P protocol.
proxySpecialRemote
	:: ProtocolVersion
	-> Remote
	-> TMVar (Either L.ByteString Message)
	-> TMVar (Either L.ByteString Message)
	-> TMVar ()
	-> TMVar ()
	-> Annex ()
proxySpecialRemote protoversion r ihdl ohdl owaitv endv = go
  where
	go :: Annex ()
	go = liftIO receivemessage >>= \case
		Just (CHECKPRESENT k) -> do
			tryNonAsync (Remote.checkPresent r k) >>= \case
				Right True -> liftIO $ sendmessage SUCCESS
				Right False -> liftIO $ sendmessage FAILURE
				Left err -> liftIO $ propagateerror err
			go
		Just (LOCKCONTENT _) -> do
			-- Special remotes do not support locking content.
			liftIO $ sendmessage FAILURE
			go
		Just (REMOVE k) -> do
			tryNonAsync (Remote.removeKey r Nothing k) >>= \case
				Right () -> liftIO $ sendmessage SUCCESS
				Left err -> liftIO $ propagateerror err
			go
		Just (PUT (ProtoAssociatedFile af) k) -> do
			proxyput af k
			go
		Just (GET offset (ProtoAssociatedFile af) k) -> do
			proxyget offset af k
			go
		Just (BYPASS _) -> go
		Just (CONNECT _) -> 
			-- Not supported and the protocol ends here.
			liftIO $ sendmessage $ CONNECTDONE (ExitFailure 1)	
		Just NOTIFYCHANGE -> do
			liftIO $ sendmessage $
				ERROR "NOTIFYCHANGE unsupported for a special remote"
			go
		Just _ -> giveup "protocol error"
		Nothing -> return ()

	getnextmessageorend = 
		liftIO $ atomically $ 
			(Right <$> takeTMVar ohdl)
				`orElse`
			(Left <$> readTMVar endv)

	receivemessage = getnextmessageorend >>= \case
		Right (Right m) -> return (Just m)
		Right (Left _b) -> giveup "unexpected ByteString received from P2P MVar"
		Left () -> return Nothing
	
	receivebytestring = atomically (takeTMVar ohdl) >>= \case
		Left b -> return b
		Right _m -> giveup "did not receive ByteString from P2P MVar"

	sendmessage m = atomically $ putTMVar ihdl (Right m)
	
	sendbytestring b = atomically $ putTMVar ihdl (Left b)

	propagateerror err = sendmessage $ ERROR $
		"proxied special remote reports: " ++ show err

	-- Not using gitAnnexTmpObjectLocation because there might be
	-- several concurrent GET and PUTs of the same key being proxied
	-- from this special remote or others, and each needs to happen
	-- independently. Also, this key is not getting added into the
	-- local annex objects.
	withproxytmpfile k a = withOtherTmp $ \othertmpdir ->
		withTmpDirIn (fromRawFilePath othertmpdir) "proxy" $ \tmpdir ->
			a (toRawFilePath tmpdir P.</> keyFile k)
			
	proxyput af k = do
		liftIO $ sendmessage $ PUT_FROM (Offset 0)
		withproxytmpfile k $ \tmpfile -> do
			let store = tryNonAsync (Remote.storeKey r k af (Just (decodeBS tmpfile)) nullMeterUpdate) >>= \case
				Right () -> liftIO $ sendmessage SUCCESS
				Left err -> liftIO $ propagateerror err
			liftIO receivemessage >>= \case
				Just (DATA (Len _)) -> do
					b <- liftIO receivebytestring
					liftIO $ L.writeFile (fromRawFilePath tmpfile) b
					-- Signal that the whole bytestring
					-- has been received.
					liftIO $ atomically $ putTMVar owaitv ()
					if protoversion > ProtocolVersion 1
						then liftIO receivemessage >>= \case
							Just (VALIDITY Valid) ->
								store
							Just (VALIDITY Invalid) ->
								liftIO $ sendmessage FAILURE
							_ -> giveup "protocol error"
						else store
				_ -> giveup "protocol error"
			liftIO $ removeWhenExistsWith removeFile (fromRawFilePath tmpfile)

	proxyget offset af k = withproxytmpfile k $ \tmpfile -> do
		-- Don't verify the content from the remote,
		-- because the client will do its own verification.
		let vc = Remote.NoVerify
		tryNonAsync (Remote.retrieveKeyFile r k af (fromRawFilePath tmpfile) nullMeterUpdate vc) >>= \case
			Right v ->
				ifM (verifyKeyContentPostRetrieval Remote.RetrievalVerifiableKeysSecure vc v k tmpfile)
					( liftIO $ senddata offset tmpfile
					, liftIO $ sendmessage $
						ERROR "verification of content failed"
					)
			Left err -> liftIO $ propagateerror err
	
	senddata (Offset offset) f = do
		size <- fromIntegral <$> getFileSize f
		let n = max 0 (size - offset)
		sendmessage $ DATA (Len n)
		withBinaryFile (fromRawFilePath f) ReadMode $ \h -> do
			hSeek h AbsoluteSeek offset
			sendbs =<< L.hGetContents h
			-- Important to keep the handle open until
			-- the client responds. The bytestring
			-- could still be lazily streaming out to
			-- the client.
			waitclientresponse
	  where
		sendbs bs = do
			sendbytestring bs
			when (protoversion > ProtocolVersion 0) $
				sendmessage (VALIDITY Valid)
			
		waitclientresponse = 
			receivemessage >>= \case
				Just SUCCESS -> return ()
				Just FAILURE -> return ()
				Just _ -> giveup "protocol error"
				Nothing -> return ()

{- Check if this repository can proxy for a specified remote uuid,
 - and if so enable proxying for it. -}
checkCanProxy :: UUID -> UUID -> Annex Bool
checkCanProxy remoteuuid ouruuid = checkCanProxy' remoteuuid ouruuid >>= \case
	Right v -> do
		Annex.changeState $ \st -> st { Annex.proxyremote = Just v }
		return True
	Left Nothing -> return False
	Left (Just err) -> giveup err

checkCanProxy' :: UUID -> UUID -> Annex (Either (Maybe String) (Either ClusterUUID Remote))
checkCanProxy' remoteuuid ouruuid = M.lookup ouruuid <$> getProxies >>= \case
	Nothing -> return (Left Nothing)
	-- This repository has (or had) proxying enabled. So it's
	-- ok to display error messages that talk about proxies.
	Just proxies ->
		case filter (\p -> proxyRemoteUUID p == remoteuuid) (S.toList proxies) of
			[] -> notconfigured
			ps -> case mkClusterUUID remoteuuid of
				Just cu -> proxyforcluster cu
				Nothing -> proxyfor ps
  where
	-- This repository may have multiple remotes that access the same
	-- repository. Proxy for the lowest cost one that is configured to
	-- be used as a proxy.
	proxyfor ps = do
		rs <- concat . Remote.byCost <$> Remote.remoteList
		myclusters <- annexClusters <$> Annex.getGitConfig
		let sameuuid r = Remote.uuid r == remoteuuid
		let samename r p = Remote.name r == proxyRemoteName p
		case headMaybe (filter (\r -> sameuuid r && proxyisconfigured rs myclusters r && any (samename r) ps) rs) of
			Nothing -> notconfigured
			Just r -> return (Right (Right r))
	
	-- Only proxy for a remote when the git configuration
	-- allows it. This is important to prevent changes to 
	-- the git-annex branch causing unexpected proxying for remotes.
	proxyisconfigured rs myclusters r
		| remoteAnnexProxy (Remote.gitconfig r) = True
		-- Proxy for remotes that are configured as cluster nodes.
		| any (`M.member` myclusters) (fromMaybe [] $ remoteAnnexClusterNode $ Remote.gitconfig r) = True
		-- Proxy for a remote when it is proxied by another remote
		-- which is itself configured as a cluster gateway.
		| otherwise = case remoteAnnexProxiedBy (Remote.gitconfig r) of
			Just proxyuuid -> not $ null $ 
				concatMap (remoteAnnexClusterGateway . Remote.gitconfig) $
					filter (\p -> Remote.uuid p == proxyuuid) rs
			Nothing -> False

	proxyforcluster cu = do
		clusters <- getClusters
		if M.member cu (clusterUUIDs clusters)
			then return (Right (Left cu))
			else notconfigured

	notconfigured = M.lookup remoteuuid <$> uuidDescMap >>= \case
		Just desc -> return $ Left $ Just $
			"not configured to proxy for repository " ++ fromUUIDDesc desc
		Nothing -> return $ Left Nothing

mkProxyMethods :: ProxyMethods
mkProxyMethods = ProxyMethods
	{ removedContent = \u k -> logChange k u InfoMissing
	, addedContent = \u k -> logChange k u InfoPresent
	}
