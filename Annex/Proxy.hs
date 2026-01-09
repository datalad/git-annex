{- proxying
 -
 - Copyright 2024 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

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
import Annex.Concurrent
import Annex.Tmp
import Annex.Verify
import Annex.UUID
import Logs.Proxy
import Logs.Cluster
import Logs.UUID
import Logs.Location
import Utility.Tmp.Dir
import Utility.Metered
import qualified Database.Export as Export
import qualified Utility.FileIO as F
#ifndef mingw32_HOST_OS
import Utility.OpenFile
#endif

#ifndef mingw32_HOST_OS
import Control.Concurrent
#endif
import Control.Concurrent.STM
import Control.Concurrent.Async
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Map as M
import qualified Data.Set as S
#ifndef mingw32_HOST_OS
import qualified Data.ByteString as BS
import System.IO.Unsafe
#endif

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
	exportdb <- ifM (Remote.isExportSupported r)
		( Just <$> Export.openDb (Remote.uuid r)
		, pure Nothing
		)
	worker <- liftIO . async =<< forkState
		(proxySpecialRemote protoversion r ihdl ohdl owaitv oclosedv exportdb)
	let remoteconn = P2PConnection
		{ connRepo = Nothing
		, connCheckAuth = const False
		, connIhdl = P2PHandleTMVar ihdl (Just iwaitv) iclosedv
		, connOhdl = P2PHandleTMVar ohdl (Just owaitv) oclosedv
		, connProcess = Nothing
		, connIdent = ConnIdent (Just (Remote.name r))
		}
	let closeremoteconn = do
		liftIO $ atomically $ putTMVar oclosedv ()
		join $ liftIO (wait worker)
		maybe noop Export.closeDb exportdb
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
	-> Maybe Export.ExportHandle
	-> Annex ()
proxySpecialRemote protoversion r ihdl ohdl owaitv oclosedv mexportdb = go
  where
	go :: Annex ()
	go = liftIO receivemessage >>= \case
		Just (CHECKPRESENT k) -> do
			checkpresent k
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

	receivemessage = liftIO (atomically recv) >>= \case
		Right (Right m) -> return (Just m)
		Right (Left _b) -> giveup "unexpected ByteString received from P2P MVar"
		Left () -> return Nothing
	  where
		recv = 
			(Right <$> takeTMVar ohdl)
				`orElse`
			(Left <$> readTMVar oclosedv)
	
	receivebytestring = atomically recv >>= \case
		Right (Left b) -> return (Just b)
		Right (Right _m) -> giveup "did not receive ByteString from P2P MVar"
		Left () -> return Nothing
	  where
		recv = 
			(Right <$> takeTMVar ohdl)
				`orElse`
			(Left <$> readTMVar oclosedv)

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
		withTmpDirIn othertmpdir (literalOsPath "proxy") $ \tmpdir ->
			a (tmpdir </> keyFile k)
			
	proxyput af k = do
		liftIO $ sendmessage $ PUT_FROM (Offset 0)
		liftIO receivemessage >>= \case
			Just (DATA (Len len)) -> withproxytmpfile k $ \tmpfile -> do
				-- Verify the content received from
				-- the client, to avoid bad content
				-- being stored in the special remote.
				iv <- startVerifyKeyContentIncrementally Remote.AlwaysVerify k
				h <- liftIO $ F.openFile tmpfile WriteMode
				let nuketmp = liftIO $ removeWhenExistsWith removeFile tmpfile
				gotall <- liftIO $ receivetofile iv h len
				liftIO $ hClose h
				verified <- if gotall
					then fst <$> finishVerifyKeyContentIncrementally' True iv
					else pure False
				let store = tryNonAsync (storeput k af tmpfile) >>= \case
					Right () -> liftIO $ sendmessage SUCCESS
					Left err -> liftIO $ propagateerror err
				if protoversion > ProtocolVersion 1
					then liftIO receivemessage >>= \case
						Just (VALIDITY Valid)
							| verified -> store >> nuketmp
							| otherwise -> do
								nuketmp
								liftIO $ sendmessage FAILURE
						Just (VALIDITY Invalid) -> do
							nuketmp
							liftIO $ sendmessage FAILURE
						_ -> do
							nuketmp
							giveup "protocol error"
					else store >> nuketmp
			Just DATA_PRESENT -> checkpresent k
			_ -> giveup "protocol error"

	checkpresent k = 
		tryNonAsync (Remote.checkPresent r k) >>= \case
			Right True -> liftIO $ sendmessage SUCCESS
			Right False -> liftIO $ sendmessage FAILURE
			Left err -> liftIO $ propagateerror err

	storeput k af tmpfile = case mexportdb of
		Just exportdb -> liftIO (Export.getExportTree exportdb k) >>= \case
			[] -> storeputkey k af tmpfile
			locs -> do
				havelocs <- liftIO $ S.fromList
					<$> Export.getExportedLocation exportdb k
				let locs' = filter (`S.notMember` havelocs) locs
				forM_ locs' $ \loc ->
					storeputexport exportdb k loc tmpfile
				liftIO $ Export.flushDbQueue exportdb
		Nothing -> storeputkey k af tmpfile
	
	storeputkey k af tmpfile = 
		Remote.storeKey r k af (Just tmpfile) nullMeterUpdate
	
	storeputexport exportdb k loc tmpfile = do
		Remote.storeExport (Remote.exportActions r) tmpfile k loc nullMeterUpdate
		liftIO $ Export.addExportedLocation exportdb k loc

	receivetofile iv h n = liftIO receivebytestring >>= \case
		Just b -> do
			n' <- storetofile iv h n (L.toChunks b)
			liftIO $ atomically $ 
				putTMVar owaitv ()
					`orElse`
				readTMVar oclosedv
			-- Normally all the data is sent in a single
			-- lazy bytestring. However, when the special
			-- remote is a node in a cluster, a PUT is
			-- streamed to it in multiple chunks.
			if n' == 0 
				then return True
				else receivetofile iv h n'
		Nothing -> return False

	storetofile _ _ n [] = pure n
	storetofile iv h n (b:bs) = do
		writeVerifyChunk iv h b
		storetofile iv h (n - fromIntegral (B.length b)) bs

	proxyget offset af k = withproxytmpfile k $ \tmpfile -> do
		let retrieve = tryNonAsync $ Remote.retrieveKeyFile
			r k af tmpfile nullMeterUpdate vc
#ifndef mingw32_HOST_OS
		ordered <- Remote.retrieveKeyFileInOrder r
#else
		_ <- Remote.retrieveKeyFileInOrder r
#endif
		case fromKey keySize k of
#ifndef mingw32_HOST_OS
			Just size | size > 0 && ordered -> do
				cancelv <- liftIO newEmptyMVar
				donev <- liftIO newEmptyMVar
				streamer <- liftIO $ async $
					streamdata offset tmpfile size cancelv donev
				retrieve >>= \case
					Right _ -> liftIO $ do
						putMVar donev ()
						wait streamer
					Left err -> liftIO $ do
						putMVar cancelv ()
						wait streamer
						propagateerror err
#endif
			_ -> retrieve >>= \case
				Right _ -> liftIO $ senddata offset tmpfile
				Left err -> liftIO $ propagateerror err
	  where
		-- Don't verify the content from the remote,
		-- because the client will do its own verification.
		vc = Remote.NoVerify

#ifndef mingw32_HOST_OS
	streamdata (Offset offset) f size cancelv donev = do
		sendlen offset size
		waitforfile
		x <- tryNonAsync $ do
			h <- openFileBeingWritten (fromOsPath f)
			hSeek h AbsoluteSeek offset
			senddata' h (getcontents size)
		case x of
			Left err -> do
				hPutStrLn stderr (show err)
				throwM err
			Right res -> return res
	  where
		-- The file doesn't exist at the start.
		-- Wait for some data to be written to it as well,
		-- in case an empty file is first created and then
		-- overwritten. When there is an offset, wait for
		-- the file to get that large. Note that this is not used
		-- when the size is 0.
		waitforfile = tryNonAsync (fromIntegral <$> getFileSize f) >>= \case
			Right sz | sz > 0 && sz >= offset -> return ()
			_ -> ifM (isEmptyMVar cancelv <&&> isEmptyMVar donev)
				( do
					threadDelay 40000
					waitforfile
				, do
					return ()
				)

		getcontents n h = unsafeInterleaveIO $ do
			isdone <- (not <$> isEmptyMVar donev) <||> (not <$> isEmptyMVar cancelv)
			c <- BS.hGet h defaultChunkSize
			let n' = n - fromIntegral (BS.length c)
			let c' = L.fromChunks [BS.take (fromIntegral n) c]
			if BS.null c
				then if isdone
					then return mempty
					else do
						-- Wait for more data to be
						-- written to the file.
						threadDelay 40000
						getcontents n h
				else if n' > 0
					then do
						-- unsafeInterleaveIO causes 
						-- this to be deferred until
						-- data is read from the lazy
						-- ByteString.
						cs <- getcontents n' h
						return $ L.append c' cs
					else return c'
#endif

	senddata (Offset offset) f = do
		size <- fromIntegral <$> getFileSize f
		sendlen offset size
		F.withBinaryFile f ReadMode $ \h -> do
			hSeek h AbsoluteSeek offset
			senddata' h L.hGetContents

	senddata' h getcontents = do
		sendbs =<< getcontents h
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
	
	sendlen offset size = do
		let n = max 0 (size - offset)
		sendmessage $ DATA (Len n)


{- Check if this repository can proxy for a specified remote uuid,
 - and if so enable proxying for it. -}
checkCanProxy :: UUID -> UUID -> Annex Bool
checkCanProxy remoteuuid myuuid = do
	myproxies <- M.lookup myuuid <$> getProxies
	checkCanProxy' myproxies remoteuuid >>= \case
		Right v -> do
			Annex.changeState $ \st -> st { Annex.proxyremote = Just v }
			return True
		Left Nothing -> return False
		Left (Just err) -> giveup err

checkCanProxy' :: Maybe (S.Set Proxy) -> UUID -> Annex (Either (Maybe String) (Either ClusterUUID Remote))
checkCanProxy' Nothing _ = return (Left Nothing)
checkCanProxy' (Just proxies) remoteuuid =
	case filter (\p -> proxyRemoteUUID p == remoteuuid) (S.toList proxies) of
		[] -> notconfigured
		ps -> case mkClusterUUID remoteuuid of
			Just cu -> proxyforcluster cu
			Nothing -> proxyfor ps
  where
	proxyfor ps = do
		rs <- concat . Remote.byCost <$> Remote.remoteList
		myclusters <- annexClusters <$> Annex.getGitConfig
		case canProxyForRemote rs ps myclusters remoteuuid of
			Nothing -> notconfigured
			Just r -> return (Right (Right r))

	proxyforcluster cu = do
		clusters <- getClusters
		if M.member cu (clusterUUIDs clusters)
			then return (Right (Left cu))
			else notconfigured

	notconfigured = M.lookup remoteuuid <$> uuidDescMap >>= \case
		Just desc -> return $ Left $ Just $
			"not configured to proxy for repository " ++ fromUUIDDesc desc
		Nothing -> return $ Left Nothing

{- Remotes that this repository is configured to proxy for.
 - 
 - When there are multiple remotes that access the same repository,
 - this picks the lowest cost one that is configured to be used as a proxy.
 -}
proxyForRemotes :: Annex [Remote]
proxyForRemotes = do
	myuuid <- getUUID
	(M.lookup myuuid <$> getProxies) >>= \case
		Nothing -> return []
		Just myproxies -> do
			let myproxies' = S.toList myproxies
			rs <- concat . Remote.byCost <$> Remote.remoteList
			myclusters <- annexClusters <$> Annex.getGitConfig
			return $ mapMaybe (canProxyForRemote rs myproxies' myclusters . Remote.uuid) rs

-- Only proxy for a remote when the git configuration allows it.
-- This is important to prevent changes to the git-annex branch
-- causing unexpected proxying for remotes.
canProxyForRemote
	:: [Remote] -- ^ must be sorted by cost
	-> [Proxy]
	-> M.Map RemoteName ClusterUUID
	-> UUID
	-> (Maybe Remote)
canProxyForRemote rs myproxies myclusters remoteuuid =
	headMaybe $ filter canproxy rs
  where
	canproxy r =
		sameuuid r && 
		proxyisconfigured r &&
		any (isproxyfor r) myproxies
	
	sameuuid r = Remote.uuid r == remoteuuid

	isproxyfor r p = 
		proxyRemoteUUID p == remoteuuid &&
		Remote.name r == proxyRemoteName p

	proxyisconfigured r
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

mkProxyMethods :: ProxyMethods
mkProxyMethods = ProxyMethods
	{ removedContent = \lu u k -> logChange lu k u InfoMissing
	, addedContent = \lu u k -> logChange lu k u InfoPresent
	}
