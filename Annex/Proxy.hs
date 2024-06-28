{- proxying
 -
 - Copyright 2024 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Annex.Proxy where

import Annex.Common
import P2P.Proxy
import P2P.Protocol
import P2P.IO
import qualified Remote
import qualified Types.Remote as Remote
import qualified Remote.Git
import Remote.Helper.Ssh (openP2PShellConnection', closeP2PShellConnection)
import Annex.Concurrent

import Control.Concurrent.STM
import Control.Concurrent.Async
import qualified Data.ByteString.Lazy as L

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
	endv <- liftIO newEmptyTMVarIO
	worker <- liftIO . async =<< forkState
		(proxySpecialRemote protoversion r ihdl ohdl endv)
	let remoteconn = P2PConnection
		{ connRepo = Nothing
		, connCheckAuth = const False
		, connIhdl = P2PHandleTMVar ihdl
		, connOhdl = P2PHandleTMVar ohdl
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
	-> Annex ()
proxySpecialRemote protoversion r ihdl ohdl endv = go
  where
	go = receivemessage >>= \case
		Just (BYPASS _) -> go
		Just (CHECKPRESENT k) -> do
			tryNonAsync (Remote.checkPresent r k) >>= \case
				Right True -> sendmessage SUCCESS
				Right False -> sendmessage FAILURE
				Left err -> sendmessage (ERROR (show err))
			go
		Just (LOCKCONTENT _) -> do
			-- Special remotes do not support locking content.
			sendmessage FAILURE
			go
		Just (REMOVE k) -> do
			tryNonAsync (Remote.removeKey r k) >>= \case
				Right () -> sendmessage SUCCESS
				Left _ -> sendmessage FAILURE
			go
		Just (PUT af k) -> giveup "TODO PUT" -- XXX
		Just (GET offset af k) -> giveup "TODO GET" -- XXX
		Just (CONNECT _) -> 
			-- Not supported and the protocol ends here.
			sendmessage $ CONNECTDONE (ExitFailure 1)	
		Just NOTIFYCHANGE -> do
			sendmessage (ERROR "NOTIFYCHANGE unsupported for a special remote")
			go
		Just _ -> giveup "protocol error"
		Nothing -> return ()

	getnextmessageorend = 
		liftIO $ atomically $ 
			(Right <$> takeTMVar ohdl)
				`orElse`
			(Left <$> takeTMVar endv)

	receivemessage = getnextmessageorend >>= \case
		Right (Right m) -> return (Just m)
		Right (Left _b) -> giveup "unexpected ByteString received from P2P MVar"
		Left () -> return Nothing
	--receivebytestring = liftIO (atomically $ takeTMVar ohdl) >>= \case
	--	Left b -> return b
	--	Right _m -> giveup "did not receive ByteString from P2P MVar"

	sendmessage m = liftIO $ atomically $ putTMVar ihdl (Right m)
	sendbytestring b = liftIO $ atomically $ putTMVar ihdl (Left b)
