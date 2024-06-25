{- P2P protocol proxying
 -
 - Copyright 2024 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE RankNTypes, FlexibleContexts, ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}

module P2P.Proxy where

import Annex.Common
import qualified Annex
import P2P.Protocol
import P2P.IO
import Utility.Metered
import Git.FilePath
import Types.Concurrency
import Annex.Concurrent

import Data.Either
import Control.Concurrent.STM
import Control.Concurrent.Async
import qualified Control.Concurrent.MSem as MSem
import qualified Data.ByteString.Lazy as L
import GHC.Conc

type ProtoCloser = Annex ()

data ClientSide = ClientSide RunState P2PConnection

data RemoteSide = RemoteSide
	{ remoteUUID :: UUID
	, remoteConnect :: Annex (Maybe (RunState, P2PConnection, ProtoCloser))
	, remoteTMVar :: TMVar (RunState, P2PConnection, ProtoCloser)
	}

mkRemoteSide :: UUID -> Annex (Maybe (RunState, P2PConnection, ProtoCloser)) -> Annex RemoteSide
mkRemoteSide remoteuuid remoteconnect = RemoteSide
	<$> pure remoteuuid
	<*> pure remoteconnect
	<*> liftIO (atomically newEmptyTMVar)

runRemoteSide :: RemoteSide -> Proto a -> Annex (Either ProtoFailure a)
runRemoteSide remoteside a = 
	liftIO (atomically $ tryReadTMVar $ remoteTMVar remoteside) >>= \case
		Just (runst, conn, _closer) -> liftIO $ runNetProto runst conn a
		Nothing -> remoteConnect remoteside >>= \case
			Just (runst, conn, closer) -> do
				liftIO $ atomically $ putTMVar
					(remoteTMVar remoteside)
					(runst, conn, closer)
				liftIO $ runNetProto runst conn a
			Nothing -> giveup "Unable to connect to remote."

closeRemoteSide :: RemoteSide -> Annex ()
closeRemoteSide remoteside = 
	liftIO (atomically $ tryReadTMVar $ remoteTMVar remoteside) >>= \case
		Just (_, _, closer) -> closer
		Nothing -> return ()

{- Selects what remotes to proxy to for top-level P2P protocol
 - actions.
 - -}
data ProxySelector = ProxySelector
	{ proxyCHECKPRESENT :: Key -> Annex (Maybe RemoteSide)
	, proxyLOCKCONTENT :: Key -> Annex (Maybe RemoteSide)
	, proxyUNLOCKCONTENT :: Annex (Maybe RemoteSide)
	, proxyREMOVE :: Key -> Annex [RemoteSide]
	-- ^ remove from all of these remotes
	, proxyGET :: Key -> Annex (Maybe RemoteSide)
	, proxyPUT :: AssociatedFile -> Key -> Annex [RemoteSide]
	-- ^ put to some/all of these remotes
	}

singleProxySelector :: RemoteSide -> ProxySelector
singleProxySelector r = ProxySelector
	{ proxyCHECKPRESENT = const (pure (Just r))
	, proxyLOCKCONTENT = const (pure (Just r))
	, proxyUNLOCKCONTENT = pure (Just r)
	, proxyREMOVE = const (pure [r])
	, proxyGET = const (pure (Just r))
	, proxyPUT = const (const (pure [r]))
	}

{- To keep this module limited to P2P protocol actions,
 - all other actions that a proxy needs to do are provided
 - here. -}
data ProxyMethods = ProxyMethods
	{ removedContent :: UUID -> Key -> Annex ()
	-- ^ called when content is removed from a repository
	, addedContent :: UUID -> Key -> Annex ()
	-- ^ called when content is added to a repository
	}

{- Type of function that takes a error handler, which is
 - used to handle a ProtoFailure when receiving a message
 - from the client or remote.
 -}
type ProtoErrorHandled r = 
	(forall t. ((t -> Annex r) -> Annex (Either ProtoFailure t) -> Annex r)) -> Annex r

{- This is the first thing run when proxying with a client. 
 - The client has already authenticated. Most clients will send a
 - VERSION message, although version 0 clients will not and will send
 - some other message.
 -
 - But before the client will send VERSION, it needs to see AUTH_SUCCESS.
 - So send that, although the connection with the remote is not actually
 - brought up yet.
 -}
getClientProtocolVersion 
	:: UUID
	-> ClientSide
	-> (Maybe (ProtocolVersion, Maybe Message) -> Annex r)
	-> ProtoErrorHandled r
getClientProtocolVersion remoteuuid (ClientSide clientrunst clientconn) cont protoerrhandler =
	protoerrhandler cont $ client $ getClientProtocolVersion' remoteuuid
  where
	client = liftIO . runNetProto clientrunst clientconn

getClientProtocolVersion'
	:: UUID
	-> Proto (Maybe (ProtocolVersion, Maybe Message))
getClientProtocolVersion' remoteuuid = do
	net $ sendMessage (AUTH_SUCCESS remoteuuid)
	msg <- net receiveMessage
	case msg of
		Nothing -> return Nothing
		Just (VERSION v) -> 
			-- If the client sends a newer version than we
			-- understand, reduce it; we need to parse the
			-- protocol too.
			let v' = min v maxProtocolVersion
			in return (Just (v', Nothing))
		Just othermsg -> return
			(Just (defaultProtocolVersion, Just othermsg))

{- Proxy between the client and the remote. This picks up after
 - getClientProtocolVersion.
 -}
proxy 
	:: Annex r
	-> ProxyMethods
	-> ServerMode
	-> ClientSide
	-> UUID
	-> ProxySelector
	-> ConcurrencyConfig
	-> ProtocolVersion
	-- ^ Protocol version being spoken between the proxy and the
	-- client. When there are multiple remotes, some may speak an
	-- earlier version.
	-> Maybe Message
	-- ^ non-VERSION message that was received from the client when
	-- negotiating protocol version, and has not been responded to yet
	-> ProtoErrorHandled r
proxy proxydone proxymethods servermode (ClientSide clientrunst clientconn) remoteuuid proxyselector concurrencyconfig (ProtocolVersion protocolversion) othermessage protoerrhandler = do
	case othermessage of
		Nothing -> protoerrhandler proxynextclientmessage $ 
			client $ net $ sendMessage $ VERSION $ ProtocolVersion protocolversion
		Just message -> proxyclientmessage (Just message)
  where
	client = liftIO . runNetProto clientrunst clientconn

	proxynextclientmessage () = protoerrhandler proxyclientmessage $
		client (net receiveMessage)

	servermodechecker c a = c servermode $ \case
		Nothing -> a
		Just notallowed -> 
			protoerrhandler proxynextclientmessage $
				client notallowed

	proxyclientmessage Nothing = proxydone
	proxyclientmessage (Just message) = case message of
		CHECKPRESENT k -> proxyCHECKPRESENT proxyselector k >>= \case
			Just remoteside -> 
				proxyresponse remoteside message
					(const proxynextclientmessage)
			Nothing -> 
				protoerrhandler proxynextclientmessage $
					client $ net $ sendMessage FAILURE
		LOCKCONTENT k -> proxyLOCKCONTENT proxyselector k >>= \case
			Just remoteside -> 
				proxyresponse remoteside message 
					(const proxynextclientmessage)
			Nothing ->
				protoerrhandler proxynextclientmessage $
					client $ net $ sendMessage FAILURE
		UNLOCKCONTENT -> proxyUNLOCKCONTENT proxyselector >>= \case
			Just remoteside ->
				proxynoresponse remoteside message
					proxynextclientmessage
			Nothing -> proxynextclientmessage ()
		REMOVE k -> do
			remotesides <- proxyREMOVE proxyselector k
			servermodechecker checkREMOVEServerMode $
				handleREMOVE remotesides k message
		GET _ _ k -> proxyGET proxyselector k >>= \case
			Just remoteside -> handleGET remoteside message
			Nothing -> 
				protoerrhandler proxynextclientmessage $
					client $ net $ sendMessage $ 
						ERROR "content not present"
		PUT paf k -> do
			af <- getassociatedfile paf
			remotesides <- proxyPUT proxyselector af k
			servermodechecker checkPUTServerMode $
				handlePUT remotesides k message
		-- These messages involve the git repository, not the
		-- annex. So they affect the git repository of the proxy,
		-- not the remote.
		CONNECT service -> 
			servermodechecker (checkCONNECTServerMode service) $
				-- P2P protocol does not continue after
				-- relaying from git.
				protoerrhandler (\() -> proxydone) $
					client $ net $ relayService service 
		NOTIFYCHANGE -> protoerr
		-- Messages that the client should only send after one of
		-- the messages above.
		SUCCESS -> protoerr
		SUCCESS_PLUS _ -> protoerr
		FAILURE -> protoerr
		FAILURE_PLUS _ -> protoerr
		DATA _ -> protoerr
		VALIDITY _ -> protoerr
		-- If the client errors out, give up.
		ERROR msg -> giveup $ "client error: " ++ msg
		-- Messages that only the server should send.
		CONNECTDONE _ -> protoerr
		CHANGED _ -> protoerr
		AUTH_SUCCESS _ -> protoerr
		AUTH_FAILURE -> protoerr
		PUT_FROM _ -> protoerr
		ALREADY_HAVE -> protoerr
		ALREADY_HAVE_PLUS _ -> protoerr
		-- Early messages that the client should not send now.
		AUTH _ _ -> protoerr
		VERSION _ -> protoerr

	-- Send a message to the remote, send its response back to the
	-- client, and pass it to the continuation.
	proxyresponse remoteside message a = 
		getresponse (runRemoteSide remoteside) message $ \resp ->
			protoerrhandler (a resp) $
				client $ net $ sendMessage resp
	
	-- Send a message to the remote, that it will not respond to.
	proxynoresponse remoteside message a =
		protoerrhandler a $
			runRemoteSide remoteside $ net $ sendMessage message
	
	-- Send a message to the endpoint and get back its response.
	getresponse endpoint message handleresp =
		protoerrhandler (withresp handleresp) $ 
			endpoint $ net $ do
				sendMessage message
				receiveMessage

	withresp a (Just resp) = a resp
	-- Whichever of the remote or client the message was read from 
	-- hung up.
	withresp _ Nothing = proxydone

	-- Read a message from one party, send it to the other,
	-- and then pass the message to the continuation.
	relayonemessage from to cont =
		flip protoerrhandler (from $ net $ receiveMessage) $
			withresp $ \message ->
				protoerrhandler (cont message) $
					to $ net $ sendMessage message
	
	protoerr = do
		_ <- client $ net $ sendMessage (ERROR "protocol error")
		giveup "protocol error"
	
	handleREMOVE [] _ _ =
		-- When no places are provided to remove from,
		-- don't report a successful remote.
		protoerrhandler proxynextclientmessage $
			client $ net $ sendMessage FAILURE	
	handleREMOVE remotesides k message = do
		v <- forMC concurrencyconfig remotesides $ \r ->
			runRemoteSideOrSkipFailed r $ do
				net $ sendMessage message
				net receiveMessage >>= return . \case
					Just SUCCESS ->
						Just (True, [remoteUUID r])
					Just (SUCCESS_PLUS us) -> 
						Just (True, remoteUUID r:us)
					Just FAILURE ->
						Just (False, [])
					Just (FAILURE_PLUS us) ->
						Just (False, us)
					_ -> Nothing
		let v' = map join v
		let us = concatMap snd $ catMaybes v'
		mapM_ (\u -> removedContent proxymethods u k) us
		protoerrhandler proxynextclientmessage $
			client $ net $ sendMessage $
				let nonplussed = all (== remoteuuid) us 
					|| protocolversion < 2
				in if all (maybe False fst) v'
					then if nonplussed
						then SUCCESS
						else SUCCESS_PLUS us
					else if nonplussed
						then FAILURE
						else FAILURE_PLUS us

	handleGET remoteside message = getresponse (runRemoteSide remoteside) message $
		withDATA (relayGET remoteside)

	handlePUT (remoteside:[]) k message
		| remoteUUID remoteside == remoteuuid =
			getresponse (runRemoteSide remoteside) message $ \resp -> case resp of
				ALREADY_HAVE -> protoerrhandler proxynextclientmessage $
					client $ net $ sendMessage resp
				ALREADY_HAVE_PLUS _ -> protoerrhandler proxynextclientmessage $
					client $ net $ sendMessage resp
				PUT_FROM _ -> 
					getresponse client resp $ 
						withDATA (relayPUT remoteside k)
				_ -> protoerr
	handlePUT [] _ _ = 
		protoerrhandler proxynextclientmessage $
			client $ net $ sendMessage ALREADY_HAVE
	handlePUT remotesides k message = 
		handlePutMulti remotesides k message

	withDATA a message@(DATA len) = a len message
	withDATA _ _ = protoerr
	
	relayGET remoteside len = relayDATAStart client $
		relayDATACore len (runRemoteSide remoteside) client $
			relayDATAFinish (runRemoteSide remoteside) client $
				relayonemessage client (runRemoteSide remoteside) $
					const proxynextclientmessage
	
	relayPUT remoteside k len = relayDATAStart (runRemoteSide remoteside) $
		relayDATACore len client (runRemoteSide remoteside) $
			relayDATAFinish client (runRemoteSide remoteside) $
				relayonemessage (runRemoteSide remoteside) client finished
	  where
		finished resp () = do
			void $ relayPUTRecord k remoteside resp
			proxynextclientmessage ()

	relayPUTRecord k remoteside SUCCESS = do
		addedContent proxymethods (remoteUUID remoteside) k
		return $ Just [remoteUUID remoteside]
	relayPUTRecord k remoteside (SUCCESS_PLUS us) = do
		let us' = remoteUUID remoteside : us
		forM_ us' $ \u ->
			addedContent proxymethods u k
		return $ Just us'
	relayPUTRecord _ _ _ =
		return Nothing

	handlePutMulti remotesides k message = do
		let initiate remoteside = do
			resp <- runRemoteSideOrSkipFailed remoteside $ net $ do
                                  sendMessage message
                                  receiveMessage
			case resp of
				Just (Just (PUT_FROM (Offset offset))) -> 
					return $ Right $
						Right (remoteside, offset)
				Just (Just ALREADY_HAVE) -> 
					return $ Right $ Left remoteside
				Just (Just _) -> protoerr
				Just Nothing -> return (Left ())
				Nothing -> return (Left ())
		let alreadyhave = \case
			Right (Left _) -> True
			_ -> False
		l <- forMC concurrencyconfig remotesides initiate
		if all alreadyhave l
			then if protocolversion < 2
				then protoerrhandler proxynextclientmessage $
					client $ net $ sendMessage ALREADY_HAVE
				else protoerrhandler proxynextclientmessage $
					client $ net $ sendMessage $ ALREADY_HAVE_PLUS $
						filter (/= remoteuuid) $
							map remoteUUID (lefts (rights l))
			else if null (rights l)
				-- no response from any remote
				then proxydone
				else do
					let l' = rights (rights l)
					let minoffset = minimum (map snd l')
					getresponse client (PUT_FROM (Offset minoffset)) $
						withDATA (relayPUTMulti minoffset l' k)	
	
	relayPUTMulti minoffset remotes k (Len datalen) _ = do
		let totallen = datalen + minoffset
		-- Tell each remote how much data to expect, depending
		-- on the remote's offset.
		rs <- forMC concurrencyconfig remotes $ \remote@(remoteside, remoteoffset) ->
			runRemoteSideOrSkipFailed remoteside $ do
				net $ sendMessage $ DATA $ Len $
					totallen - remoteoffset
				return remote
		protoerrhandler (send (catMaybes rs) minoffset) $
			client $ net $ receiveBytes (Len datalen) nullMeterUpdate
	  where
		chunksize = fromIntegral defaultChunkSize
		
	  	-- Stream the lazy bytestring out to the remotes in chunks.
		-- Only start sending to a remote once past its desired
		-- offset.
		send rs n b = do
			let (chunk, b') = L.splitAt chunksize b
			let chunklen = fromIntegral (L.length chunk)
			let !n' = n + chunklen
			rs' <- forMC concurrencyconfig rs $ \r@(remoteside, remoteoffset) ->
				if n >= remoteoffset
					then runRemoteSideOrSkipFailed remoteside $ do
						net $ sendBytes (Len chunklen) chunk nullMeterUpdate
						return r
					else if (n' > remoteoffset)
						then do
							let chunkoffset = remoteoffset - n
							let subchunklen = chunklen - chunkoffset
							let subchunk = L.drop (fromIntegral chunkoffset) chunk
							runRemoteSideOrSkipFailed remoteside $ do
								net $ sendBytes (Len subchunklen) subchunk nullMeterUpdate
								return r
						else return (Just r)
			if L.null b'
				then sent (catMaybes rs')
				else send (catMaybes rs') n' b'

		sent [] = proxydone
		sent rs = relayDATAFinishMulti k (map fst rs)
	
	runRemoteSideOrSkipFailed remoteside a = 
		runRemoteSide remoteside a >>= \case
			Right v -> return (Just v)
			Left _ -> do
				-- This connection to the remote is
				-- unrecoverable at this point, so close it.
				closeRemoteSide remoteside
				return Nothing

	relayDATAStart x receive message =
		protoerrhandler (\() -> receive) $
			x $ net $ sendMessage message

	relayDATACore len x y a = protoerrhandler send $
			x $ net $ receiveBytes len nullMeterUpdate
	  where
		send b = protoerrhandler a $
			y $ net $ sendBytes len b nullMeterUpdate
	
	relayDATAFinish x y sendsuccessfailure ()
		| protocolversion == 0 = sendsuccessfailure
		-- Protocol version 1 has a VALID or
		-- INVALID message after the data.
		| otherwise = relayonemessage x y (\_ () -> sendsuccessfailure)

	relayDATAFinishMulti k rs
		| protocolversion == 0 = 
			finish $ net receiveMessage
		| otherwise =
			flip protoerrhandler (client $ net $ receiveMessage) $
				withresp $ \message ->
					finish $ do
						-- Relay VALID or INVALID message
						-- only to remotes that support
						-- protocol version 1.
						net getProtocolVersion >>= \case
							ProtocolVersion 0 -> return ()
							_ -> net $ sendMessage message
						net receiveMessage			
	  where
		finish a = do
			storeduuids <- forMC concurrencyconfig rs $ \r -> 
				runRemoteSideOrSkipFailed r a >>= \case
					Just (Just resp) ->
						relayPUTRecord k r resp
					_ -> return Nothing
			protoerrhandler proxynextclientmessage $
				client $ net $ sendMessage $
					case concat (catMaybes storeduuids) of
						[] -> FAILURE
						us
							| protocolversion < 2 -> SUCCESS
							| otherwise -> SUCCESS_PLUS us

	-- The associated file received from the P2P protocol
	-- is relative to the top of the git repository. But this process
	-- may be running with a different cwd.
	getassociatedfile (ProtoAssociatedFile (AssociatedFile (Just f))) =
		AssociatedFile . Just 
			<$> fromRepo (fromTopFilePath (asTopFilePath f))
	getassociatedfile (ProtoAssociatedFile (AssociatedFile Nothing)) = 
		return $ AssociatedFile Nothing

data ConcurrencyConfig = ConcurrencyConfig Int (MSem.MSem Int)

noConcurrencyConfig :: Annex ConcurrencyConfig
noConcurrencyConfig = liftIO $ ConcurrencyConfig 1 <$> MSem.new 1

getConcurrencyConfig :: Annex ConcurrencyConfig
getConcurrencyConfig = (annexJobs <$> Annex.getGitConfig) >>= \case
	NonConcurrent -> noConcurrencyConfig
	Concurrent n -> go n
	ConcurrentPerCpu -> go =<< liftIO getNumProcessors
  where
	go n = do
		c <- liftIO getNumCapabilities
		when (n > c) $
			liftIO $ setNumCapabilities n
		setConcurrency (ConcurrencyGitConfig (Concurrent n))
		msem <- liftIO $ MSem.new n
		return (ConcurrencyConfig n msem)

forMC :: ConcurrencyConfig -> [a] -> (a -> Annex b) -> Annex [b]
forMC _ (x:[]) a = do
	r <- a x
	return [r]
forMC (ConcurrencyConfig n msem) xs a
	| n < 2 = forM xs a
	| otherwise = do
		runners <- forM xs $ \x ->
			forkState $ bracketIO 
				(MSem.wait msem)
				(const $ MSem.signal msem)
				(const $ a x)
		mapM id =<< liftIO (forConcurrently runners id)

