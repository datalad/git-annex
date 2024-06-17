{- P2P protocol proxying
 -
 - Copyright 2024 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE RankNTypes, FlexibleContexts, ScopedTypeVariables #-}

module P2P.Proxy where

import Annex.Common
import P2P.Protocol
import P2P.IO
import Utility.Metered (nullMeterUpdate)

import Control.Concurrent.STM

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
			let v' = if v > maxProtocolVersion
				then maxProtocolVersion
				else v
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
	-> (Message -> Annex RemoteSide)
	-> ProtocolVersion
	-> Maybe Message
	-- ^ non-VERSION message that was received from the client when
	-- negotiating protocol version, and has not been responded to yet
	-> ProtoErrorHandled r
proxy proxydone proxymethods servermode (ClientSide clientrunst clientconn) getremoteside protocolversion othermessage protoerrhandler = do
	case othermessage of
		Nothing -> protoerrhandler proxynextclientmessage $ 
			client $ net $ sendMessage $ VERSION protocolversion
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
		CHECKPRESENT _ -> do
			remoteside <- getremoteside message
			proxyresponse remoteside message (const proxynextclientmessage)
		LOCKCONTENT _ -> do
			remoteside <- getremoteside message
			proxyresponse remoteside message (const proxynextclientmessage)
		UNLOCKCONTENT -> do
			remoteside <- getremoteside message
			proxynoresponse remoteside message proxynextclientmessage
		REMOVE k -> do
			remoteside <- getremoteside message
			servermodechecker checkREMOVEServerMode $
				handleREMOVE remoteside k message
		GET _ _ _ -> do
			remoteside <- getremoteside message
			handleGET remoteside message
		PUT _ k -> do
			remoteside <- getremoteside message
			servermodechecker checkPUTServerMode $
				handlePUT remoteside k message
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
		FAILURE -> protoerr
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
		
	handleREMOVE remoteside k message =
		proxyresponse remoteside message $ \resp () -> do
			case resp of
				SUCCESS -> removedContent proxymethods
					(remoteUUID remoteside) k
				_ -> return ()
			proxynextclientmessage ()

	handleGET remoteside message = getresponse (runRemoteSide remoteside) message $
		withDATA (relayGET remoteside)

	handlePUT remoteside k message = 
		getresponse (runRemoteSide remoteside) message $ \resp -> case resp of
			ALREADY_HAVE -> protoerrhandler proxynextclientmessage $
				client $ net $ sendMessage resp
			PUT_FROM _ -> 
				getresponse client resp $ withDATA (relayPUT remoteside k)
			_ -> protoerr

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
			case resp of
				SUCCESS -> addedContent proxymethods (remoteUUID remoteside) k
				_ -> return ()
			proxynextclientmessage ()

	relayDATAStart x receive message =
		protoerrhandler (\() -> receive) $
			x $ net $ sendMessage message

	relayDATACore len x y finishget = protoerrhandler send $
			x $ net $ receiveBytes len nullMeterUpdate
	  where
		send b = protoerrhandler finishget $
			y $ net $ sendBytes len b nullMeterUpdate
	
	relayDATAFinish x y sendsuccessfailure () = case protocolversion of
		ProtocolVersion 0 -> sendsuccessfailure
		-- Protocol version 1 has a VALID or
		-- INVALID message after the data.
		_ -> relayonemessage x y (\_ () -> sendsuccessfailure)

