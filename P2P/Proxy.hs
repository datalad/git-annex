{- P2P protocol proxying
 -
 - Copyright 2024 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE RankNTypes, FlexibleContexts #-}

module P2P.Proxy where

import Annex.Common
import P2P.Protocol
import P2P.IO
import qualified Remote

data ClientSide = ClientSide RunState P2PConnection
data RemoteSide = RemoteSide RunState P2PConnection

{- Type of function that takes a error handler, which is
 - used to handle a ProtoFailure when receiving a message
 - from the client or remote.
 -}
type ProtoErrorHandled m r = 
	(forall t. ((t -> m r) -> m (Either ProtoFailure t) -> m r)) -> m r

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
	:: Remote 
	-> ClientSide
	-> (Maybe (ProtocolVersion, Maybe Message) -> Annex r)
	-> ProtoErrorHandled Annex r
getClientProtocolVersion remote (ClientSide clientrunst clientconn) cont protoerrhandler =
	protoerrhandler cont $
		liftIO $ runNetProto clientrunst clientconn $
			getClientProtocolVersion' remote

getClientProtocolVersion'
	:: Remote
	-> Proto (Maybe (ProtocolVersion, Maybe Message))
getClientProtocolVersion' remote = do
	net $ sendMessage (AUTH_SUCCESS (Remote.uuid remote))
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
 - getClientProtocolVersion, after the connection to the remote has
 - been made, and the protocol version negotiated with the remote.
 -}
proxy 
	:: Annex r
	-> ServerMode
	-> ClientSide
	-> RemoteSide
	-> Maybe Message
	-- ^ non-VERSION message that was received from the client when
	-- negotiating protocol version, and has not been responded to yet
	-> ProtoErrorHandled Annex r
proxy proxydone servermode clientside remoteside othermessage protoerrhandler = do
	case othermessage of
		Just message -> proxyclientmessage (Just message)
		Nothing -> do
			-- Send client the VERSION from the remote.
			proxyprotocolversion <- 
				either (const defaultProtocolVersion) id
					<$> toremote (net getProtocolVersion)
			protoerrhandler proxynextclientmessage $ 
				toclient $ net $ sendMessage 
					(VERSION proxyprotocolversion)
  where
	ClientSide clientrunst clientconn = clientside
	RemoteSide remoterunst remoteconn = remoteside
	
	toremote = liftIO . runNetProto remoterunst remoteconn
	toclient = liftIO . runNetProto clientrunst clientconn

	proxynextclientmessage () = protoerrhandler proxyclientmessage $
		toclient (net receiveMessage)

	-- Send a message to the remote and then
	-- send its response back to the client.
	proxyresponse message = 
		protoerrhandler handleresp $ 
			toremote $ net $ do
				sendMessage message
				receiveMessage
	  where
	  	handleresp (Just resp) =
			protoerrhandler proxynextclientmessage $
				toclient $ net $ sendMessage resp
		-- Remote hung up
		handleresp Nothing = proxydone
	
	-- Send a message to the remote, that it will not respond to.
	proxynoresponse message =
		protoerrhandler proxynextclientmessage $
			toremote $ net $ sendMessage message

	servermodechecker c a = c servermode $ \case
		Nothing -> a
		Just notallowed -> 
			protoerrhandler proxynextclientmessage $
				toclient notallowed

	proxyclientmessage Nothing = proxydone
	proxyclientmessage (Just message) = case message of
		CHECKPRESENT _ -> proxyresponse message
		LOCKCONTENT _ -> proxyresponse message
		UNLOCKCONTENT -> proxynoresponse message
		REMOVE _ -> 
			servermodechecker checkREMOVEServerMode $
				proxyresponse message
		GET offset af k -> giveup "TODO GET"
		PUT af k ->
			servermodechecker checkPUTServerMode $
				giveup "TODO PUT"
		-- These messages involve the git repository, not the
		-- annex. So they affect the git repository of the proxy,
		-- not the remote.
		CONNECT service -> 
			servermodechecker (checkCONNECTServerMode service) $
				giveup "TODO CONNECT"
		NOTIFYCHANGE -> giveup "TODO NOTIFYCHANGE"
		-- Messages that the client should only send after one of
		-- the messages above.
		SUCCESS -> protoerr
		FAILURE -> protoerr
		DATA len -> protoerr
		VALIDITY v -> protoerr
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

	protoerr = do
		_ <- toclient $ net $ sendMessage (ERROR "protocol error")
		giveup "protocol error"
