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
import qualified Remote
import Utility.Metered (nullMeterUpdate)

type ProtoRunner = forall a. Proto a -> Annex (Either ProtoFailure a)

data ClientSide = ClientSide ProtoRunner
data RemoteSide = RemoteSide ProtoRunner UUID

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
	:: Remote 
	-> ClientSide
	-> (Maybe (ProtocolVersion, Maybe Message) -> Annex r)
	-> ProtoErrorHandled r
getClientProtocolVersion remote (ClientSide client) cont protoerrhandler =
	protoerrhandler cont $ client $ getClientProtocolVersion' remote

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
	-> ProxyMethods
	-> ServerMode
	-> ClientSide
	-> RemoteSide
	-> Maybe Message
	-- ^ non-VERSION message that was received from the client when
	-- negotiating protocol version, and has not been responded to yet
	-> ProtoErrorHandled r
proxy proxydone proxymethods servermode (ClientSide client) (RemoteSide remote remoteuuid) othermessage protoerrhandler = do
	case othermessage of
		Just message -> proxyclientmessage (Just message)
		Nothing -> do
			v <- protocolversion
			protoerrhandler proxynextclientmessage $ 
				client $ net $ sendMessage $ VERSION v
  where
	protocolversion = either (const defaultProtocolVersion) id
		<$> remote (net getProtocolVersion)

	proxynextclientmessage () = protoerrhandler proxyclientmessage $
		client (net receiveMessage)

	servermodechecker c a = c servermode $ \case
		Nothing -> a
		Just notallowed -> 
			protoerrhandler proxynextclientmessage $
				client notallowed

	proxyclientmessage Nothing = proxydone
	proxyclientmessage (Just message) = case message of
		CHECKPRESENT _ ->
			proxyresponse message (const proxynextclientmessage)
		LOCKCONTENT _ ->
			proxyresponse message (const proxynextclientmessage)
		UNLOCKCONTENT ->
			proxynoresponse message proxynextclientmessage
		REMOVE k -> 
			servermodechecker checkREMOVEServerMode $
				handleREMOVE k message
		GET _ _ _ -> handleGET message
		PUT _ k ->
			servermodechecker checkPUTServerMode $
				handlePUT k message
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
	proxyresponse message a = getresponse remote message $ \resp ->
		protoerrhandler (a resp) $
			client $ net $ sendMessage resp
	
	-- Send a message to the remote, that it will not respond to.
	proxynoresponse message a =
		protoerrhandler a $
			remote $ net $ sendMessage message
	
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
		
	handleREMOVE k message =
		proxyresponse message $ \resp () -> do
			case resp of
				SUCCESS -> removedContent proxymethods
					remoteuuid k
				_ -> return ()
			proxynextclientmessage ()

	handleGET message = getresponse remote message $ withDATA relayGET

	handlePUT k message = getresponse remote message $ \resp -> case resp of
		ALREADY_HAVE -> protoerrhandler proxynextclientmessage $
			client $ net $ sendMessage resp
		PUT_FROM _ -> 
			getresponse client resp $ withDATA (relayPUT k)
		_ -> protoerr

	withDATA a message@(DATA len) = a len message
	withDATA _ _ = protoerr

	relayGET len = relayDATAStart client $
		relayDATACore len remote client $
			relayDATAFinish remote client $
				relayonemessage client remote $
					const proxynextclientmessage
	
	relayPUT k len = relayDATAStart remote $
		relayDATACore len client remote $
			relayDATAFinish client remote $
				relayonemessage remote client finished
	  where
		finished resp () = do
			case resp of
				SUCCESS -> addedContent proxymethods remoteuuid k
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
	
	relayDATAFinish x y sendsuccessfailure () = protocolversion >>= \case
		ProtocolVersion 0 -> sendsuccessfailure
		-- Protocol version 1 has a VALID or
		-- INVALID message after the data.
		_ -> relayonemessage x y (\_ () -> sendsuccessfailure)

