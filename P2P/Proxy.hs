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

{- This is the first thing run when proxying with a client. Most clients
 - will send a VERSION message, although version 0 clients will not and
 - will send some other message.
 -
 - But before the client will send VERSION, it needs to see AUTH_SUCCESS.
 - So send that, although the connection with the remote is not actually
 - brought up yet.
 -}
getClientProtocolVersion 
	:: (forall t. ((t -> Annex r) -> Annex (Either ProtoFailure t) -> Annex r))
	-> Remote 
	-> ClientSide
	-> (Maybe (ProtocolVersion, Maybe Message) -> Annex r)
	-> Annex r
getClientProtocolVersion clienterrhandler remote (ClientSide clientrunst clientconn) cont =
	clienterrhandler cont $
		liftIO $ runNetProto clientrunst clientconn $
			getClientProtocolVersion' remote

getClientProtocolVersion' :: Remote -> Proto (Maybe (ProtocolVersion, Maybe Message))
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
 - getClientProtocolVersion, and after the connection to
 - the remote has been made, and the protocol version negotiated with the
 - remote.
 -}
proxy 
	:: (forall t. ((t -> Annex r) -> Annex (Either ProtoFailure t) -> Annex r))
	-> Annex r
	-> ServerMode
	-> ClientSide
	-> RemoteSide
	-> Maybe Message
	-- ^ non-VERSION message that was received from the client and has
	-- not been responded to yet
	-> Annex r
proxy clienterrhandler endsuccess servermode (ClientSide clientrunst clientconn) (RemoteSide remoterunst remoteconn) othermessage = do
	case othermessage of
		Just message -> clientmessage (Just message)
		Nothing -> do
			-- Send client the VERSION from the remote.
			proxyprotocolversion <- 
				either (const defaultProtocolVersion) id
					<$> toremote (net getProtocolVersion)
			clienterrhandler (\() -> getnextclientmessage) $ 
				toclient $ net $ sendMessage 
					(VERSION proxyprotocolversion)
  where
	toremote = liftIO . runNetProto remoterunst remoteconn
	toclient = liftIO . runNetProto clientrunst clientconn

	getnextclientmessage = clienterrhandler clientmessage $
		toclient (net receiveMessage)

	clientmessage Nothing = endsuccess
	clientmessage (Just message) = giveup "TODO" -- XXX
