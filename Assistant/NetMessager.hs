{- git-annex assistant out of band network messager interface
 -
 - Copyright 2012-2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE BangPatterns #-}

module Assistant.NetMessager where

import Assistant.Common
import Assistant.Types.NetMessager

import Control.Concurrent.STM
import Control.Concurrent.MSampleVar
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.DList as D

sendNetMessage :: NetMessage -> Assistant ()
sendNetMessage m = 
	(atomically . flip writeTChan m) <<~ (netMessages . netMessager)

waitNetMessage :: Assistant (NetMessage)
waitNetMessage = (atomically . readTChan) <<~ (netMessages . netMessager)

notifyNetMessagerRestart :: Assistant ()
notifyNetMessagerRestart =
	flip writeSV () <<~ (netMessagerRestart . netMessager)

{- This can be used to get an early indication if the network has
 - changed, to immediately restart a connection. However, that is not
 - available on all systems, so clients also need to deal with
 - restarting dropped connections in the usual way. -}
waitNetMessagerRestart :: Assistant ()
waitNetMessagerRestart = readSV <<~ (netMessagerRestart . netMessager)

{- Store a new important NetMessage for a client, and if an equivilant
 - older message is already stored, remove it from both importantNetMessages
 - and sentImportantNetMessages. -}
storeImportantNetMessage :: NetMessage -> ClientID -> (ClientID -> Bool) -> Assistant ()
storeImportantNetMessage m client matchingclient = go <<~ netMessager
  where
	go nm = atomically $ do
		q <- takeTMVar $ importantNetMessages nm
		sent <- takeTMVar $ sentImportantNetMessages nm
		putTMVar (importantNetMessages nm) $
			M.alter (Just . maybe (S.singleton m) (S.insert m)) client $
				M.mapWithKey removematching q
		putTMVar (sentImportantNetMessages nm) $
			M.mapWithKey removematching sent
	removematching someclient s
		| matchingclient someclient = S.filter (not . equivilantImportantNetMessages m) s
		| otherwise = s

{- Indicates that an important NetMessage has been sent to a client. -}
sentImportantNetMessage :: NetMessage -> ClientID -> Assistant ()
sentImportantNetMessage m client = go <<~ (sentImportantNetMessages . netMessager)
  where
	go v = atomically $ do
		sent <- takeTMVar v
		putTMVar v $
			M.alter (Just . maybe (S.singleton m) (S.insert m)) client sent

{- Checks for important NetMessages that have been stored for a client, and
 - sent to a client. Typically the same client for both, although 
 - a modified or more specific client may need to be used. -}
checkImportantNetMessages :: (ClientID, ClientID) -> Assistant (S.Set NetMessage, S.Set NetMessage)
checkImportantNetMessages (storedclient, sentclient) = go <<~ netMessager
  where
	go nm = atomically $ do
		stored <- M.lookup storedclient <$> (readTMVar $ importantNetMessages nm)
		sent <- M.lookup sentclient <$> (readTMVar $ sentImportantNetMessages nm)
		return (fromMaybe S.empty stored, fromMaybe S.empty sent)

{- Queues a push initiation message in the queue for the appropriate
 - side of the push but only if there is not already an initiation message
 - from the same client in the queue. -}
queuePushInitiation :: NetMessage -> Assistant ()
queuePushInitiation msg@(Pushing clientid stage) = do
	tv <- getPushInitiationQueue side
  	liftIO $ atomically $ do
		r <- tryTakeTMVar tv
		case r of
			Nothing -> putTMVar tv [msg]
			Just l -> do
				let !l' = msg : filter differentclient l
				putTMVar tv l'
  where
  	side = pushDestinationSide stage
	differentclient (Pushing cid _) = cid /= clientid
	differentclient _ = True
queuePushInitiation _ = noop

{- Waits for a push inititation message to be received, and runs
 - function to select a message from the queue. -}
waitPushInitiation :: PushSide -> ([NetMessage] -> (NetMessage, [NetMessage])) -> Assistant NetMessage
waitPushInitiation side selector = do
	tv <- getPushInitiationQueue side
	liftIO $ atomically $ do
		q <- takeTMVar tv
		if null q
			then retry
			else do
				let (msg, !q') = selector q
				unless (null q') $
					putTMVar tv q'
				return msg

{- Stores messages for a push into the appropriate inbox.
 -
 - To avoid overflow, only 1000 messages max are stored in any
 - inbox, which should be far more than necessary.
 -
 - TODO: If we have more than 100 inboxes for different clients,
 - discard old ones that are not currently being used by any push.
 -}
storeInbox :: NetMessage -> Assistant ()
storeInbox msg@(Pushing clientid stage) = do
	inboxes <- getInboxes side
	stored <- liftIO $ atomically $ do
		m <- readTVar inboxes
		let update = \v -> do
			writeTVar inboxes $
				M.insertWith' const clientid v m
			return True
		case M.lookup clientid m of
			Nothing -> update (1, tostore)
			Just (sz, l)
				| sz > 1000 -> return False
				| otherwise ->
					let !sz' = sz + 1
					    !l' = D.append l tostore
					in update (sz', l')
	if stored
		then netMessagerDebug clientid ["stored", logNetMessage msg, "in", show side, "inbox"]
		else netMessagerDebug clientid ["discarded", logNetMessage msg, "; ", show side, "inbox is full"]
  where
	side = pushDestinationSide stage
	tostore = D.singleton msg
storeInbox _ = noop

{- Gets the new message for a push from its inbox.
 - Blocks until a message has been received. -}
waitInbox :: ClientID -> PushSide -> Assistant (NetMessage)
waitInbox clientid side = do
	inboxes <- getInboxes side
	liftIO $ atomically $ do
		m <- readTVar inboxes
		case M.lookup clientid m of
			Nothing -> retry
			Just (sz, dl)
				| sz < 1 -> retry
				| otherwise -> do
					let msg = D.head dl
					let dl' = D.tail dl
					let !sz' = sz - 1
					writeTVar inboxes $
						M.insertWith' const clientid (sz', dl') m
					return msg

emptyInbox :: ClientID -> PushSide -> Assistant ()
emptyInbox clientid side = do
	inboxes <- getInboxes side
	liftIO $ atomically $
		modifyTVar' inboxes $
			M.delete clientid
	
getInboxes :: PushSide -> Assistant Inboxes
getInboxes side =
	getSide side . netMessagerInboxes <$> getAssistant netMessager

getPushInitiationQueue :: PushSide -> Assistant (TMVar [NetMessage])
getPushInitiationQueue side =
	getSide side . netMessagerPushInitiations <$> getAssistant netMessager

netMessagerDebug :: ClientID -> [String] -> Assistant ()
netMessagerDebug clientid l = debug $
	"NetMessager" : l ++ [show $ logClientID clientid]
