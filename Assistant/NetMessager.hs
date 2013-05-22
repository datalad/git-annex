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
import Control.Exception as E
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

waitNetMessagerRestart :: Assistant ()
waitNetMessagerRestart = readSV <<~ (netMessagerRestart . netMessager)

{- Store an important NetMessage for a client, and if the same message was
 - already sent, remove it from sentImportantNetMessages. -}
storeImportantNetMessage :: NetMessage -> ClientID -> (ClientID -> Bool) -> Assistant ()
storeImportantNetMessage m client matchingclient = go <<~ netMessager
  where
	go nm = atomically $ do
		q <- takeTMVar $ importantNetMessages nm
		sent <- takeTMVar $ sentImportantNetMessages nm
		putTMVar (importantNetMessages nm) $
			M.alter (Just . maybe (S.singleton m) (S.insert m)) client q
		putTMVar (sentImportantNetMessages nm) $
			M.mapWithKey removematching sent
	removematching someclient s
		| matchingclient someclient = S.delete m s
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

{- Runs an action that runs either the send or receive side of a push.
 - Only one such action per side can run at a time. Other pushes, for
 - the same, or other clients, need to wait their turn.
 -
 - Only one push is allowed to wait per client.
 - There is no point in building up more.
 -
 - While the push is running, netMessagesPush will get messages put into it
 - relating to this push, while any messages relating to other pushes
 - on the same side go to netMessagesDeferred. Once the push finishes,
 - those deferred messages will be fed to handledeferred for processing.
 -}
runPush :: PushSide -> ClientID -> Assistant Bool -> Assistant Bool
runPush side clientid a = do
	debugmsg "preparing to run"
	nm <- getAssistant netMessager
	let setup = do
		(canrun, release) <- atomically $ checkcanrun nm
		if canrun
			then atomically $ waittorun nm release
			else return (False, noop)
	let cleanup (_, release) = atomically release
	go <- asIO1 $ \(run, _) ->
		if run
			then do
				debugmsg "started running"
				r <- a
				debugmsg "finished running"
				{- Empty the inbox, because stuff may have
				 - been left in it if the push failed. -}
				emptyInbox clientid side
				return r
			else do
				debugmsg "skipping running"
				return False
	r <- liftIO $ E.bracket setup cleanup go
	return r
  where
	debugmsg s = netMessagerDebug clientid [s, show side]
	-- check that this is one of the two threads allowed
	-- to run at the same time, pushing to the same client
	-- on the same side
  	checkcanrun nm = do
		let v = getSide side $ netMessagerPushThreadCount nm
		m <- readTVar v
		case M.lookup clientid m of
			Just count
				| count > 2 -> return (False, noop)
			_ -> do
				writeTVar v $
					M.insertWith' (+) clientid 1 m
				let release = modifyTVar' v $
					M.insertWith' (-) clientid 1
				return (True, release)
	-- block until this is the only thread performing
	-- a push on this side, to any client
	waittorun nm release = do
		let v = getSide side $ netMessagerPushRunning nm
		ifM (isNothing <$> tryReadTMVar v)
			( do
				putTMVar v clientid
				let release' = do
					void $ takeTMVar v
					release
				return (True, release')
			, retry
			)

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
	getSide side . netMessagesInboxes <$> getAssistant netMessager

netMessagerDebug :: ClientID -> [String] -> Assistant ()
netMessagerDebug clientid l = debug $
	"NetMessager" : l ++ [show $ logClientID clientid]
