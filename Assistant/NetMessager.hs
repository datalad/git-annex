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
 -
 - While the push is running, netMessagesPush will get messages put into it
 - relating to this push, while any messages relating to other pushes
 - on the same side go to netMessagesDeferred. Once the push finishes,
 - those deferred messages will be fed to handledeferred for processing.
 -
 - If this is called when a push of the same side is running, it will
 - block until that push completes, and then run.
 -}
runPush :: PushSide -> ClientID -> Assistant a -> Assistant a
runPush side clientid a = do
	nm <- getAssistant netMessager
	let v = getSide side $ netMessagerPushRunning nm
	debugmsg <- asIO1 $ \s -> netMessagerDebug clientid [s, show side]
	let setup = do
		debugmsg "preparing to run"
		atomically $ ifM (isNothing <$> tryReadTMVar v)
			( putTMVar v clientid
			, retry
			)
		debugmsg "started running"
	let cleanup = do
		debugmsg "finished running"
		atomically $ takeTMVar v
	r <- E.bracket_ setup cleanup <~> a
	{- Empty the inbox, because stuff may have been left in it
	 - if the push failed. -}
	emptyInbox clientid side
	return r

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
