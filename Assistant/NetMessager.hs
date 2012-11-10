{- git-annex assistant out of band network messager interface
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.NetMessager where

import Assistant.Common
import Assistant.Types.NetMessager

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.MSampleVar
import Control.Exception as E
import qualified Data.Set as S

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

getPushRunning :: Assistant PushRunning
getPushRunning =
	(atomically . readTMVar) <<~ (netMessagerPushRunning . netMessager)

{- Runs an action that runs either the send or receive end of a push.
 -
 - While the push is running, netMessagesPush will get messages put into it
 - relating to this push, while any messages relating to other pushes
 - go to netMessagesDeferred. Once the push finishes, those deferred
 - messages will be fed to handledeferred for processing.
 -}
runPush :: PushRunning -> (NetMessage -> Assistant ()) -> Assistant a -> Assistant a
runPush v handledeferred a = do
	nm <- getAssistant netMessager
	let pr = netMessagerPushRunning nm
	let setup = void $ atomically $ swapTMVar pr v
	let cleanup = atomically $ do
		void $ swapTMVar pr NoPushRunning
		emptytchan (netMessagesPush nm)
	r <- E.bracket_ setup cleanup <~> a
	(void . forkIO) <~> processdeferred nm
	return r
  where
	emptytchan c = maybe noop (const $ emptytchan c) =<< tryReadTChan c
	processdeferred nm = do
		s <- liftIO $ atomically $ swapTMVar (netMessagesDeferredPush nm) S.empty
		mapM_ rundeferred (S.toList s)
	rundeferred m = (void . (E.try :: (IO () -> IO (Either SomeException ()))))
		<~> handledeferred m

{- While a push is running, matching push messages are put into
 - netMessagesPush, while others go to netMessagesDeferredPush.
 - To avoid bloating memory, only messages that initiate pushes are
 - deferred.
 -
 - When no push is running, returns False.
 -}
queueNetPushMessage :: NetMessage -> Assistant Bool
queueNetPushMessage m = do
	nm <- getAssistant netMessager
	liftIO $ atomically $ do
		running <- readTMVar (netMessagerPushRunning nm)
		case running of
			NoPushRunning -> return False
			SendPushRunning cid -> go nm cid
			ReceivePushRunning cid -> go nm cid
  where
	go nm cid
		| getClientID m == Just cid = do
			writeTChan (netMessagesPush nm) m
			return True
		| otherwise = do
			when (isPushInitiationMessage m) $
				defer nm
			return True
	defer nm = do
		s <- takeTMVar (netMessagesDeferredPush nm)
		putTMVar (netMessagesDeferredPush nm) $ S.insert m s

waitNetPushMessage :: Assistant (NetMessage)
waitNetPushMessage = (atomically . readTChan) <<~ (netMessagesPush . netMessager)
