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

{- Runs an action that runs either the send or receive side of a push.
 -
 - While the push is running, netMessagesPush will get messages put into it
 - relating to this push, while any messages relating to other pushes
 - on the same side go to netMessagesDeferred. Once the push finishes,
 - those deferred messages will be fed to handledeferred for processing.
 -}
runPush :: PushSide -> ClientID -> (NetMessage -> Assistant ()) -> Assistant a -> Assistant a
runPush side clientid handledeferred a = do
	nm <- getAssistant netMessager
	let runningv = getSide side $ netMessagerPushRunning nm
	let setup = void $ atomically $ swapTMVar runningv $ Just clientid
	let cleanup = atomically $ do
		void $ swapTMVar runningv Nothing
		emptytchan (getSide side $ netMessagesPush nm)
	r <- E.bracket_ setup cleanup <~> a
	(void . forkIO) <~> processdeferred nm
	return r
  where
	emptytchan c = maybe noop (const $ emptytchan c) =<< tryReadTChan c
	processdeferred nm = do
		s <- liftIO $ atomically $ swapTMVar (getSide side $ netMessagesPushDeferred nm) S.empty
		mapM_ rundeferred (S.toList s)
	rundeferred m = (void . (E.try :: (IO () -> IO (Either SomeException ()))))
		<~> handledeferred m

{- While a push is running, matching push messages are put into
 - netMessagesPush, while others that involve the same side go to
 - netMessagesDeferredPush.
 -
 - When no push is running involving the same side, returns False.
 -
 - To avoid bloating memory, only messages that initiate pushes are
 - deferred.
 -}
queueNetPushMessage :: NetMessage -> Assistant Bool
queueNetPushMessage m@(Pushing clientid stage) = do
	nm <- getAssistant netMessager
	liftIO $ atomically $ do
		v <- readTMVar (getSide side $ netMessagerPushRunning nm)
		case v of
			Nothing -> return False
			(Just runningclientid)
				| runningclientid == clientid -> queue nm
				| isPushInitiation stage -> defer nm
				| otherwise -> discard
  where
	side = pushDestinationSide stage
	queue nm = do
		writeTChan (getSide side $ netMessagesPush nm) m
		return True
	defer nm = do
		let mv = getSide side $ netMessagesPushDeferred nm
		s <- takeTMVar mv
		putTMVar mv $ S.insert m s
		return True
	discard = return True
queueNetPushMessage _ = return False

waitNetPushMessage :: PushSide -> Assistant (NetMessage)
waitNetPushMessage side = (atomically . readTChan)
	<<~ (getSide side . netMessagesPush . netMessager)

