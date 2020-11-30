{- External remote protocol async extension.
 - 
 - Copyright 2020 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}

module Remote.External.AsyncExtension (runRelayToExternalAsync) where

import Common
import Messages
import Remote.External.Types
import Utility.SimpleProtocol as Proto

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TBMChan
import qualified Data.Map.Strict as M

-- | Starts a thread that will handle all communication with the external
-- process. The input ExternalState communicates directly with the external
-- process.
runRelayToExternalAsync :: External -> ExternalState -> IO ExternalAsyncRelay
runRelayToExternalAsync external st = do
	jidmap <- newTVarIO M.empty
	sendq <- newSendQueue
	nextjid <- newTVarIO (JobId 1)
	sender <- async $ sendloop st sendq
	receiver <- async $ receiveloop external st jidmap sendq sender
	return $ ExternalAsyncRelay $ do
		receiveq <- newReceiveQueue
		jid <- atomically $ do
			jid@(JobId n) <- readTVar nextjid
			let !jid' = JobId (succ n)
			writeTVar nextjid jid'
			modifyTVar' jidmap $ M.insert jid receiveq
			return jid
		return $ ExternalState
			{ externalSend = \msg -> 
				atomically $ writeTBMChan sendq
					(toAsyncWrapped msg, jid)
			, externalReceive = atomically (readTBMChan receiveq)
			-- This shuts down the whole relay.
			, externalShutdown = shutdown external st sendq sender receiver
			-- These three TMVars are shared amoung all
			-- ExternalStates that use this relay; they're
			-- common state about the external process.
			, externalPrepared = externalPrepared st
			, externalConfig = externalConfig st
			, externalConfigChanges = externalConfigChanges st
			}

type ReceiveQueue = TBMChan String

type SendQueue = TBMChan (AsyncWrapped, JobId)

type JidMap = TVar (M.Map JobId ReceiveQueue)

newReceiveQueue :: IO ReceiveQueue
newReceiveQueue = newTBMChanIO 10

newSendQueue :: IO SendQueue
newSendQueue = newTBMChanIO 10

receiveloop :: External -> ExternalState -> JidMap -> SendQueue -> Async () -> IO ()
receiveloop external st jidmap sendq sendthread = externalReceive st >>= \case
	Just l -> case parseMessage l :: Maybe AsyncMessage of
		Just (AsyncMessage jid msg) ->
			M.lookup jid <$> readTVarIO jidmap >>= \case
				Just c -> do
					atomically $ writeTBMChan c msg
					receiveloop external st jidmap sendq sendthread
				Nothing -> protoerr "unknown job number"
		Nothing -> case parseMessage l :: Maybe ExceptionalMessage of
			Just _ -> do
				-- ERROR is relayed to all listeners
				m <- readTVarIO jidmap
				forM_ (M.elems m) $ \c ->
					atomically  $ writeTBMChan c l
				receiveloop external st jidmap sendq sendthread
			Nothing -> protoerr "unexpected non-async message"
	Nothing -> closeandshutdown
  where
	protoerr s = do
		warningIO $ "async external special remote protocol error: " ++ s
		closeandshutdown
	
	closeandshutdown = do
		dummy <- async noop
		shutdown external st sendq sendthread dummy True
		m <- atomically $ readTVar jidmap
		forM_ (M.elems m) (atomically . closeTBMChan)

sendloop :: ExternalState -> SendQueue -> IO ()
sendloop st sendq = atomically (readTBMChan sendq) >>= \case
	Just (wrappedmsg, jid) -> do
		case wrappedmsg of
			AsyncWrappedRemoteResponse msg ->
				externalSend st $ wrapjid msg jid
			AsyncWrappedRequest msg ->
				externalSend st $ wrapjid msg jid
			AsyncWrappedExceptionalMessage msg -> 
				externalSend st msg
			AsyncWrappedAsyncMessage msg ->
				externalSend st msg
		sendloop st sendq
	Nothing -> return ()
  where
	wrapjid msg jid = AsyncMessage jid $ unwords $ Proto.formatMessage msg

shutdown :: External -> ExternalState -> SendQueue -> Async () -> Async () -> Bool -> IO ()
shutdown external st sendq sendthread receivethread b = do
	-- Receive thread is normally blocked reading from a handle.
	-- That can block closing the handle, so it needs to be canceled.
	cancel receivethread
	-- Cleanly shutdown the send thread as well, allowing it to finish
	-- writing anything that was buffered.
	atomically $ closeTBMChan sendq
	wait sendthread
	r <- atomically $ do
		r <- tryTakeTMVar (externalAsync external) 
		putTMVar (externalAsync external)
			UncheckedExternalAsync
		return r
	case r of
		Just (ExternalAsync _) -> externalShutdown st b
		_ -> noop
