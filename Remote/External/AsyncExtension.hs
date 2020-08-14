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
	void $ async $ sendloop st nextjid jidmap sendq
	void $ async $ receiveloop external st jidmap sendq
	return $ ExternalAsyncRelay $ do
		jidv <- newTVarIO Nothing
		receiveq <- newReceiveQueue
		return $ ExternalState
			{ externalSend = \msg -> 
				atomically $ writeTBMChan sendq
					(toAsyncWrapped msg, (jidv, receiveq))
			, externalReceive = atomically (readTBMChan receiveq)
			-- This shuts down the whole relay.
			, externalShutdown = shutdown external st sendq
			-- These three TVars are shared amoung all
			-- ExternalStates that use this relay; they're
			-- common state about the external process.
			-- TODO: ALL code using these in Remote.External
			-- has to be made async-safe.
			, externalPrepared = externalPrepared st
			, externalConfig = externalConfig st
			, externalConfigChanges = externalConfigChanges st
			}

type ReceiveQueue = TBMChan String

type SendQueue = TBMChan (AsyncWrapped, Conn)

type Conn = (TVar (Maybe JobId), ReceiveQueue)

type JidMap = TVar (M.Map JobId Conn)

type NextJid = TVar JobId

newReceiveQueue :: IO ReceiveQueue
newReceiveQueue = newTBMChanIO 10

newSendQueue :: IO SendQueue
newSendQueue = newTBMChanIO 10

receiveloop :: External -> ExternalState -> JidMap -> SendQueue -> IO ()
receiveloop external st jidmap sendq = externalReceive st >>= \case
	Just l -> case parseMessage l :: Maybe AsyncMessage of
		Just (AsyncMessage jid msg) ->
			M.lookup jid <$> readTVarIO jidmap >>= \case
				Just (_jidv, c) -> do
					atomically $ writeTBMChan c msg
					receiveloop external st jidmap sendq
				Nothing -> protoerr "unknown job number"
		_ -> protoerr "unexpected non-async message"
	Nothing -> closeandshutdown
  where
	protoerr s = do
		warningIO $ "async external special remote protocol error: " ++ s
		closeandshutdown
	
	closeandshutdown = do
		shutdown external st sendq True
		m <- atomically $ readTVar jidmap
		forM_ (M.elems m) (atomically . closeTBMChan . snd)

sendloop :: ExternalState -> NextJid -> JidMap -> SendQueue -> IO ()
sendloop st nextjid jidmap sendq = atomically (readTBMChan sendq) >>= \case
	Just (wrappedmsg, conn@(jidv, _)) -> do
		case wrappedmsg of
			AsyncWrappedRequest msg -> do
				jid <- atomically $ do
					jid@(JobId n) <- readTVar nextjid
					let !jid' = JobId (succ n)
					writeTVar nextjid jid'
					writeTVar jidv (Just jid)
					modifyTVar' jidmap $ M.insert jid conn
					return jid
				externalSend st $ wrapjid msg jid
			AsyncWrappedRemoteResponse msg ->
				readTVarIO jidv >>= \case
					Just jid -> externalSend st $ wrapjid msg jid
					Nothing -> error "failed to find jid"
			AsyncWrappedExceptionalMessage msg ->
				externalSend st msg
			AsyncWrappedAsyncMessage msg ->
				externalSend st msg
		sendloop st nextjid jidmap sendq
	Nothing -> return ()
  where
	wrapjid msg jid = AsyncMessage jid $ unwords $ Proto.formatMessage msg

shutdown :: External -> ExternalState -> SendQueue -> Bool -> IO ()
shutdown external st sendq b = do
	r <- atomically $ do
		r <- tryTakeTMVar (externalAsync external) 
		putTMVar (externalAsync external)
			UncheckedExternalAsync
		return r
	case r of
		Just (ExternalAsync _) -> externalShutdown st b
		_ -> noop
	atomically $ closeTBMChan sendq
