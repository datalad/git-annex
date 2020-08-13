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
	mapjid <- newTVarIO M.empty
	commcounter <- newTVarIO 0
	newconns <- newTVarIO []
	sendq <- newSendQueue
	void $ async $ sendloop st newconns mapjid jidmap sendq
	void $ async $ receiveloop external st newconns jidmap mapjid sendq
	return $ ExternalAsyncRelay $ do
		n <- atomically $ do
			n <- readTVar commcounter
			let n' = succ n
			writeTVar commcounter n'
			return n'
		receiveq <- newReceiveQueue
		return $ ExternalState
			{ externalSend = \msg -> 
				atomically $ writeTBMChan sendq
					( toAsyncWrapped msg
					, (n, receiveq)
					)
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

type ConnNum = Integer

type Conn = (ConnNum, ReceiveQueue)

type NewConns = TVar [Conn]

type MapJid = TVar (M.Map ConnNum JobId)

type JidMap = TVar (M.Map JobId Conn)

newReceiveQueue :: IO ReceiveQueue
newReceiveQueue = newTBMChanIO 10

newSendQueue :: IO SendQueue
newSendQueue = newTBMChanIO 10

receiveloop :: External -> ExternalState -> NewConns -> JidMap -> MapJid -> SendQueue -> IO ()
receiveloop external st newconns jidmap mapjid sendq = externalReceive st >>= \case
	Just l -> case parseMessage l :: Maybe AsyncMessage of
		Just (RESULT_ASYNC msg) -> getnext newconns >>= \case
			Just (_n, c) -> do
				relayto c msg
				loop
			Nothing -> protoerr "unexpected RESULT-ASYNC"
		Just (START_ASYNC jid) -> getnext newconns >>= \case
			Just v@(n, _c) -> do
				atomically $ do
					modifyTVar' jidmap $ M.insert jid v
					modifyTVar' mapjid $ M.insert n jid
				loop
			Nothing -> protoerr "unexpected START-ASYNC"
		Just (ASYNC jid msg) -> getjid jid >>= \case
			Just (_n, c) -> do
				relayto c msg
				loop
			Nothing -> protoerr "ASYNC with unknown jobid"
		_ -> protoerr "unexpected non-async message"
	Nothing -> closeandshutdown
  where
	loop = receiveloop external st newconns jidmap mapjid sendq

	relayto q msg = atomically $ writeTBMChan q msg

	closerelayto q = atomically $ closeTBMChan q

	getnext l = atomically $ readTVar l >>= \case
		[] -> return Nothing
		(c:rest) -> do
			writeTVar l rest
			return (Just c)
	
	getjid jid = M.lookup jid <$> readTVarIO jidmap

	protoerr s = do
		warningIO $ "async external special remote protocol error: " ++ s
		closeandshutdown
	
	closeandshutdown = do
		shutdown external st sendq True
		(m, l) <- atomically $ (,) 
			<$> readTVar jidmap
			<*> readTVar newconns
		forM_ (M.elems m ++ l) (closerelayto . snd)

sendloop :: ExternalState -> NewConns -> MapJid -> JidMap -> SendQueue -> IO ()
sendloop st newconns mapjid jidmap sendq = atomically (readTBMChan sendq) >>= \case
	Just (wrappedmsg, c@(n, _)) -> do
		let newconn = atomically $ do
			-- This append is not too expensive,
			-- because the list length is limited
			-- to the maximum number of jobs.
			modifyTVar' newconns (++[c])
			M.lookup n <$> readTVar mapjid >>= \case
				Nothing -> return ()
				Just jid -> do
					modifyTVar' jidmap (M.delete jid)
					modifyTVar' mapjid (M.delete n)
		case wrappedmsg of
			AsyncWrappedRequest msg -> do
				newconn
				externalSend st msg
			AsyncWrappedExceptionalMessage msg -> do
				newconn
				externalSend st msg
			AsyncWrappedRemoteResponse msg ->
				externalSend st =<< wrapremoteresponse msg n
			AsyncWrappedAsyncReply msg ->
				externalSend st msg
		sendloop st newconns mapjid jidmap sendq
	Nothing -> return ()
  where
	wrapremoteresponse msg n =
		M.lookup n <$> readTVarIO mapjid >>= \case
			Just jid -> return $ REPLY_ASYNC jid $
				unwords $ Proto.formatMessage msg
			Nothing -> error "failed to find jobid"

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
