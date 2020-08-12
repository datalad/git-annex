{- External remote protocol async extension.
 - 
 - Copyright 2013-2020 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE BangPatterns #-}

module Remote.External.AsyncExtension where

import Common
import Remote.External.Types

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TBMChan
import qualified Data.Map.Strict as M

-- | Starts a thread that will handle all communication with the external
-- process. The input ExternalState communicates directly with the external
-- process.
runRelayToExternalAsync :: External -> ExternalState -> IO ExternalAsyncRelay
runRelayToExternalAsync external st = do
	startcomm <- runRelayToExternalAsync' external st
	return $ ExternalAsyncRelay $ do
		(sendh, receiveh, shutdown) <- startcomm
		return $ ExternalState
			{ externalSend = atomically . writeTBMChan sendh
			, externalReceive = fmap join $ atomically $ readTBMChan receiveh
			-- This shuts down the whole relay.
			, externalShutdown = shutdown
			-- These three TVars are shared amoung all
			-- ExternalStates that use this relay; they're
			-- common state about the external process.
			-- TODO: ALL code using these in Remote.External
			-- has to be made async-safe.
			, externalPrepared = externalPrepared st
			, externalConfig = externalConfig st
			, externalConfigChanges = externalConfigChanges st
			}

runRelayToExternalAsync'
	:: External
	-> ExternalState
	-> IO (IO (TBMChan String, TBMChan (Maybe String), Bool -> IO ()))
runRelayToExternalAsync' external st = do
	newreqs <- newTVarIO []
	startedcomms <- newTVarIO []
	let startcomm = do
		toq <- newTBMChanIO 10 
		fromq <- newTBMChanIO 10
		let c = (toq, fromq, shutdown)
		atomically $ do
			l <- readTVar startedcomms
			-- This append is ok because the maximum size
			-- is the number of jobs that git-annex is
			-- configured to use, which is a relatively 
			-- small number.
			writeTVar startedcomms (l ++ [c])
		return c
	void $ async $ sendloop newreqs startedcomms
	void $ async $ receiveloop newreqs M.empty
	return startcomm
  where
	receiveloop newreqs jidmap = externalReceive st >>= \case
		Just l -> case parseMessage l :: Maybe AsyncMessage of
			Just (RESULT_ASYNC msg) -> getnext newreqs >>= \case
				Just c -> do
					relayto c msg
					receiveloop newreqs jidmap
				Nothing -> protoerr "unexpected RESULT-ASYNC"
			Just (START_ASYNC jid msg) -> getnext newreqs >>= \case
				Just c -> do
					relayto c msg
					let !jidmap' = M.insert jid c jidmap
					receiveloop newreqs jidmap'
				Nothing -> protoerr "unexpected START-ASYNC"
			Just (END_ASYNC jid msg) -> case M.lookup jid jidmap of
				Just c -> do
					relayto c msg
					closerelayto c
					let !jidmap' = M.delete jid jidmap
					receiveloop newreqs jidmap'
				Nothing -> protoerr "END-ASYNC with unknown jobid"
			Just (ASYNC jid msg) -> case M.lookup jid jidmap of
				Just c -> do
					relayto c msg
					let !jidmap' = M.delete jid jidmap
					receiveloop newreqs jidmap'
				Nothing -> protoerr "ASYNC with unknown jobid"
			_ -> protoerr "unexpected non-async message"
		Nothing -> do
			-- Unable to receive anything more from the
			-- process, so it's not usable any longer.
			forM_ (M.elems jidmap) closerelayto
			shutdown True

	sendloop newreqs startedcomms = do
		error "TODO"

	relayto (toq, _fromq) msg =
		atomically $ writeTBMChan toq msg

	closerelayto (toq, fromq) = do
		atomically $ closeTBMChan toq
		atomically $ closeTBMChan fromq

	getnext l = atomically $ readTVar l >>= \case
		[] -> return Nothing
		(c:rest) -> do
			writeTVar l rest
			return (Just c)
	
	shutdown b = do
		r <- atomically $ do
			r <- tryTakeTMVar (externalAsync external) 
			putTMVar (externalAsync external)
				UncheckedExternalAsync
			return r
		case r of
			Just (ExternalAsync _) -> externalShutdown st b
			_ -> noop
	
	protoerr s = giveup ("async special remote protocol error: " ++ s)
