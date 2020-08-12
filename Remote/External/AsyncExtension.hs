{- External remote protocol async extension.
 - 
 - Copyright 2013-2020 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE BangPatterns #-}

module Remote.External.AsyncExtension where

import Common.Annex
import Remote.External.Types

import Control.Concurrent.Async
import Control.Concurrent.STM
import Control.Concurrent.STM.TBMChan
import qualified Data.Map.Strict as M

-- | Constructs an ExternalState that can be used to communicate with
-- the external process via the relay.
relayToExternalAsync :: ExternalAsyncRelay -> IO ExternalState
relayToExternalAsync relay = do
	n <- liftIO $ atomically $ do
		v <- readTVar (asyncRelayLast relay)
		let !n = succ v
		writeTVar (asyncRelayLast relay) n
		return n
	return $ asyncRelayExternalState n

-- | Starts a thread that will handle all communication with the external
-- process. The input ExternalState communicates directly with the external
-- process.
runRelayToExternalAsync :: ExternalState -> IO ExternalAsyncRelay
runRelayToExternalAsync st = do
	startcomm <- runRelayToExternalAsync' st
	pv <- atomically $ newTVar 1
	return $ ExternalAsyncRelay
		{ asyncRelayLastId = pv
		, asyncRelayExternalState = relaystate startcomm
		}
  where
	relaystate startcomm n = do
		(sendh, receiveh, shutdownh) <- startcomm n
		ExternalState
			{ externalSend = atomically . writeTBMChan sendh
			, externalReceive = atomically . readTBMChan receiveh
			, externalShutdown = atomically . writeTBMChan shutdownh
			-- These three TVars are shared amoung all
			-- ExternalStates that use this relay; they're
			-- common state about the external process.
			-- TODO: ALL code using these in Remote.External
			-- has to be made async-safe.
			, externalPrepared = externalPrepared st
			, externalConfig = externalConfig st
			, externalConfigChanges = externalConfigChanges st
			}

newtype ClientId = ClientId Int
	deriving (Show, Eq, Ord)

runRelayToExternalAsync'
	:: ExternalState
	-> IO (ClientId -> IO (TBMChan String, TBMChan (Maybe String), TBMChan Bool))
runRelayToExternalAsync' st = do
	let startcomm n = 
	sendt <- async sendloop
	void $ async (receiveloop [] Nothing sendt)
	return startcomm
  where
	receiveloop newreqs currjid sendt = externalReceive st >>= \case
		Just l -> case parseMessage l :: Maybe AsyncMessage of
			Just (START_ASYNC jid) -> case newreqs of
				[] -> giveup "async special remote protocol error: unexpected START-ASYNC"
				(c:newreqs') -> do
					let !receiverjids' = M.insert jid c receiverjids
					receiveloop newreqs' Nothing receiverjids' sendt
			Just (END_ASYNC jid) -> do
				let !receiverjids' = M.delete jid receiverjids
				receiveloop newreqs (Just jid) receiverjids' sendt
			Just (UPDATE_ASYNC jid) ->
				receiveloop newreqs (Just jid) receiverjids sendt
			Nothing -> case currjid of
				Just jid -> 
				-- 
				Nothing -> case newreqs of
					[] -> giveup "async special remote protocol error: unexpected non-async message"
					(c:_) -> do
						case M.lookup c receivers of
							Just c -> atomically $ writeTBMChan c l
							Nothing -> return ()
						receiveloop newreqs Nothing sendt
		Nothing -> do
			-- Unable to receive anything more from the
			-- process, so it's not usable any longer.
			-- So close all chans, stop the process,
			-- and avoid any new ExternalStates from being
			-- created using it.
			atomically $ do
				void $ tryTakeTMVar (externalAsync external) 
				putTMVar (externalAsync external)
					UncheckedExternalAsync
			forM_ (M.toList receivers) $
				atomically . closeTBMChan
			forM_ (M.toList senders) $
				atomically . closeTBMChan
			externalShutdown st True
			cancel sendt

	sendloop = do
