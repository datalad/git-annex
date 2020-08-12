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

-- | Constructs an ExternalState that can be used to communicate with
-- the external process via the relay.
relayToExternalAsync :: ExternalAsyncRelay -> IO ExternalState
relayToExternalAsync relay = do
	n <- liftIO $ atomically $ do
		v <- readTVar (asyncRelayLastId relay)
		let !n = succ v
		writeTVar (asyncRelayLastId relay) n
		return n
	asyncRelayExternalState relay n

-- | Starts a thread that will handle all communication with the external
-- process. The input ExternalState communicates directly with the external
-- process.
runRelayToExternalAsync :: External -> ExternalState -> IO ExternalAsyncRelay
runRelayToExternalAsync external st = do
	startcomm <- runRelayToExternalAsync' external st
	pv <- atomically $ newTVar 1
	return $ ExternalAsyncRelay
		{ asyncRelayLastId = pv
		, asyncRelayExternalState = relaystate startcomm
		}
  where
	relaystate startcomm n = do
		(sendh, receiveh, shutdownh) <- startcomm (ClientId n)
		return $ ExternalState
			{ externalSend = atomically . writeTBMChan sendh
			, externalReceive = fmap join $ atomically $ readTBMChan receiveh
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
	:: External
	-> ExternalState
	-> IO (ClientId -> IO (TBMChan String, TBMChan (Maybe String), TBMChan Bool))
runRelayToExternalAsync' external st = do
	let startcomm n = error "TODO"
	sendt <- async sendloop
	newreqs <- newTVarIO []
	void $ async (receiveloop newreqs M.empty sendt)
	return startcomm
  where
	receiveloop newreqs jidmap sendt = externalReceive st >>= \case
		Just l -> case parseMessage l :: Maybe AsyncMessage of
			Just (RESULT_ASYNC msg) -> getnext newreqs >>= \case
				Just c -> do
					relayto c msg
					receiveloop newreqs jidmap sendt
				Nothing -> protoerr "unexpected RESULT-ASYNC"
			Just (START_ASYNC jid msg) -> getnext newreqs >>= \case
				Just c -> do
					relayto c msg
					let !jidmap' = M.insert jid c jidmap
					receiveloop newreqs jidmap' sendt
				Nothing -> protoerr "unexpected START-ASYNC"
			Just (END_ASYNC jid msg) -> case M.lookup jid jidmap of
				Just c -> do
					relayto c msg
					closerelayto c
					let !jidmap' = M.delete jid jidmap
					receiveloop newreqs jidmap' sendt
				Nothing -> protoerr "END-ASYNC with unknown jobid"
			Just (ASYNC jid msg) -> case M.lookup jid jidmap of
				Just c -> do
					relayto c msg
					let !jidmap' = M.delete jid jidmap
					receiveloop newreqs jidmap' sendt
				Nothing -> protoerr "ASYNC with unknown jobid"
			_ -> protoerr "unexpected non-async message"
		Nothing -> do
			-- Unable to receive anything more from the
			-- process, so it's not usable any longer.
			-- So close all chans, stop the process,
			-- and avoid any new ExternalStates from being
			-- created using it.
			cancel sendt
			atomically $ do
				void $ tryTakeTMVar (externalAsync external) 
				putTMVar (externalAsync external)
					UncheckedExternalAsync
			forM_ (M.elems jidmap) closerelayto
			externalShutdown st True

	sendloop = do
		error "TODO"

	relayto (toq, _fromq) msg = atomically $ writeTBMChan toq msg

	closerelayto (toq, fromq) = do
		atomically $ closeTBMChan toq
		atomically $ closeTBMChan fromq

	getnext l = atomically $ readTVar l >>= \case
		[] -> return Nothing
		(c:rest) -> do
			writeTVar l rest
			return (Just c)
	
	protoerr s = giveup ("async special remote protocol error: " ++ s)

