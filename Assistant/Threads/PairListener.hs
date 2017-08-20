{- git-annex assistant thread to listen for incoming pairing traffic
 -
 - Copyright 2012 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.Threads.PairListener where

import Assistant.Common
import Assistant.Pairing
import Assistant.Pairing.Network
import Assistant.Pairing.MakeRemote
import Assistant.WebApp (UrlRenderer)
import Assistant.WebApp.Types
import Assistant.Alert
import Assistant.DaemonStatus
import Utility.ThreadScheduler
import Git

import Network.Multicast
import Network.Socket
import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as BU8
import qualified Network.Socket.ByteString as B
import qualified Data.Text as T

pairListenerThread :: UrlRenderer -> NamedThread
pairListenerThread urlrenderer = namedThread "PairListener" $ do
	listener <- asIO1 $ go [] []
	liftIO $ withSocketsDo $
		runEvery (Seconds 60) $ void $ tryIO $ 
			listener =<< getsock
  where
	{- Note this can crash if there's no network interface,
	 - or only one like lo that doesn't support multicast. -}
	getsock = multicastReceiver (multicastAddress IPv4AddrClass) pairingPort
		
	go reqs cache sock = liftIO (getmsg sock B.empty) >>= \msg -> case readish (BU8.toString msg) of
		Nothing -> go reqs cache sock
		Just m -> do
			debug ["received", show msg]
			(pip, verified) <- verificationCheck m
				=<< (pairingInProgress <$> getDaemonStatus)
			let wrongstage = maybe False (\p -> pairMsgStage m <= inProgressPairStage p) pip
			let fromus = maybe False (\p -> remoteSshPubKey (pairMsgData m) == remoteSshPubKey (inProgressPairData p)) pip
			case (wrongstage, fromus, checkSane (pairMsgData m), pairMsgStage m) of
				(_, True, _, _) -> do
					debug ["ignoring message that looped back"]
					go reqs cache sock
				(_, _, False, _) -> do
					liftAnnex $ warning $
						"illegal control characters in pairing message; ignoring (" ++ show (pairMsgData m) ++ ")"
					go reqs cache sock
				-- PairReq starts a pairing process, so a
				-- new one is always heeded, even if
				-- some other pairing is in process.
				(_, _, _, PairReq) -> if m `elem` reqs
					then go reqs (invalidateCache m cache) sock
					else do
						pairReqReceived verified urlrenderer m
						go (m:take 10 reqs) (invalidateCache m cache) sock
				(True, _, _, _) -> do
					debug
						["ignoring out of order message"
						, show (pairMsgStage m)
						, "expected"
						, show (succ . inProgressPairStage <$> pip)
						]
					go reqs cache sock
				(_, _, _, PairAck) -> do
					cache' <- pairAckReceived verified pip m cache
					go reqs cache' sock
				(_,_ , _, PairDone) -> do
					pairDoneReceived verified pip m
					go reqs cache sock

	{- As well as verifying the message using the shared secret,
	 - check its UUID against the UUID we have stored. If
	 - they're the same, someone is sending bogus messages,
	 - which could be an attempt to brute force the shared secret. -}
	verificationCheck _ Nothing = return (Nothing, False)
	verificationCheck m (Just pip)
		| not verified && sameuuid = do
			liftAnnex $ warning
				"detected possible pairing brute force attempt; disabled pairing"
			stopSending pip
			return (Nothing, False)
		| otherwise = return (Just pip, verified && sameuuid)
	  where
		verified = verifiedPairMsg m pip
		sameuuid = pairUUID (inProgressPairData pip) == pairUUID (pairMsgData m)

	{- PairReqs invalidate the cache of recently finished pairings.
	 - This is so that, if a new pairing is started with the
	 - same secret used before, a bogus PairDone is not sent. -}
	invalidateCache msg = filter (not . verifiedPairMsg msg)

	getmsg sock c = do
		(msg, _) <- B.recvFrom sock chunksz
		if B.length msg < chunksz
			then return $ c <> msg
			else getmsg sock $ c <> msg
	  where
		chunksz = 1024

{- Show an alert when a PairReq is seen. -}
pairReqReceived :: Bool -> UrlRenderer -> PairMsg -> Assistant ()
pairReqReceived True _ _ = noop -- ignore our own PairReq
pairReqReceived False urlrenderer msg = do
	button <- mkAlertButton True (T.pack "Respond") urlrenderer (FinishLocalPairR msg)
	void $ addAlert $ pairRequestReceivedAlert repo button
  where
	repo = pairRepo msg

{- When a verified PairAck is seen, a host is ready to pair with us, and has
 - already configured our ssh key. Stop sending PairReqs, finish the pairing,
 - and send a single PairDone. -}
pairAckReceived :: Bool -> Maybe PairingInProgress -> PairMsg -> [PairingInProgress] -> Assistant [PairingInProgress]
pairAckReceived True (Just pip) msg cache = do
	stopSending pip
	repodir <- repoPath <$> liftAnnex gitRepo
	liftIO $ setupAuthorizedKeys msg repodir
	finishedLocalPairing msg (inProgressSshKeyPair pip)
	startSending pip PairDone $ multicastPairMsg
		(Just 1) (inProgressSecret pip) (inProgressPairData pip)
	return $ pip : take 10 cache
{- A stale PairAck might also be seen, after we've finished pairing.
 - Perhaps our PairDone was not received. To handle this, we keep
 - a cache of recently finished pairings, and re-send PairDone in
 - response to stale PairAcks for them. -}
pairAckReceived _ _ msg cache = do
	let pips = filter (verifiedPairMsg msg) cache
	unless (null pips) $
		forM_ pips $ \pip ->
			startSending pip PairDone $ multicastPairMsg
				(Just 1) (inProgressSecret pip) (inProgressPairData pip)
	return cache

{- If we get a verified PairDone, the host has accepted our PairAck, and
 - has paired with us. Stop sending PairAcks, and finish pairing with them.
 -
 - TODO: Should third-party hosts remove their pair request alert when they
 - see a PairDone?
 - Complication: The user could have already clicked on the alert and be
 - entering the secret. Would be better to start a fresh pair request in this
 - situation.
 -}
pairDoneReceived :: Bool -> Maybe PairingInProgress -> PairMsg -> Assistant ()
pairDoneReceived False _ _ = noop -- not verified
pairDoneReceived True Nothing _ = noop -- not in progress
pairDoneReceived True (Just pip) msg = do
	stopSending pip
	finishedLocalPairing msg (inProgressSshKeyPair pip)
