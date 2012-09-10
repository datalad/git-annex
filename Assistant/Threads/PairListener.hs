{- git-annex assistant thread to listen for incoming pairing traffic
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.Threads.PairListener where

import Assistant.Common
import Assistant.Pairing
import Assistant.Pairing.Network
import Assistant.ThreadedMonad
import Assistant.DaemonStatus
import Assistant.WebApp
import Assistant.WebApp.Types
import Assistant.Alert
import Utility.Verifiable
import Utility.Tense

import Network.Multicast
import Network.Socket
import qualified Data.Text as T

thisThread :: ThreadName
thisThread = "PairListener"

pairListenerThread :: ThreadState -> DaemonStatusHandle -> UrlRenderer -> NamedThread
pairListenerThread st dstatus urlrenderer = thread $ withSocketsDo $ do
	sock <- multicastReceiver (multicastAddress $ IPv4Addr undefined) pairingPort
	go sock
	where
		thread = NamedThread thisThread
		
		go sock = do
			msg <- getmsg sock []
			dispatch $ readish msg
			go sock

		getmsg sock c = do
			(msg, n, _) <- recvFrom sock chunksz
			if n < chunksz
				then return $ c ++ msg
				else getmsg sock $ c ++ msg
			where
				chunksz = 1024

		dispatch Nothing = noop
		dispatch (Just m@(PairMsg v)) = do
			verified <- maybe False (verify v . inProgressSecret)
				. pairingInProgress
				<$> getDaemonStatus dstatus
			case pairMsgStage m of
				PairReq -> pairReqReceived verified dstatus urlrenderer m
				PairAck -> pairAckReceived verified dstatus m
				PairDone -> pairDoneReceived verified dstatus m

{- Pair request alerts from the same host combine,
 - so repeated requests do not add additional alerts. -}
pairReqReceived :: Bool -> DaemonStatusHandle -> UrlRenderer -> PairMsg -> IO ()
pairReqReceived True _ _ _ = noop -- ignore out own PairReq
pairReqReceived False dstatus urlrenderer msg = do
	url <- renderUrl urlrenderer (FinishPairR msg) []
	void $ addAlert dstatus $ pairRequestReceivedAlert repo
		(repo ++ " is sending a pair request.") $
		AlertButton
			{ buttonUrl = url
			, buttonLabel = T.pack "Respond"
			, buttonAction = Just onclick
			}
	where
		v = fromPairMsg msg
		(_, pairdata) = verifiableVal v
		repo = concat
			[ remoteUserName pairdata
			, "@"
			, fromMaybe (showAddr $ remoteAddress pairdata)
				(remoteHostName pairdata)
			, ":"
			, (remoteDirectory pairdata)
			]
		{- Remove the button when it's clicked, and change the
		 - alert to be in progress. This alert cannot be entirely
		 - removed since more pair request messages are coming in
		 - and would re-add it. -}
		onclick i = updateAlert dstatus i $ \alert -> Just $ alert
			{ alertButton = Nothing
			, alertClass = Activity
			, alertIcon = Just ActivityIcon
			, alertData = [UnTensed $ T.pack $ "pair request with " ++ repo ++ " in progress"]
			}

{- When a valid PairAck is seen, a host has successfully paired with
 - us, and we should finish pairing with them. Then send a single PairDone.
 -
 - A stale PairAck might also be seen, after we've finished pairing.
 - Perhaps our PairDone was not received. To handle this, we keep
 - a list of recently finished pairings, and re-send PairDone in
 - response to stale PairAcks for them.
 -}
pairAckReceived :: Bool -> DaemonStatusHandle -> PairMsg -> IO ()
pairAckReceived False _ _ = noop -- not verified
pairAckReceived True dstatus msg = error "TODO"

{- If we get a valid PairDone, and are sending PairAcks, we can stop
 - sending them, as the message has been received.
 -
 - Also, now is the time to remove the pair request alert, as pairing is
 - over. Do that even if the PairDone cannot be validated, as we might
 - be a third host that did not participate in the pairing.
 - Note: This does allow a bad actor to squelch pairing on a network
 - by sending bogus PairDones.
 -}
pairDoneReceived :: Bool -> DaemonStatusHandle -> PairMsg -> IO ()
pairDoneReceived False _ _ = noop -- not verified
pairDoneReceived True dstatus msg = error "TODO"
