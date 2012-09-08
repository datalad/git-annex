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

import Network.Multicast
import Network.Socket
import qualified Data.Text as T

thisThread :: ThreadName
thisThread = "PairListener"

pairListenerThread :: ThreadState -> DaemonStatusHandle -> UrlRenderer -> NamedThread
pairListenerThread st dstatus urlrenderer = thread $ withSocketsDo $ do
	sock <- multicastReceiver (multicastAddress $ IPv4Addr undefined) pairingPort
	forever $ do
		msg <- getmsg sock []
		dispatch $ readish msg
	where
		thread = NamedThread thisThread

		getmsg sock c = do
			(msg, n, _) <- recvFrom sock chunksz
			if n < chunksz
				then return $ c ++ msg
				else getmsg sock $ c ++ msg
			where
				chunksz = 1024

		dispatch Nothing = noop
		dispatch (Just (PairReqM r@(PairReq v))) =
			unlessM (mypair v) $
				pairReqAlert dstatus urlrenderer r
		dispatch (Just (PairAckM r@(PairAck v))) =
			unlessM (mypair v) $
				pairAckAlert dstatus r

		{- Filter out our own pair requests, by checking if we
		 - can verify using the secrets of any of them. -}
		mypair v = any (verified v . inProgressSecret) . pairingInProgress
			<$> getDaemonStatus dstatus

{- Pair request alerts from the same host combine,
 - so repeated requests do not add additional alerts. -}
pairReqAlert :: DaemonStatusHandle -> UrlRenderer -> PairReq -> IO ()
pairReqAlert dstatus urlrenderer r@(PairReq v) = do
	let pairdata = verifiableVal v
	let repo = remoteUserName pairdata ++ "@" ++
		fromMaybe (showAddr $ remoteAddress pairdata)
			(remoteHostName pairdata) ++
			(remoteDirectory pairdata)
	let msg = repo ++ " is sending a pair request."
	url <- renderUrl urlrenderer (FinishPairR r) []
	void $ addAlert dstatus $ pairRequestAlert repo msg $
		AlertButton
			{ buttonUrl = url
			, buttonLabel = T.pack "Respond"
			}

pairAckAlert :: DaemonStatusHandle -> PairAck -> IO ()
pairAckAlert dstatus r@(PairAck v) = error "TODO"
