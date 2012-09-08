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
import Assistant.Alert
import Utility.Verifiable

import Network.Multicast
import Network.Socket

thisThread :: ThreadName
thisThread = "PairListener"

pairListenerThread :: ThreadState -> DaemonStatusHandle -> NamedThread
pairListenerThread st dstatus = thread $ withSocketsDo $ do
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
		dispatch (Just (PairReqM (PairReq r))) = void $ do
			let pairdata = verifiableVal r
			let repo = remoteUserName pairdata ++ "@" ++
				fromMaybe (showAddr $ remoteAddress pairdata)
					(remoteHostName pairdata)
			let msg = repo ++ " is sending a pair request."
			{- Pair request alerts from the same host combine,
			 - so repeated requests do not add additional alerts. -}
			addAlert dstatus $ pairRequestAlert repo msg
		dispatch (Just (PairAckM _)) = noop -- TODO
