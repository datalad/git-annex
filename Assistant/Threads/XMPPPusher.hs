{- git-annex XMPP pusher threads
 -
 - This is a pair of threads. One handles git send-pack,
 - and the other git receive-pack. Each thread can be running at most
 - one such operation at a time.
 -
 - Why not use a single thread? Consider two clients A and B.
 - If both decide to run a receive-pack at the same time to the other,
 - they would deadlock with only one thread. For larger numbers of
 - clients, the two threads are also sufficient.
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.Threads.XMPPPusher where

import Assistant.Common
import Assistant.NetMessager
import Assistant.Types.NetMessager
import Assistant.WebApp (UrlRenderer)
import Assistant.WebApp.Configurators.XMPP (checkCloudRepos)
import Assistant.XMPP.Git

import Control.Exception as E

xmppSendPackThread :: UrlRenderer -> NamedThread
xmppSendPackThread = pusherThread "XMPPSendPack" SendPack

xmppReceivePackThread :: UrlRenderer -> NamedThread
xmppReceivePackThread = pusherThread "XMPPReceivePack" ReceivePack

pusherThread :: String -> PushSide -> UrlRenderer -> NamedThread
pusherThread threadname side urlrenderer = namedThread threadname $ go Nothing
  where
  	go lastpushedto = do
		msg <- waitPushInitiation side $ selectNextPush lastpushedto
		debug ["started running push", logNetMessage msg]

		runpush <- asIO $ runPush checker msg
		r <- liftIO (E.try runpush :: IO (Either SomeException (Maybe ClientID)))
		let successful = case r of
			Right (Just _) -> True
			_ -> False

		{- Empty the inbox, because stuff may have
		 - been left in it if the push failed. -}
		let justpushedto = getclient msg
		maybe noop (`emptyInbox` side) justpushedto

		debug ["finished running push", logNetMessage msg, show successful]
		go $ if successful then justpushedto else lastpushedto
	
	checker = checkCloudRepos urlrenderer

	getclient (Pushing cid _) = Just cid
	getclient _ = Nothing

{- Select the next push to run from the queue.
 - The queue cannot be empty!
 -
 - We prefer to select the most recently added push, because its requestor
 - is more likely to still be connected.
 -
 - When passed the ID of a client we just pushed to, we prefer to not
 - immediately push again to that same client. This avoids one client 
 - drowing out others. So pushes from the client we just pushed to are 
 - relocated to the beginning of the list, to be processed later.
 -}
selectNextPush :: Maybe ClientID -> [NetMessage] -> (NetMessage, [NetMessage])
selectNextPush _ (m:[]) = (m, []) -- common case
selectNextPush _ [] = error "selectNextPush: empty list"
selectNextPush lastpushedto l = go [] l
  where
	go (r:ejected) [] = (r, ejected)
	go rejected (m:ms) = case m of
		(Pushing clientid _)
			| Just clientid /= lastpushedto -> (m, rejected ++ ms)
		_ -> go (m:rejected) ms
  	go [] [] = undefined
