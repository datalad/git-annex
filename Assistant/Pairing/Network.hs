{- git-annex assistant pairing network code
 -
 - All network traffic is sent over multicast UDP. For reliability,
 - each message is repeated until acknowledged. This is done using a
 - thread, that gets stopped before the next message is sent.
 -
 - Copyright 2012 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.Pairing.Network where

import Assistant.Common
import Assistant.Pairing
import Assistant.DaemonStatus
import Utility.ThreadScheduler
import Utility.Verifiable

import Network.Multicast
import Network.Info
import Network.Socket
import qualified Network.Socket.ByteString as B
import qualified Data.ByteString.UTF8 as BU8
import qualified Data.Map as M
import Control.Concurrent

{- This is an arbitrary port in the dynamic port range, that could
 - conceivably be used for some other broadcast messages.
 - If so, hope they ignore the garbage from us; we'll certianly
 - ignore garbage from them. Wild wild west. -}
pairingPort :: PortNumber
pairingPort = 55556

{- Goal: Reach all hosts on the same network segment.
 - Method: Use same address that avahi uses. Other broadcast addresses seem
 - to not be let through some routers. -}
multicastAddress :: AddrClass -> HostName
multicastAddress IPv4AddrClass = "224.0.0.251"
multicastAddress IPv6AddrClass = "ff02::fb"

{- Multicasts a message repeatedly on all interfaces, with a 2 second
 - delay between each transmission. The message is repeated forever
 - unless a number of repeats is specified.
 -
 - The remoteHostAddress is set to the interface's IP address.
 -
 - Note that new sockets are opened each time. This is hardly efficient,
 - but it allows new network interfaces to be used as they come up.
 - On the other hand, the expensive DNS lookups are cached.
 -}
multicastPairMsg :: Maybe Int -> Secret -> PairData -> PairStage -> IO ()
multicastPairMsg repeats secret pairdata stage = go M.empty repeats
  where
	go _ (Just 0) = noop
	go cache n = do
		addrs <- activeNetworkAddresses
		let cache' = updatecache cache addrs
		mapM_ (sendinterface cache') addrs
		threadDelaySeconds (Seconds 2)
		go cache' $ pred <$> n
	{- The multicast library currently chokes on ipv6 addresses. -}
	sendinterface _ (IPv6Addr _) = noop
	sendinterface cache i = void $ tryIO $
		withSocketsDo $ bracket setup cleanup use
	  where
		setup = multicastSender (multicastAddress IPv4AddrClass) pairingPort
		cleanup (sock, _) = close sock -- FIXME does not work
		use (sock, addr) = do
			setInterface sock (showAddr i)
			maybe noop
				(\s -> void $ B.sendTo sock (BU8.fromString s) addr)
				(M.lookup i cache)
	updatecache cache [] = cache
	updatecache cache (i:is)
		| M.member i cache = updatecache cache is
		| otherwise = updatecache (M.insert i (show $ mkmsg i) cache) is
	mkmsg addr = PairMsg $
		mkVerifiable (stage, pairdata, addr) secret

startSending :: PairingInProgress -> PairStage -> (PairStage -> IO ()) -> Assistant ()
startSending pip stage sender = do
	a <- asIO start
	void $ liftIO $ forkIO a
  where
	start = do
		tid <- liftIO myThreadId
		let pip' = pip { inProgressPairStage = stage, inProgressThreadId = Just tid }
		oldpip <- modifyDaemonStatus $
			\s -> (s { pairingInProgress = Just pip' }, pairingInProgress s)
		maybe noop stopold oldpip
		liftIO $ sender stage
	stopold = maybe noop (liftIO . killThread) . inProgressThreadId

stopSending :: PairingInProgress -> Assistant ()
stopSending pip = do
	maybe noop (liftIO . killThread) $ inProgressThreadId pip
	modifyDaemonStatus_ $ \s -> s { pairingInProgress = Nothing }

class ToSomeAddr a where
	toSomeAddr :: a -> SomeAddr

instance ToSomeAddr IPv4 where
	toSomeAddr (IPv4 a) = IPv4Addr a

instance ToSomeAddr IPv6 where
	toSomeAddr (IPv6 o1 o2 o3 o4) = IPv6Addr (o1, o2, o3, o4)

showAddr :: SomeAddr -> HostName
showAddr (IPv4Addr a) = show $ IPv4 a
showAddr (IPv6Addr (o1, o2, o3, o4)) = show $ IPv6 o1 o2 o3 o4

activeNetworkAddresses :: IO [SomeAddr]
activeNetworkAddresses = filter (not . all (`elem` "0.:") . showAddr)
	. concatMap (\ni -> [toSomeAddr $ ipv4 ni, toSomeAddr $ ipv6 ni])
	<$> getNetworkInterfaces

{- A human-visible description of the repository being paired with.
 - Note that the repository's description is not shown to the user, because
 - it could be something like "my repo", which is confusing when pairing
 - with someone else's repo. However, this has the same format as the
 - default decription of a repo. -}
pairRepo :: PairMsg -> String
pairRepo msg = concat
	[ remoteUserName d
	, "@"
	, fromMaybe (showAddr $ pairMsgAddr msg) (remoteHostName d)
	, ":"
	, remoteDirectory d
	]
  where
	d = pairMsgData msg
