{- git-annex assistant pairing network code
 -
 - All network traffic is sent over multicast UDP. For reliability,
 - each message is repeated until acknowledged. This is done using a
 - thread, that gets stopped before the next message is sent.
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
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
import Control.Exception (bracket)
import qualified Data.Map as M
import Control.Concurrent

{- This is an arbitrary port in the dynamic port range, that could
 - conceivably be used for some other broadcast messages.
 - If so, hope they ignore the garbage from us; we'll certianly
 - ignore garbage from them. Wild wild west. -}
pairingPort :: PortNumber
pairingPort = 55556

{- This is the All Hosts multicast group, which should reach all hosts
 - on the same network segment. -}
multicastAddress :: SomeAddr -> HostName
multicastAddress (IPv4Addr _) = "224.0.0.1"
multicastAddress (IPv6Addr _) = "ff02::1"

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
multicastPairMsg :: Maybe Int -> Secret -> PairStage -> PairData -> IO ()
multicastPairMsg repeats secret stage pairdata = go M.empty repeats
	where
		go _ (Just 0) = noop
		go cache n = do
			addrs <- activeNetworkAddresses
			let cache' = updatecache cache addrs
			mapM_ (sendinterface cache') addrs
			threadDelaySeconds (Seconds 2)
			go cache' $ pred <$> n
		sendinterface cache i = void $ catchMaybeIO $
			withSocketsDo $ bracket
				(multicastSender (multicastAddress i) pairingPort)
				(sClose . fst)
				(\(sock, addr) -> do
					setInterface sock (showAddr i)
					maybe noop (\s -> void $ sendTo sock s addr)
						(M.lookup i cache)
				)
		updatecache cache [] = cache
		updatecache cache (i:is)
			| M.member i cache = updatecache cache is
			| otherwise = updatecache (M.insert i (show $ mkmsg i) cache) is
		mkmsg addr = PairMsg $
			mkVerifiable (stage, pairdata, addr) secret

startSending :: DaemonStatusHandle -> PairingInProgress -> IO () -> IO ()
startSending dstatus pip sender = do
	tid <- forkIO sender
	let pip' = pip { inProgressThreadId = Just tid }
	oldpip <- modifyDaemonStatus dstatus $
		\s -> (s { pairingInProgress = Just pip' }, pairingInProgress s)
	maybe noop stopold oldpip
	where
		stopold = maybe noop killThread . inProgressThreadId

stopSending :: DaemonStatusHandle ->  PairingInProgress -> IO ()
stopSending dstatus pip = do
	maybe noop killThread $ inProgressThreadId pip
	modifyDaemonStatus_ dstatus $ \s -> s { pairingInProgress = Nothing }

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
	. concat . map (\ni -> [toSomeAddr $ ipv4 ni, toSomeAddr $ ipv6 ni])
	<$> getNetworkInterfaces
