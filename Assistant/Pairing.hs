{- git-annex assistant repo pairing
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.Pairing where

import Common
import Utility.Verifiable
import Utility.ThreadScheduler

import Network.Multicast
import Network.Info
import Network.Socket
import Control.Concurrent
import Control.Exception (bracket)
import qualified Data.Map as M

{- "I'll pair with anybody who shares the secret that can be used to verify
 - this request." -}
data PairReq = PairReq (Verifiable PairData)
	deriving (Eq, Read, Show)

{- "I've verified your request, and you can verify mine to see that I know
 - the secret. I set up your ssh key already. Here's mine for you to set up." -}
data PairAck = PairAck (Verifiable PairData)
	deriving (Eq, Read, Show)

fromPairReq :: PairReq -> Verifiable PairData
fromPairReq (PairReq v) = v

fromPairAck :: PairAck -> Verifiable PairData
fromPairAck (PairAck v) = v

data PairMsg
	= PairReqM PairReq
	| PairAckM PairAck
	deriving (Eq, Read, Show)

data PairData = PairData
	-- uname -n output, not a full domain name
	{ remoteHostName :: Maybe HostName
	, remoteAddress :: SomeAddr
	, remoteUserName :: UserName
	, sshPubKey :: SshPubKey
	}
	deriving (Eq, Read, Show)

type SshPubKey = String
type UserName = String

{- A pairing that is in progress has a secret, and a thread that is
 - broadcasting pairing requests. -}
data PairingInProgress = PairingInProgress Secret ThreadId

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

{- Multicasts a message repeatedly on all interfaces until its thread
 - is killed, with a 2 second delay between each transmission.
 -
 - The remoteHostAddress is set to the interface's IP address.
 -
 - Note that new sockets are opened each time. This is hardly efficient,
 - but it allows new network interfaces to be used as they come up.
 - On the other hand, the expensive DNS lookups are cached.
 -}
multicastPairMsg :: (SomeAddr -> PairMsg) -> IO ThreadId
multicastPairMsg mkmsg = forkIO $ go M.empty
	where
		go cache = do
			addrs <- activeNetworkAddresses
			let cache' = updatecache cache addrs
			mapM_ (sendinterface cache') addrs
			threadDelaySeconds (Seconds 2)
			go cache'
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

{- Finds the best hostname to use for the host that sent the PairData.
 -
 - If remoteHostName is set, tries to use a .local address based on it.
 - That's the most robust, if this system supports .local.
 - Otherwise, looks up the hostname in the DNS for the remoteAddress,
 - if any. May fall back to remoteAddress if there's no DNS. Ugh. -}
bestHostName :: PairData -> IO HostName
bestHostName d = case remoteHostName d of
	Just h -> do
		let localname = h ++ ".local"
		addrs <- catchDefaultIO (getAddrInfo Nothing (Just localname) Nothing) []
		maybe fallback (const $ return localname) (headMaybe addrs)
	Nothing -> fallback
	where
		fallback = do
			let sockaddr = case remoteAddress d of
				IPv4Addr a -> SockAddrInet (PortNum 0) a
				IPv6Addr a -> SockAddrInet6 (PortNum 0) 0 a 0
			fromMaybe (show $ remoteAddress d) 
				<$> catchDefaultIO (fst <$> getNameInfo [] True False sockaddr) Nothing

data SomeAddr = IPv4Addr HostAddress | IPv6Addr HostAddress6
	deriving (Ord, Eq, Read, Show)

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
