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
import Utility.Network

import Network.Multicast
import Network.Info
import Network.Socket
import Control.Concurrent
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
	{ remoteHostName :: HostName
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
multicastAddress :: HostName
multicastAddress = "224.0.0.1"

type MkPairMsg = HostName -> PairMsg

{- Multicasts a message repeatedly on all interfaces until its thread
 - is killed, with a 2 second delay between each transmission.
 -
 - The remoteHostName is set to the best host name that can be found for
 - each interface's IP address. When possible, that's a .local name.
 - If not, it's whatever is found in the DNS for the address, or failing
 - that, the IP address.
 -
 - Note that new sockets are opened each time. This is hardly efficient,
 - but it allows new network interfaces to be used as they come up.
 - On the other hand, the expensive DNS lookups are cached. -}
multicastPairMsg :: MkPairMsg -> IO ThreadId
multicastPairMsg mkmsg = forkIO $ go =<< initMsgCache mkmsg
	where
		go cache = do
			addrs <- activeNetworkAddresses
			cache' <- updateMsgCache mkmsg cache addrs
			mapM_ (sendinterface cache') addrs
			threadDelaySeconds (Seconds 2)
			go cache'
		sendinterface cache i = void $ catchMaybeIO $ withSocketsDo $ do
			(sock, addr) <- multicastSender multicastAddress pairingPort
			setInterface sock (show i)
			maybe noop (\s -> void $ sendTo sock s addr)
				(M.lookup i cache)

{- A cache of serialized messages. -}
type MsgCache = M.Map SomeAddr String

{- Ensures that the cache has messages for each address. -}
updateMsgCache :: MkPairMsg -> MsgCache -> [SomeAddr] -> IO MsgCache
updateMsgCache _ m [] = return m
updateMsgCache mkmsg m (v:vs)
	| M.member v m = updateMsgCache mkmsg m vs
	| otherwise = do
		let sockaddr = case v of
			IPv4Addr (IPv4 a) -> SockAddrInet (PortNum 0) a
			IPv6Addr (IPv6 o1 o2 o3 o4) -> SockAddrInet6 (PortNum 0) 0 (o1, o2, o3, o4) 0
		mhostname <- catchDefaultIO (fst <$> getNameInfo [] True False sockaddr) Nothing
		let cache' = M.insert v (show $ mkmsg $ fromMaybe (show v) mhostname) m
		updateMsgCache mkmsg cache' vs

{- An initial message cache. Look up hostname.local, and if found, 
 - put it in the cache. -}
initMsgCache :: MkPairMsg -> IO MsgCache
initMsgCache mkmsg = go =<< getHostname
	where
		go Nothing = return M.empty
		go (Just n) = do
			let localname = n ++ ".local"
			addrs <- catchDefaultIO (getAddrInfo Nothing (Just localname) Nothing) []
			case headMaybe addrs of
				Nothing -> return M.empty
				Just addr -> case addrAddress addr of
					SockAddrInet _ a ->
						use localname $
							IPv4Addr $ IPv4 a
					SockAddrInet6 _ _ (o1, o2, o3, o4) _ ->
						use localname $	
							IPv6Addr $ IPv6 o1 o2 o3 o4
					_ -> return M.empty
		use hostname addr = return $ M.fromList [(addr, show $ mkmsg hostname)]

data SomeAddr = IPv4Addr IPv4 | IPv6Addr IPv6
	deriving (Ord, Eq)

instance Show SomeAddr where
	show (IPv4Addr x) = show x
	show (IPv6Addr x) = show x

activeNetworkAddresses :: IO [SomeAddr]
activeNetworkAddresses = filter (not . all (`elem` "0.:") . show)
	. concat . map (\ni -> [IPv4Addr $ ipv4 ni, IPv6Addr $ ipv6 ni])
	<$> getNetworkInterfaces
