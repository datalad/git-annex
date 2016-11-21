{- tor interface
 -
 - Copyright 2016 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Utility.Tor where

import Common
import Utility.ThreadScheduler
import System.PosixCompat.Types
import Data.Char
import Network.Socket
import Network.Socks5
import qualified Data.ByteString.UTF8 as BU8
import qualified System.Random as R

type OnionPort = Int

newtype OnionAddress = OnionAddress String
	deriving (Show)

type OnionSocket = FilePath

type UniqueIdent = String

connectHiddenService :: OnionAddress -> OnionPort -> IO Socket
connectHiddenService (OnionAddress address) port = do
        soc <- socket AF_UNIX Stream defaultProtocol
	connect soc (SockAddrUnix "/run/user/1000/1ecd1f64-3234-47ec-876c-47c4bd7f7407.sock")
	return soc

connectHiddenService' :: OnionAddress -> OnionPort -> IO Socket
connectHiddenService' (OnionAddress address) port = do
	(s, _) <- socksConnect torsockconf socksaddr
	return s
  where
	torsocksport = 9050
	torsockconf = defaultSocksConf "127.0.0.1" torsocksport
	socksdomain = SocksAddrDomainName (BU8.fromString address)
	socksaddr = SocksAddress socksdomain (fromIntegral port)

-- | Adds a hidden service connecting to localhost, using some kind
-- of unique identifier.
--
-- This will only work if run as root, and tor has to already be running.
--
-- Picks a random high port number for the hidden service that is not
-- used by any other hidden service. Returns the hidden service's
-- onion address, port, and the unix socket file to use.
-- 
-- If there is already a hidden service for the specified unique
-- identifier, returns its information without making any changes.
addHiddenService :: UserID -> UniqueIdent -> IO (OnionAddress, OnionPort)
addHiddenService uid ident = do
	ls <- lines <$> readFile torrc
	let portssocks = mapMaybe (parseportsock . separate isSpace) ls
	case filter (\(_, s) -> s == sockfile) portssocks of
		((p, _s):_) -> waithiddenservice 1 p
		_ -> do
			highports <- R.getStdRandom highports
			let newport = Prelude.head $
				filter (`notElem` map fst portssocks) highports
			writeFile torrc $ unlines $
				ls ++
				[ ""
				, "HiddenServiceDir " ++ hiddenServiceDir uid ident
				, "HiddenServicePort " ++ show newport ++ 
					" unix:" ++ sockfile
				]
			-- Reload tor, so it will see the new hidden
			-- service and generate the hostname file for it.
			reloaded <- anyM (uncurry boolSystem)
				[ ("systemctl", [Param "reload", Param "tor"])
				, ("sefvice", [Param "tor", Param "reload"])
				]
			unless reloaded $
				error "failed to reload tor, perhaps the tor service is not running"
			waithiddenservice 120 newport
  where
	parseportsock ("HiddenServicePort", l) = do
		p <- readish $ takeWhile (not . isSpace) l
		return (p, drop 1 (dropWhile (/= ':') l))
	parseportsock _ = Nothing
	
	sockfile = socketFile uid ident

	-- An infinite random list of high ports.
	highports g = 
		let (g1, g2) = R.split g
		in (R.randomRs (1025, 65534) g1, g2)

	waithiddenservice :: Int -> OnionPort -> IO (OnionAddress, OnionPort)
	waithiddenservice 0 _ = error "tor failed to create hidden service, perhaps the tor service is not running"
	waithiddenservice n p = do
		v <- tryIO $ readFile $ hiddenServiceHostnameFile uid ident
		case v of
			Right s | ".onion\n" `isSuffixOf` s -> 
				return (OnionAddress (takeWhile (/= '\n') s), p)
			_ -> do
				threadDelaySeconds (Seconds 1)
				waithiddenservice (n-1) p

torrc :: FilePath
torrc = "/etc/tor/torrc"

libDir :: FilePath
libDir = "/var/lib/tor"

runDir :: UserID -> FilePath
runDir uid = "/var/run/user" </> show uid

socketFile :: UserID -> UniqueIdent -> FilePath
socketFile uid ident = runDir uid </> ident ++ ".sock"

hiddenServiceDir :: UserID -> UniqueIdent -> FilePath
hiddenServiceDir uid ident = libDir </> "hidden_service_" ++ show uid ++ "_" ++ ident

hiddenServiceHostnameFile :: UserID -> UniqueIdent -> FilePath
hiddenServiceHostnameFile uid ident = hiddenServiceDir uid ident </> "hostname"
