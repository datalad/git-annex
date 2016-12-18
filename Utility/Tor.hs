{- tor interface
 -
 - Copyright 2016 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Utility.Tor where

import Common
import Utility.ThreadScheduler
import Utility.FileMode

import System.PosixCompat.Types
import Data.Char
import Network.Socket
import Network.Socks5
import qualified Data.ByteString.UTF8 as BU8
import qualified System.Random as R

type OnionPort = Int

newtype OnionAddress = OnionAddress String
	deriving (Show, Eq)

type OnionSocket = FilePath

type UniqueIdent = String

connectHiddenService :: OnionAddress -> OnionPort -> IO Socket
connectHiddenService (OnionAddress address) port = do
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
	prepHiddenServiceSocketDir uid ident
	ls <- lines <$> readFile torrc
	let portssocks = mapMaybe (parseportsock . separate isSpace) ls
	case filter (\(_, s) -> s == sockfile) portssocks of
		((p, _s):_) -> waithiddenservice 1 p
		_ -> do
			highports <- R.getStdRandom mkhighports
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
				, ("service", [Param "tor", Param "reload"])
				]
			unless reloaded $
				giveup "failed to reload tor, perhaps the tor service is not running"
			waithiddenservice 120 newport
  where
	parseportsock ("HiddenServicePort", l) = do
		p <- readish $ takeWhile (not . isSpace) l
		return (p, drop 1 (dropWhile (/= ':') l))
	parseportsock _ = Nothing
	
	sockfile = hiddenServiceSocketFile uid ident

	-- An infinite random list of high ports.
	mkhighports g = 
		let (g1, g2) = R.split g
		in (R.randomRs (1025, 65534) g1, g2)

	waithiddenservice :: Int -> OnionPort -> IO (OnionAddress, OnionPort)
	waithiddenservice 0 _ = giveup "tor failed to create hidden service, perhaps the tor service is not running"
	waithiddenservice n p = do
		v <- tryIO $ readFile $ hiddenServiceHostnameFile uid ident
		case v of
			Right s | ".onion\n" `isSuffixOf` s ->
				return (OnionAddress (takeWhile (/= '\n') s), p)
			_ -> do
				threadDelaySeconds (Seconds 1)
				waithiddenservice (n-1) p

-- | A hidden service directory to use.
--
-- The "hs" is used in the name to prevent too long a path name,
-- which could present problems for socketFile.
hiddenServiceDir :: UserID -> UniqueIdent -> FilePath
hiddenServiceDir uid ident = libDir </> "hs_" ++ show uid ++ "_" ++ ident

hiddenServiceHostnameFile :: UserID -> UniqueIdent -> FilePath
hiddenServiceHostnameFile uid ident = hiddenServiceDir uid ident </> "hostname"

-- | Location of the socket for a hidden service.
--
-- This has to be a location that tor can read from, and that the user
-- can write to. Tor is often prevented by apparmor from reading
-- from many locations. Putting it in /etc is a FHS violation, but it's the
-- simplest and most robust choice until http://bugs.debian.org/846275 is
-- dealt with.
--
-- Note that some unix systems limit socket paths to 92 bytes long.
-- That should not be a problem if the UniqueIdent is around the length of
-- a UUID.
hiddenServiceSocketFile :: UserID -> UniqueIdent -> FilePath
hiddenServiceSocketFile uid ident = etcDir </> "hidden_services" </> show uid ++ "_" ++ ident </> "s"

-- | Sets up the directory for the socketFile, with appropriate
-- permissions. Must run as root.
prepHiddenServiceSocketDir :: UserID -> UniqueIdent -> IO ()
prepHiddenServiceSocketDir uid ident = do
	createDirectoryIfMissing True d
	setOwnerAndGroup d uid (-1)
	modifyFileMode d $
		addModes [ownerReadMode, ownerExecuteMode, ownerWriteMode]
  where
	d = takeDirectory $ hiddenServiceSocketFile uid ident

torrc :: FilePath
torrc = "/etc/tor/torrc"

libDir :: FilePath
libDir = "/var/lib/tor"

etcDir :: FilePath
etcDir = "/etc/tor"
