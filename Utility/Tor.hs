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

-- | A unique identifier for a hidden service.
type UniqueIdent = String

-- | Name of application that is providing a hidden service.
type AppName = String

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
addHiddenService :: AppName -> UserID -> UniqueIdent -> IO (OnionAddress, OnionPort)
addHiddenService appname uid ident = do
	prepHiddenServiceSocketDir appname uid ident
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
				, "HiddenServiceDir " ++ hiddenServiceDir appname uid ident
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
	
	sockfile = hiddenServiceSocketFile appname uid ident

	-- An infinite random list of high ports.
	mkhighports g = 
		let (g1, g2) = R.split g
		in (R.randomRs (1025, 65534) g1, g2)

	waithiddenservice :: Int -> OnionPort -> IO (OnionAddress, OnionPort)
	waithiddenservice 0 _ = giveup "tor failed to create hidden service, perhaps the tor service is not running"
	waithiddenservice n p = do
		v <- tryIO $ readFile $ hiddenServiceHostnameFile appname uid ident
		case v of
			Right s | ".onion\n" `isSuffixOf` s ->
				return (OnionAddress (takeWhile (/= '\n') s), p)
			_ -> do
				threadDelaySeconds (Seconds 1)
				waithiddenservice (n-1) p

-- | A hidden service directory to use.
--
-- Has to be inside the torLibDir so tor can create it.
--
-- Has to end with "uid_ident" so getHiddenServiceSocketFile can find it.
hiddenServiceDir :: AppName -> UserID -> UniqueIdent -> FilePath
hiddenServiceDir appname uid ident = torLibDir </> appname ++ "_" ++ show uid ++ "_" ++ ident

hiddenServiceHostnameFile :: AppName -> UserID -> UniqueIdent -> FilePath
hiddenServiceHostnameFile appname uid ident = hiddenServiceDir appname uid ident </> "hostname"

-- | Location of the socket for a hidden service.
--
-- This has to be a location that tor can read from, and that the user
-- can write to. Since torLibDir is locked down, it can't go in there.
--
-- Note that some unix systems limit socket paths to 92 bytes long.
-- That should not be a problem if the UniqueIdent is around the length of
-- a UUID, and the AppName is short.
hiddenServiceSocketFile :: AppName -> UserID -> UniqueIdent -> FilePath
hiddenServiceSocketFile appname uid ident = varLibDir </> appname </> show uid ++ "_" ++ ident </> "s"

-- | Parse torrc, to get the socket file used for a hidden service with
-- the specified UniqueIdent.
getHiddenServiceSocketFile :: AppName -> UserID -> UniqueIdent -> IO (Maybe FilePath)
getHiddenServiceSocketFile _appname uid ident = 
	parse . map words . lines <$> catchDefaultIO "" (readFile torrc)
  where
	parse [] = Nothing
	parse (("HiddenServiceDir":hsdir:[]):("HiddenServicePort":_hsport:hsaddr:[]):rest)
		| "unix:" `isPrefixOf` hsaddr && hasident hsdir =
			Just (drop (length "unix:") hsaddr)
		| otherwise = parse rest
	parse (_:rest) = parse rest

	-- Don't look for AppName in the hsdir, because it didn't used to
	-- be included.
	hasident hsdir = (show uid ++ "_" ++ ident) `isSuffixOf` takeFileName hsdir

-- | Sets up the directory for the socketFile, with appropriate
-- permissions. Must run as root.
prepHiddenServiceSocketDir :: AppName -> UserID -> UniqueIdent -> IO ()
prepHiddenServiceSocketDir appname uid ident = do
	createDirectoryIfMissing True d
	setOwnerAndGroup d uid (-1)
	modifyFileMode d $
		addModes [ownerReadMode, ownerExecuteMode, ownerWriteMode]
  where
	d = takeDirectory $ hiddenServiceSocketFile appname uid ident

torrc :: FilePath
torrc = "/etc/tor/torrc"

torLibDir :: FilePath
torLibDir = "/var/lib/tor"

varLibDir :: FilePath
varLibDir = "/var/lib"
