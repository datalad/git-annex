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

type OnionPort = Int
type OnionAddress = String
type OnionSocket = FilePath
type UniqueIdent = String

-- | Adds a hidden service connecting to localhost, using some kind
-- of unique identifier.
--
-- This will only work if run as root, and tor has to already be running.
--
-- Picks a port number for the hidden service that is not used by any
-- other hidden service (and is >= 1024). Returns the hidden service's
-- onion address, port, and the unix socket file to use.
-- 
-- If there is already a hidden service for the specified unique
-- identifier, returns its information without making any changes.
addHiddenService :: UserID -> UniqueIdent -> IO (OnionAddress, OnionPort, OnionSocket)
addHiddenService uid ident = do
	ls <- lines <$> readFile torrc
	let portssocks = mapMaybe (parseportsock . separate isSpace) ls
	case filter (\(_, s) -> s == sockfile) portssocks of
		((p, _s):_) -> waithiddenservice 1 p
		_ -> do
			let newport = Prelude.head $
				filter (`notElem` map fst portssocks) [1024..]
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

	waithiddenservice :: Int -> OnionPort -> IO (OnionAddress, OnionPort, OnionSocket)
	waithiddenservice 0 _ = error "tor failed to create hidden service, perhaps the tor service is not running"
	waithiddenservice n p = do
		v <- tryIO $ readFile $ hiddenServiceHostnameFile uid ident
		case v of
			Right s | ".onion\n" `isSuffixOf` s -> 
				return (takeWhile (/= '\n') s, p, sockfile)
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
