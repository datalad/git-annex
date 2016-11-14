{- tor interface
 -
 - Copyright 2016 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Utility.Tor where

import Common
import Utility.ThreadScheduler
import Data.Char

type LocalPort = Int
type OnionPort = Int
type OnionAddress = String

-- | Adds a hidden service connecting to localhost on the specified local port.
-- This will only work if run as root, and tor has to already be running.
--
-- Picks a port number for the hidden service that is not used by any
-- other hidden service (and is >= 1024). Returns the hidden service's
-- onion address and port.

-- If there is already a hidden service for the specified local port,
-- returns its information without making any changes.
addHiddenService :: LocalPort -> IO (OnionAddress, OnionPort)
addHiddenService localport = do
	ls <- map (separate isSpace) . lines <$> readFile torrc
	let usedports = mapMaybe readish $
		map (drop 1 . dropWhile (/= ':')) $
		map snd $
		filter (\(k, _) -> k == "HiddenServicePort") ls
	let newport = Prelude.head $ filter (`notElem` usedports) [1024..]
	let dir = libDir </> "hidden_service" ++ show localport
	if localport `elem` usedports
		then waithiddenservice 1 dir newport
		else do
			writeFile torrc $ unlines $
				map (\(k, v) -> k ++ " " ++ v) ls ++
				[ ""
				, "HiddenServiceDir " ++ dir
				, "HiddenServicePort " ++ show newport ++ 
					" 127.0.0.1:" ++ show localport
				]
			-- Reload tor, so it will see the new hidden
			-- service and generate the hostname file for it.
			reloaded <- anyM (uncurry boolSystem)
				[ ("systemctl", [Param "reload", Param "tor"])
				, ("sefvice", [Param "tor", Param "reload"])
				]
			unless reloaded $
				error "failed to reload tor, perhaps the tor service is not running"
			waithiddenservice 120 dir newport
  where
	waithiddenservice :: Int -> FilePath -> OnionPort -> IO (OnionAddress, OnionPort)
	waithiddenservice 0 _ _ = error "tor failed to create hidden service, perhaps the tor service is not running"
	waithiddenservice n dir newport = do
		v <- tryIO $ readFile (dir </> "hostname")
		case v of
			Right s | ".onion\n" `isSuffixOf` s -> 
				return (takeWhile (/= '\n') s, newport)
			_ -> do
				threadDelaySeconds (Seconds 1)
				waithiddenservice (n-1) dir newport

torrc :: FilePath
torrc = "/etc/tor/torrc"

libDir :: FilePath
libDir = "/var/lib/tor"
