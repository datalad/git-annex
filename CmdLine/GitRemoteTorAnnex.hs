{- git-remote-tor-annex program
 -
 - Copyright 2016 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module CmdLine.GitRemoteTorAnnex where

import Common
import Utility.Tor
import P2P.Address
import CmdLine.GitRemoteP2PAnnex (runner)

run :: [String] -> IO ()
run = runner mkaddress
  where
 	mkaddress address =
		let (onionaddress, onionport)
			| '/' `elem` address = parseAddressPort $
				reverse $ takeWhile (/= '/') $ reverse address
			| otherwise = parseAddressPort address
		in TorAnnex onionaddress onionport

parseAddressPort :: String -> (OnionAddress, OnionPort)
parseAddressPort s = 
	let (a, sp) = separate (== ':') s
	in case readish sp of
		Nothing -> giveup "onion address must include port number"
		Just p -> (OnionAddress a, p)
