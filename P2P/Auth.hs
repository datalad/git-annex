{- P2P authtokens
 -
 - Copyright 2016-2025 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module P2P.Auth where

import Annex.Common
import Creds
import P2P.Address
import Utility.AuthToken
import Utility.Tor
import Utility.Env

import qualified Data.Text as T

-- | Load authtokens that are accepted by this repository for tor.
loadP2PAuthTokensTor :: Annex AllowedAuthTokens
loadP2PAuthTokensTor = allowedAuthTokens 
	. map fst . filter istor
	<$> loadP2PAuthTokens'
  where
	istor (_, Nothing) = True
	istor _ = False

-- | Load authtokens that are accepted for a given P2PAddress.
loadP2PAuthTokens :: P2PAddress -> Annex AllowedAuthTokens
loadP2PAuthTokens addr = allowedAuthTokens 
	. map fst . filter ((== Just addr) . snd)
	<$> loadP2PAuthTokens'

loadP2PAuthTokens' :: Annex [(AuthToken, Maybe P2PAddress)]
loadP2PAuthTokens' = mapMaybe parse
        . lines
        . fromMaybe []
        <$> readCreds p2pAuthCredsFile
  where
	parse l = 
		let (tok, addr) = separate (== ' ') l
		in do
			tok' <- toAuthToken (T.pack tok) 
			return (tok', unformatP2PAddress addr)

-- | Stores an AuthToken, making it be accepted by this repository.
storeP2PAuthToken :: P2PAddress -> AuthToken -> Annex ()
storeP2PAuthToken addr t = do
	ts <- loadP2PAuthTokens'
	unless (v `elem` ts) $ do
		let d = unlines $ map fmt (v:ts)
		writeCreds d p2pAuthCredsFile
  where
	v = case addr of
		TorAnnex _ _ -> (t, Nothing)
		-- _ -> (t, Just addr)
	
	fmt (tok, Nothing) = T.unpack (fromAuthToken tok)
  	fmt (tok, Just addr') = T.unpack (fromAuthToken tok) 
		++ " " ++ formatP2PAddress addr'


p2pAuthCredsFile :: OsPath
p2pAuthCredsFile = literalOsPath "p2pauth"

-- | Loads the AuthToken to use when connecting with a given P2P address.
--
-- It's loaded from the first line of the creds file, but
-- GIT_ANNEX_P2P_AUTHTOKEN overrides.
loadP2PRemoteAuthToken :: P2PAddress -> Annex (Maybe AuthToken)
loadP2PRemoteAuthToken addr = maybe Nothing mk <$> getM id
	[ liftIO $ getEnv "GIT_ANNEX_P2P_AUTHTOKEN"
	, readCreds (addressCredsFile addr)
	]
  where
	mk = toAuthToken . T.pack . takeWhile (/= '\n')

p2pAuthTokenEnv :: String
p2pAuthTokenEnv = "GIT_ANNEX_P2P_AUTHTOKEN"

-- | Stores the AuthToken to use when connecting with a given P2P address.
storeP2PRemoteAuthToken :: P2PAddress -> AuthToken -> Annex ()
storeP2PRemoteAuthToken addr t = writeCreds
	(T.unpack $ fromAuthToken t)
	(addressCredsFile addr)

addressCredsFile :: P2PAddress -> OsPath
-- We can omit the port and just use the onion address for the creds file,
-- because any given tor hidden service runs on a single port and has a
-- unique onion address.
addressCredsFile (TorAnnex (OnionAddress onionaddr) _port) =
	toOsPath onionaddr
