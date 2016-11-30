{- P2P authtokens
 -
 - Copyright 2016 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module P2P.Auth where

import Annex.Common
import Creds
import P2P.Address
import Utility.AuthToken
import Utility.Tor

import qualified Data.Text as T

-- | Load authtokens that are accepted by this repository.
loadP2PAuthTokens :: Annex AllowedAuthTokens
loadP2PAuthTokens = allowedAuthTokens <$> loadP2PAuthTokens'

loadP2PAuthTokens' :: Annex [AuthToken]
loadP2PAuthTokens' = mapMaybe toAuthToken
        . map T.pack
        . lines
        . fromMaybe []
        <$> readCacheCreds p2pAuthCredsFile

-- | Stores an AuthToken, making it be accepted by this repository.
storeP2PAuthToken :: AuthToken -> Annex ()
storeP2PAuthToken t = do
	ts <- loadP2PAuthTokens'
	unless (t `elem` ts) $ do
		let d = unlines $ map (T.unpack . fromAuthToken) (t:ts)
		writeCacheCreds d p2pAuthCredsFile

p2pAuthCredsFile :: FilePath
p2pAuthCredsFile = "p2pauth"

-- | Loads the AuthToken to use when connecting with a given P2P address.
loadP2PRemoteAuthToken :: P2PAddress -> Annex (Maybe AuthToken)
loadP2PRemoteAuthToken addr = maybe Nothing (toAuthToken . T.pack)
	<$> readCacheCreds (addressCredsFile addr)

-- | Stores the AuthToken o use when connecting with a given P2P address.
storeP2PRemoteAuthToken :: P2PAddress -> AuthToken -> Annex ()
storeP2PRemoteAuthToken addr t = writeCacheCreds
	(T.unpack $ fromAuthToken t)
	(addressCredsFile addr)

addressCredsFile :: P2PAddress -> FilePath
-- We can omit the port and just use the onion address for the creds file,
-- because any given tor hidden service runs on a single port and has a
-- unique onion address.
addressCredsFile (TorAnnex (OnionAddress onionaddr) _port) = onionaddr
