{- P2P authtokens
 -
 - Copyright 2016 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module P2P.Auth where

import Annex.Common
import Creds
import Utility.AuthToken

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

storeP2PAuthToken :: AuthToken -> Annex ()
storeP2PAuthToken t = do
	ts <- loadP2PAuthTokens'
	unless (t `elem` ts) $ do
		let d = unlines $ map (T.unpack . fromAuthToken) (t:ts)
		writeCacheCreds d p2pAuthCredsFile

p2pAuthCredsFile :: FilePath
p2pAuthCredsFile = "p2pauth"
