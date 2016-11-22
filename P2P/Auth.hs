{- P2P protocol, authorization
 -
 - Copyright 2016 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module P2P.Auth where

import Common
import Utility.AuthToken

import qualified Data.Text as T

-- Use .git/annex/creds/p2p to hold AuthTokens of authorized peers.
getAuthTokens :: Annex AllowedAuthTokens
getAuthTokens = allowedAuthTokens <$> getAuthTokens'

getAuthTokens' :: Annex [AuthTokens]
getAuthTokens' = mapMaybe toAuthToken
	. map T.pack
	. lines
	. fromMaybe []
	<$> readCacheCreds "tor"

addAuthToken :: AuthToken -> Annex ()
addAuthToken t = do
	ts <- getAuthTokens'
	let d = unlines $ map (T.unpack . fromAuthToken) (t:ts)
	writeCacheCreds d "tor"
