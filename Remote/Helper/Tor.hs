{- Helpers for tor remotes.
 -
 - Copyright 2016 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Remote.Helper.Tor where

import Annex.Common
import Remote.Helper.P2P (mkAuthToken, AuthToken)
import Creds
import Utility.Tor
import Utility.Env

import Network.Socket

getTorAuthToken :: OnionAddress -> Annex (Maybe AuthToken)
getTorAuthToken (OnionAddress onionaddress) =
	maybe Nothing mkAuthToken <$> getM id 
		[ liftIO $ getEnv torAuthTokenEnv
		, readCacheCreds onionaddress
		]

torAuthTokenEnv :: String
torAuthTokenEnv = "GIT_ANNEX_TOR_AUTHTOKEN"

torHandle :: Socket -> IO Handle
torHandle s = do
	h <- socketToHandle s ReadWriteMode
	hSetBuffering h LineBuffering
	hSetBinaryMode h False
	fileEncoding h
	return h
