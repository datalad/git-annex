{- Helpers for tor remotes.
 -
 - Copyright 2016 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Remote.Helper.Tor where

import Annex.Common
import Utility.AuthToken
import Creds
import Utility.Tor
import Utility.Env

import Network.Socket
import qualified Data.Text as T

-- Read the first line of the creds file. Environment variable overrides.
getTorAuthTokenFor :: OnionAddress -> Annex (Maybe AuthToken)
getTorAuthTokenFor (OnionAddress onionaddress) =
	maybe Nothing mk <$> getM id 
		[ liftIO $ getEnv torAuthTokenEnv
		, readCacheCreds onionaddress
		]
  where
	mk = toAuthToken . T.pack . takeWhile (/= '\n')

torAuthTokenEnv :: String
torAuthTokenEnv = "GIT_ANNEX_TOR_AUTHTOKEN"

torHandle :: Socket -> IO Handle
torHandle s = do
	h <- socketToHandle s ReadWriteMode
	hSetBuffering h LineBuffering
	hSetBinaryMode h False
	fileEncoding h
	return h
