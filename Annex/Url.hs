{- Url downloading, with git-annex user agent.
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Annex.Url (
	module U,
	withUserAgent,
	getUserAgent,
) where

import Common.Annex
import qualified Annex
import Utility.Url as U
import qualified Build.SysConfig as SysConfig

defaultUserAgent :: U.UserAgent
defaultUserAgent = "git-annex/" ++ SysConfig.packageversion

getUserAgent :: Annex (Maybe U.UserAgent)
getUserAgent = Annex.getState $ 
	Just . fromMaybe defaultUserAgent . Annex.useragent

withUserAgent :: (Maybe U.UserAgent -> IO a) -> Annex a
withUserAgent a = liftIO . a =<< getUserAgent
