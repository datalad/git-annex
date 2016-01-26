{- Url downloading, with git-annex user agent and configured http
 - headers and wget/curl options.
 -
 - Copyright 2013-2014 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Annex.Url (
	module U,
	withUrlOptions,
	getUrlOptions,
	getUserAgent,
) where

import Annex.Common
import qualified Annex
import Utility.Url as U
import qualified Build.SysConfig as SysConfig

defaultUserAgent :: U.UserAgent
defaultUserAgent = "git-annex/" ++ SysConfig.packageversion

getUserAgent :: Annex (Maybe U.UserAgent)
getUserAgent = Annex.getState $ 
	Just . fromMaybe defaultUserAgent . Annex.useragent

getUrlOptions :: Annex U.UrlOptions
getUrlOptions = mkUrlOptions
	<$> getUserAgent
	<*> headers
	<*> options
  where
	headers = do
		v <- annexHttpHeadersCommand <$> Annex.getGitConfig
		case v of
			Just cmd -> lines <$> liftIO (readProcess "sh" ["-c", cmd])
			Nothing -> annexHttpHeaders <$> Annex.getGitConfig
	options = map Param . annexWebOptions <$> Annex.getGitConfig

withUrlOptions :: (U.UrlOptions -> IO a) -> Annex a
withUrlOptions a = liftIO . a =<< getUrlOptions
