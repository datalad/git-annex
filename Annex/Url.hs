{- Url downloading, with git-annex user agent and configured http
 - headers and curl options.
 -
 - Copyright 2013-2018 Joey Hess <id@joeyh.name>
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
import qualified BuildInfo

defaultUserAgent :: U.UserAgent
defaultUserAgent = "git-annex/" ++ BuildInfo.packageversion

getUserAgent :: Annex (Maybe U.UserAgent)
getUserAgent = Annex.getState $ 
	Just . fromMaybe defaultUserAgent . Annex.useragent

getUrlOptions :: Annex U.UrlOptions
getUrlOptions = Annex.getState Annex.urloptions >>= \case
	Just uo -> return uo
	Nothing -> do
		uo <- mk
		Annex.changeState $ \s -> s
			{ Annex.urloptions = Just uo }
		return uo
  where
	mk = mkUrlOptions
		<$> getUserAgent
		<*> headers
		<*> options
		<*> liftIO (U.newManager U.managerSettings)
	headers = annexHttpHeadersCommand <$> Annex.getGitConfig >>= \case
		Just cmd -> lines <$> liftIO (readProcess "sh" ["-c", cmd])
		Nothing -> annexHttpHeaders <$> Annex.getGitConfig
	options = map Param . annexWebOptions <$> Annex.getGitConfig

withUrlOptions :: (U.UrlOptions -> Annex a) -> Annex a
withUrlOptions a = a =<< getUrlOptions
