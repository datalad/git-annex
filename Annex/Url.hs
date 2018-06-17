{- Url downloading, with git-annex user agent and configured http
 - headers, security restrictions, etc.
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
	httpAddressesUnlimited,
) where

import Annex.Common
import qualified Annex
import Utility.Url as U
import Utility.IPAddress
import Utility.HttpManagerRestricted
import qualified BuildInfo

import Network.Socket

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
	mk = do
		(urldownloader, manager) <- checkallowedaddr
		mkUrlOptions
			<$> getUserAgent
			<*> headers
			<*> pure urldownloader
			<*> pure manager
			<*> (annexAllowedUrlSchemes <$> Annex.getGitConfig)
	
	headers = annexHttpHeadersCommand <$> Annex.getGitConfig >>= \case
		Just cmd -> lines <$> liftIO (readProcess "sh" ["-c", cmd])
		Nothing -> annexHttpHeaders <$> Annex.getGitConfig
	
	checkallowedaddr = words . annexAllowedHttpAddresses <$> Annex.getGitConfig >>= \case
		["all"] -> do
			-- Only allow curl when all are allowed,
			-- as its interface does not allow preventing
			-- it from accessing specific IP addresses.
			curlopts <- map Param . annexWebOptions <$> Annex.getGitConfig
			let urldownloader = if null curlopts
				then U.DownloadWithCurl curlopts
				else U.DownloadWithConduit
			manager <- liftIO $ U.newManager U.managerSettings
			return (urldownloader, manager)
		allowedaddrs -> do
			addrmatcher <- liftIO $ 
				(\l v -> any (\f -> f v) l) . catMaybes
					<$> mapM makeAddressMatcher allowedaddrs
			-- Default to not allowing access to loopback
			-- and private IP addresses to avoid data
			-- leakage.
			let isallowed addr
				| addrmatcher addr = True
				| isLoopbackAddress addr = False
				| isPrivateAddress addr = False
				| otherwise = True
			let r = Restriction
				{ addressRestriction = \addr ->
					if isallowed (addrAddress addr)
						then Nothing
						else Just (addrConnectionRestricted addr)
				}
			manager <- liftIO $ U.newManager $
				restrictManagerSettings r U.managerSettings
			return (U.DownloadWithConduit, manager)

httpAddressesUnlimited :: Annex Bool
httpAddressesUnlimited = 
	("all" == ) . annexAllowedHttpAddresses <$> Annex.getGitConfig

withUrlOptions :: (U.UrlOptions -> Annex a) -> Annex a
withUrlOptions a = a =<< getUrlOptions
