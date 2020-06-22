{- Url downloading, with git-annex user agent and configured http
 - headers, security restrictions, etc.
 -
 - Copyright 2013-2020 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Annex.Url (
	withUrlOptions,
	withUrlOptionsPromptingCreds,
	getUrlOptions,
	getUserAgent,
	ipAddressesUnlimited,
	checkBoth,
	download,
	download',
	exists,
	getUrlInfo,
	U.downloadQuiet,
	U.URLString,
	U.UrlOptions(..),
	U.UrlInfo(..),
	U.sinkResponseFile,
	U.matchStatusCodeException,
	U.downloadConduit,
	U.downloadPartial,
	U.parseURIRelaxed,
	U.allowedScheme,
	U.assumeUrlExists,
) where

import Annex.Common
import qualified Annex
import qualified Utility.Url as U
import Utility.IPAddress
#ifdef WITH_HTTP_CLIENT_RESTRICTED
import Network.HTTP.Client.Restricted
#else
import Utility.HttpManagerRestricted
#endif
import Utility.Metered
import Git.Credential
import qualified BuildInfo

import Network.Socket
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Text.Read

defaultUserAgent :: U.UserAgent
defaultUserAgent = "git-annex/" ++ BuildInfo.packageversion

getUserAgent :: Annex U.UserAgent
getUserAgent = Annex.getState $ 
	fromMaybe defaultUserAgent . Annex.useragent

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
		U.mkUrlOptions
			<$> (Just <$> getUserAgent)
			<*> headers
			<*> pure urldownloader
			<*> pure manager
			<*> (annexAllowedUrlSchemes <$> Annex.getGitConfig)
			<*> pure U.noBasicAuth
	
	headers = annexHttpHeadersCommand <$> Annex.getGitConfig >>= \case
		Just cmd -> lines <$> liftIO (readProcess "sh" ["-c", cmd])
		Nothing -> annexHttpHeaders <$> Annex.getGitConfig
	
	checkallowedaddr = words . annexAllowedIPAddresses <$> Annex.getGitConfig >>= \case
		["all"] -> do
			-- Only allow curl when all are allowed,
			-- as its interface does not allow preventing
			-- it from accessing specific IP addresses.
			curlopts <- map Param . annexWebOptions <$> Annex.getGitConfig
			let urldownloader = if null curlopts
				then U.DownloadWithConduit $
					U.DownloadWithCurlRestricted mempty
				else U.DownloadWithCurl curlopts
			manager <- liftIO $ U.newManager $ 
				avoidtimeout $ tlsManagerSettings
			return (urldownloader, manager)
		allowedaddrsports -> do
			addrmatcher <- liftIO $ 
				(\l v -> any (\f -> f v) l) . catMaybes
					<$> mapM (uncurry makeAddressMatcher) 
						(mapMaybe splitAddrPort allowedaddrsports)
			-- Default to not allowing access to loopback
			-- and private IP addresses to avoid data
			-- leakage.
			let isallowed addr
				| addrmatcher addr = True
				| isLoopbackAddress addr = False
				| isPrivateAddress addr = False
				| otherwise = True
			let connectionrestricted = connectionRestricted 
				("Configuration of annex.security.allowed-ip-addresses does not allow accessing address " ++)
			let r = addressRestriction $ \addr ->
				if isallowed (addrAddress addr)
					then Nothing
					else Just (connectionrestricted addr)
			(settings, pr) <- liftIO $ 
				mkRestrictedManagerSettings r Nothing Nothing
			case pr of
				Nothing -> return ()
				Just ProxyRestricted -> toplevelWarning True
					"http proxy settings not used due to annex.security.allowed-ip-addresses configuration"
			manager <- liftIO $ U.newManager $ 
				avoidtimeout settings
			let urldownloader = U.DownloadWithConduit $
				U.DownloadWithCurlRestricted r
			return (urldownloader, manager)
	
	-- http-client defailts to timing out a request after 30 seconds
	-- or so, but some web servers are slower and git-annex has its own
	-- separate timeout controls, so disable that.
	avoidtimeout s = s { managerResponseTimeout = responseTimeoutNone }

splitAddrPort :: String -> Maybe (String, Maybe PortNumber)
splitAddrPort s
	-- "[addr]:port" (also allow "[addr]")
	| "[" `isPrefixOf` s = case splitc ']' (drop 1 s) of
		[a,cp] -> case splitc ':' cp of
			["",p] -> do
				pn <- readMaybe p
				return (a, Just pn)
			[""] -> Just (a, Nothing)
			_ -> Nothing
		_ -> Nothing
	| otherwise = Just (s, Nothing)

ipAddressesUnlimited :: Annex Bool
ipAddressesUnlimited = 
	("all" == ) . annexAllowedIPAddresses <$> Annex.getGitConfig

withUrlOptions :: (U.UrlOptions -> Annex a) -> Annex a
withUrlOptions a = a =<< getUrlOptions

-- When downloading an url, if authentication is needed, uses
-- git-credential to prompt for username and password.
withUrlOptionsPromptingCreds :: (U.UrlOptions -> Annex a) -> Annex a
withUrlOptionsPromptingCreds a = do
	g <- Annex.gitRepo
	uo <- getUrlOptions
	prompter <- mkPrompter
	a $ uo
		{ U.getBasicAuth = \u -> prompter $
			getBasicAuthFromCredential g u
		-- Can't download with curl and handle basic auth,
		-- so make sure it uses conduit.
		, U.urlDownloader = case U.urlDownloader uo of
			U.DownloadWithCurl _ -> U.DownloadWithConduit $
				U.DownloadWithCurlRestricted mempty
			v -> v
		}

checkBoth :: U.URLString -> Maybe Integer -> U.UrlOptions -> Annex Bool
checkBoth url expected_size uo =
	liftIO (U.checkBoth url expected_size uo) >>= \case
		Right r -> return r
		Left err -> warning err >> return False

download :: MeterUpdate -> U.URLString -> FilePath -> U.UrlOptions -> Annex Bool
download meterupdate url file uo =
	liftIO (U.download meterupdate url file uo) >>= \case
		Right () -> return True
		Left err -> warning err >> return False

download' :: MeterUpdate -> U.URLString -> FilePath -> U.UrlOptions -> Annex (Either String ())
download' meterupdate url file uo =
	liftIO (U.download meterupdate url file uo)

exists :: U.URLString -> U.UrlOptions -> Annex Bool
exists url uo = liftIO (U.exists url uo) >>= \case
	Right b -> return b
	Left err -> warning err >> return False

getUrlInfo :: U.URLString -> U.UrlOptions -> Annex (Either String U.UrlInfo)
getUrlInfo url uo = liftIO (U.getUrlInfo url uo)
