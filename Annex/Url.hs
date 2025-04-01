{- Url downloading, with git-annex user agent and configured http
 - headers, security restrictions, etc.
 -
 - Copyright 2013-2022 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

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
import qualified Utility.Url.Parse as U
import Annex.Hook
import Utility.Hash (IncrementalVerifier)
import Utility.IPAddress
import Network.HTTP.Client.Restricted
import Utility.Metered
import Git.Credential
import qualified BuildInfo

import Network.Socket
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Text.Read
import qualified Data.Set as S

defaultUserAgent :: U.UserAgent
defaultUserAgent = "git-annex/" ++ BuildInfo.packageversion

getUserAgent :: Annex U.UserAgent
getUserAgent = Annex.getRead $ 
	fromMaybe defaultUserAgent . Annex.useragent

getUrlOptions :: Maybe RemoteGitConfig -> Annex U.UrlOptions
getUrlOptions mgc = Annex.getState Annex.urloptions >>= \case
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
			<*> pure (Just (\u -> "Configuration of annex.security.allowed-url-schemes does not allow accessing " ++ show u))
			<*> pure U.noBasicAuth
	
	headers =
		outputOfAnnexHook httpHeadersAnnexHook annexHttpHeadersCommand
			>>= \case
				Just output -> pure (lines output)
				Nothing -> annexHttpHeaders <$> Annex.getGitConfig
			
	getweboptions = case mgc of
		Just gc | not (null (remoteAnnexWebOptions gc)) ->
			pure (remoteAnnexWebOptions gc)
		_ -> annexWebOptions <$> Annex.getGitConfig
	
	checkallowedaddr = words . annexAllowedIPAddresses <$> Annex.getGitConfig >>= \case
		["all"] -> do
			curlopts <- map Param <$> getweboptions
			allowedurlschemes <- annexAllowedUrlSchemes <$> Annex.getGitConfig
			let urldownloader = if null curlopts && not (any (`S.notMember` U.conduitUrlSchemes) allowedurlschemes)
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
			-- Curl is not used, as its interface does not allow
			-- preventing it from accessing specific IP addresses.
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

withUrlOptions :: Maybe RemoteGitConfig -> (U.UrlOptions -> Annex a) -> Annex a
withUrlOptions mgc a = a =<< getUrlOptions mgc

-- When downloading an url, if authentication is needed, uses
-- git-credential to prompt for username and password.
--
-- Note that, when the downloader is curl, it will not use git-credential.
-- If the user wants to, they can configure curl to use a netrc file that
-- handles authentication.
withUrlOptionsPromptingCreds :: Maybe RemoteGitConfig -> (U.UrlOptions -> Annex a) -> Annex a
withUrlOptionsPromptingCreds mgc a = do
	g <- Annex.gitRepo
	uo <- getUrlOptions mgc
	prompter <- mkPrompter
	cc <- Annex.getRead Annex.gitcredentialcache
	a $ uo
		{ U.getBasicAuth = \u -> prompter $
			getBasicAuthFromCredential g cc u
		}

checkBoth :: U.URLString -> Maybe Integer -> U.UrlOptions -> Annex Bool
checkBoth url expected_size uo =
	liftIO (U.checkBoth url expected_size uo) >>= \case
		Right r -> return r
		Left err -> warning (UnquotedString err) >> return False

download :: MeterUpdate -> Maybe IncrementalVerifier -> U.URLString -> OsPath -> U.UrlOptions -> Annex Bool
download meterupdate iv url file uo =
	liftIO (U.download meterupdate iv url file uo) >>= \case
		Right () -> return True
		Left err -> warning (UnquotedString err) >> return False

download' :: MeterUpdate -> Maybe IncrementalVerifier -> U.URLString -> OsPath -> U.UrlOptions -> Annex (Either String ())
download' meterupdate iv url file uo =
	liftIO (U.download meterupdate iv url file uo)

exists :: U.URLString -> U.UrlOptions -> Annex Bool
exists url uo = liftIO (U.exists url uo) >>= \case
	Right b -> return b
	Left err -> warning (UnquotedString err) >> return False

getUrlInfo :: U.URLString -> U.UrlOptions -> Annex (Either String U.UrlInfo)
getUrlInfo url uo = liftIO (U.getUrlInfo url uo)
