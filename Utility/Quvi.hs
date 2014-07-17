{- querying quvi (import qualified)
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE OverloadedStrings #-}

module Utility.Quvi where

import Common
import Utility.Url

import Data.Aeson
import Data.ByteString.Lazy.UTF8 (fromString)
import qualified Data.Map as M
import Network.URI (uriAuthority, uriRegName)
import Data.Char

data QuviVersion
	= Quvi04
	| Quvi09
	| NoQuvi

data Page = Page
	{ pageTitle :: String
	, pageLinks :: [Link]
	} deriving (Show)

data Link = Link
	{ linkSuffix :: String
	, linkUrl :: URLString
	} deriving (Show)

{- JSON instances for quvi 0.4. -}
instance FromJSON Page where
	parseJSON (Object v) = Page
		<$> v .: "page_title"
		<*> v .: "link"
	parseJSON _ = mzero

instance FromJSON Link where
	parseJSON (Object v) = Link
		<$> v .: "file_suffix"
		<*> v .: "url"
	parseJSON _ = mzero

{- "enum" format used by quvi 0.9 -}
parseEnum :: String -> Maybe Page
parseEnum s = Page
	<$> get "QUVI_MEDIA_PROPERTY_TITLE"
	<*> ((:[]) <$>
		( Link
			<$> get "QUVI_MEDIA_STREAM_PROPERTY_CONTAINER"
			<*> get "QUVI_MEDIA_STREAM_PROPERTY_URL"
		)
	    )
  where
	get = flip M.lookup m
	m = M.fromList $ map (separate (== '=')) $ lines s

probeVersion :: IO QuviVersion
probeVersion = examine <$> processTranscript "quvi" ["--version"] Nothing
  where
	examine (s, True)
		| "quvi v0.4" `isInfixOf` s = Quvi04
		| otherwise = Quvi09
	examine _ = NoQuvi

type Query a = QuviVersion -> [CommandParam] -> URLString -> IO a

{- Throws an error when quvi is not installed. -}
forceQuery :: Query (Maybe Page)
forceQuery v ps url = query' v ps url `catchNonAsync` onerr
  where
	onerr _ = ifM (inPath "quvi")
		( error "quvi failed"
		, error "quvi is not installed"
		)

{- Returns Nothing if the page is not a video page, or quvi is not
 - installed. -}
query :: Query (Maybe Page)
query v ps url = flip catchNonAsync (const $ return Nothing) (query' v ps url)

query' :: Query (Maybe Page)
query' Quvi09 ps url = parseEnum
	<$> readProcess "quvi" (toCommand $ [Param "dump", Param "-p", Param "enum"] ++ ps ++ [Param url])
query' Quvi04 ps url = decode . fromString
	<$> readProcess "quvi" (toCommand $ ps ++ [Param url])
query' NoQuvi _ _ = return Nothing

queryLinks :: Query [URLString]
queryLinks v ps url = maybe [] (map linkUrl . pageLinks) <$> query v ps url

{- Checks if quvi can still find a download link for an url.
 - If quvi is not installed, returns False. -}
check :: Query Bool
check v ps url = maybe False (not . null . pageLinks) <$> query v ps url

{- Checks if an url is supported by quvi, as quickly as possible
 - (without hitting it if possible), and without outputting
 - anything. Also returns False if quvi is not installed. -}
supported :: QuviVersion -> URLString -> IO Bool
supported NoQuvi _ = return False
supported Quvi04 url = boolSystem "quvi"
		[ Params "--verbosity mute --support"
		, Param url
		]
{- Use quvi-info to see if the url's domain is supported.
 - If so, have to do a online verification of the url. -}
supported Quvi09 url = (firstlevel <&&> secondlevel)
		`catchNonAsync` (\_ -> return False)
  where
  	firstlevel = case uriAuthority =<< parseURIRelaxed url of
		Nothing -> return False
		Just auth -> do
			let domain = map toLower $ uriRegName auth
			let basedomain = intercalate "." $ reverse $ take 2 $ reverse $ split "." domain
			any (\h -> domain `isSuffixOf` h || basedomain `isSuffixOf` h) 
				. map (map toLower) <$> listdomains Quvi09
	secondlevel = snd <$> processTranscript "quvi"
		(toCommand [Param "dump", Param "-o", Param url]) Nothing

listdomains :: QuviVersion -> IO [String]
listdomains Quvi09 = concatMap (split ",") 
	. concatMap (drop 1 . words) 
	. filter ("domains: " `isPrefixOf`) . lines
	<$> readProcess "quvi"
		(toCommand [Param "info", Param "-p", Param "domains"])
listdomains _ = return []

type QuviParam = QuviVersion -> CommandParam

{- Disables progress, but not information output. -}
quiet :: QuviParam
-- Cannot use quiet as it now disables informational output.
-- No way to disable progress.
quiet Quvi09 = Params "--verbosity verbose"
quiet Quvi04 = Params "--verbosity quiet"
quiet NoQuvi = Params ""

{- Only return http results, not streaming protocols. -}
httponly :: QuviParam
-- No way to do it with 0.9?
httponly Quvi04 = Params "-c http"
httponly _ = Params "" -- No way to do it with 0.9?
