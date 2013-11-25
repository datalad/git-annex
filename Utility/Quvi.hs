{- querying quvi (import qualified)
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Utility.Quvi where

import Common
import Utility.Url
import Build.SysConfig (newquvi)

import Data.Aeson
import Data.ByteString.Lazy.UTF8 (fromString)
import qualified Data.Map as M
import Network.URI (uriAuthority, uriRegName)
import Data.Char

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
	<*> ((:[]) <$> link)
  where
	link = Link
		<$> get "QUVI_MEDIA_STREAM_PROPERTY_CONTAINER"
		<*> get "QUVI_MEDIA_STREAM_PROPERTY_URL"
	get = flip M.lookup m
	m = M.fromList $ map (separate (== '=')) $ lines s

type Query a = [CommandParam] -> URLString -> IO a

{- Throws an error when quvi is not installed. -}
forceQuery :: Query (Maybe Page)
forceQuery ps url = query' ps url `catchNonAsync` onerr
  where
	onerr _ = ifM (inPath "quvi")
		( error "quvi failed"
		, error "quvi is not installed"
		)

{- Returns Nothing if the page is not a video page, or quvi is not
 - installed. -}
query :: Query (Maybe Page)
query ps url = flip catchNonAsync (const $ return Nothing) (query' ps url)

query' :: Query (Maybe Page)
query' ps url
	| newquvi = parseEnum
		<$> readProcess "quvi" (toCommand $ [Param "dump", Param "-p", Param "enum"] ++ ps ++ [Param url])
	| otherwise = decode . fromString
		<$> readProcess "quvi" (toCommand $ ps ++ [Param url])

queryLinks :: Query [URLString]
queryLinks ps url = maybe [] (map linkUrl . pageLinks) <$> query ps url

{- Checks if quvi can still find a download link for an url.
 - If quvi is not installed, returns False. -}
check :: Query Bool
check ps url = maybe False (not . null . pageLinks) <$> query ps url

{- Checks if an url is supported by quvi, as quickly as possible
 - (without hitting it if possible), and without outputting
 - anything. Also returns False if quvi is not installed. -}
supported :: URLString -> IO Bool
supported url
	{- Use quvi-info to see if the url's domain is supported.
	 - If so, have to do a online verification of the url. -}
	| newquvi = (firstlevel <&&> secondlevel)
		`catchNonAsync` (\_ -> return False)
	| otherwise = boolSystem "quvi" [Params "--verbosity mute --support", Param url]
  where
  	firstlevel = case uriAuthority =<< parseURIRelaxed url of
		Nothing -> return False
		Just auth -> do
			let domain = map toLower $ uriRegName auth
			let basedomain = intercalate "." $ reverse $ take 2 $ reverse $ split "." domain
			any (\h -> domain `isSuffixOf` h || basedomain `isSuffixOf` h) 
				. map (map toLower) <$> listdomains
	secondlevel = snd <$> processTranscript "quvi"
		(toCommand [Param "dump", Param "-o", Param url]) Nothing

listdomains :: IO [String]
listdomains 
	| newquvi = concatMap (split ",") 
		. concatMap (drop 1 . words) 
		. filter ("domains: " `isPrefixOf`) . lines
		<$> readProcess "quvi"
			(toCommand [Param "info", Param "-p", Param "domains"])
	| otherwise = return []

{- Disables progress, but not information output. -}
quiet :: CommandParam
quiet
	-- Cannot use quiet as it now disables informational output.
	-- No way to disable progress.
	| newquvi = Params "--verbosity verbose"
	| otherwise = Params "--verbosity quiet"

{- Only return http results, not streaming protocols. -}
httponly :: CommandParam
httponly
	-- No way to do it with 0.9?
	| newquvi = Params ""
	| otherwise = Params "-c http"
