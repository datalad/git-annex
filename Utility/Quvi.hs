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

import Data.Aeson
import Data.ByteString.Lazy.UTF8 (fromString)

data Page = Page
	{ pageTitle :: String
	, pageLinks :: [Link]
	} deriving (Show)

data Link = Link
	{ linkSuffix :: String
	, linkUrl :: URLString
	} deriving (Show)

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
query' ps url = decode . fromString
	<$> readProcess "quvi" (toCommand $ ps ++ [Param url])

queryLinks :: Query [URLString]
queryLinks ps url = maybe [] (map linkUrl . pageLinks) <$> query ps url

{- Checks if quvi can still find a download link for an url.
 - If quvi is not installed, returns False. -}
check :: Query Bool
check ps url = maybe False (not . null . pageLinks) <$> query ps url

{- Checks if an url is supported by quvi, without hitting it, or outputting
 - anything. Also returns False if quvi is not installed. -}
supported :: URLString -> IO Bool
supported url = boolSystem "quvi" [Params "-v mute --support", Param url]

quiet :: CommandParam
quiet = Params "-v quiet"

noredir :: CommandParam
noredir = Params "-e -resolve"

{- Only return http results, not streaming protocols. -}
httponly :: CommandParam
httponly = Params "-c http"
