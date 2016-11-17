{- querying quvi (import qualified)
 -
 - Copyright 2013 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Utility.Quvi where

import Common
import Utility.Url

import Data.Aeson
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map as M
import Network.URI (uriAuthority, uriRegName)
import Data.Char

data QuviVersion
	= Quvi04
	| Quvi09
	| NoQuvi
	deriving (Show)

data Page = Page
	{ pageTitle :: String
	, pageLinks :: [Link]
	} deriving (Show)

data Link = Link
	{ linkSuffix :: Maybe String
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
		<$> v .:? "file_suffix"
		<*> v .: "url"
	parseJSON _ = mzero

{- "enum" format used by quvi 0.9 -}
parseEnum :: String -> Maybe Page
parseEnum s = Page
	<$> get "QUVI_MEDIA_PROPERTY_TITLE"
	<*> ((:[]) <$>
		( Link
			<$> Just <$> (get "QUVI_MEDIA_STREAM_PROPERTY_CONTAINER")
			<*> get "QUVI_MEDIA_STREAM_PROPERTY_URL"
		)
	    )
  where
	get = flip M.lookup m
	m = M.fromList $ map (separate (== '=')) $ lines s

probeVersion :: IO QuviVersion
probeVersion = catchDefaultIO NoQuvi $
	examine <$> processTranscript "quvi" ["--version"] Nothing
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
	onerr e = ifM (inPath "quvi")
		( giveup ("quvi failed: " ++ show e)
		, giveup "quvi is not installed"
		)

{- Returns Nothing if the page is not a video page, or quvi is not
 - installed. -}
query :: Query (Maybe Page)
query v ps url = flip catchNonAsync (const $ return Nothing) (query' v ps url)

query' :: Query (Maybe Page)
query' Quvi09 ps url = parseEnum
	<$> readQuvi (toCommand $ [Param "dump", Param "-p", Param "enum"] ++ ps ++ [Param url])
query' Quvi04 ps url = do
	let p = proc "quvi" (toCommand $ ps ++ [Param url])
	decode . BL.fromStrict
		<$> withHandle StdoutHandle createProcessSuccess p B.hGetContents
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
		[ Param "--verbosity", Param "mute"
		, Param "--support"
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
	<$> readQuvi (toCommand [Param "info", Param "-p", Param "domains"])
listdomains _ = return []

type QuviParams = QuviVersion -> [CommandParam]

{- Disables progress, but not information output. -}
quiet :: QuviParams
-- Cannot use quiet as it now disables informational output.
-- No way to disable progress.
quiet Quvi09 = [Param "--verbosity", Param "verbose"]
quiet Quvi04 = [Param "--verbosity", Param "quiet"]
quiet NoQuvi = []

{- Only return http results, not streaming protocols. -}
httponly :: QuviParams
-- No way to do it with 0.9?
httponly Quvi04 = [Param "-c", Param "http"]
httponly _ = [] -- No way to do it with 0.9?

{- Both versions of quvi will output utf-8 encoded data even when
 - the locale doesn't support it. -}
readQuvi :: [String] -> IO String
readQuvi ps = withHandle StdoutHandle createProcessSuccess p $ \h -> do
	fileEncoding h
	r <- hGetContentsStrict h
	hClose h
	return r
  where
	p = proc "quvi" ps
