{- Web url logs.
 -
 - Copyright 2011-2023 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE BangPatterns #-}

module Logs.Web (
	URLString,
	URLByteString,
	getUrls,
	getUrls',
	getUrlsWithPrefix,
	setUrlPresent,
	setUrlMissing,
	Downloader(..),
	getDownloader,
	setDownloader,
	setDownloader',
	setTempUrl,
	removeTempUrl,
	parseUrlLog,
) where

import qualified Data.Map as M
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L

import Annex.Common
import qualified Annex
import Logs
import Logs.Presence
import Logs.Location
import Utility.Url
import Annex.UUID
import qualified Annex.Branch
import qualified Types.Remote as Remote

type URLByteString = S.ByteString

{- Gets all urls that a key might be available from. -}
getUrls :: Key -> Annex [URLString]
getUrls key = do
	l <- map decodeBS <$> getUrls' key
	tmpl <- Annex.getState (maybeToList . M.lookup key . Annex.tempurls)
	return (tmpl ++ l)

{- Note that this does not include temporary urls set with setTempUrl. -}
getUrls' :: Key -> Annex [URLByteString]
getUrls' key = do
	config <- Annex.getGitConfig
	go $ urlLogFile config key : oldurlLogs config key
  where
	go [] = return []
	go (l:ls) = do
		us <- presentLogInfo l
		if null us
			then go ls
			else return $ map fromLogInfo us

getUrlsWithPrefix :: Key -> String -> Annex [URLString]
getUrlsWithPrefix key prefix = filter (prefix `isPrefixOf`) 
	. map (fst . getDownloader)
	<$> getUrls key

setUrlPresent :: Key -> URLString -> Annex ()
setUrlPresent key url = do
	-- Avoid reading the url log when not compacting, for speed.
	us <- ifM (annexAlwaysCompact <$> Annex.getGitConfig)
		( getUrls key
		, pure mempty
		)
	unless (url `elem` us) $ do
		config <- Annex.getGitConfig
		addLog (Annex.Branch.RegardingUUID []) (urlLogFile config key)
			InfoPresent (LogInfo (encodeBS url))
	-- If the url does not have an OtherDownloader, it must be present
	-- in the web.
	case snd (getDownloader url) of
		OtherDownloader -> return ()
		_ -> logChange NoLiveUpdate key webUUID InfoPresent

setUrlMissing :: Key -> URLString -> Annex ()
setUrlMissing key url = do
	-- Avoid making any changes if the url was not registered.
	us <- getUrls key
	when (url `elem` us) $ do
		config <- Annex.getGitConfig
		addLog (Annex.Branch.RegardingUUID []) (urlLogFile config key)
			InfoMissing (LogInfo (encodeBS url))
		-- If the url was a web url and none of the remaining urls
		-- for the key are web urls, the key must not be present
		-- in the web.
		when (isweb url && null (filter isweb $ filter (/= url) us)) $
			logChange NoLiveUpdate key webUUID InfoMissing
  where
	isweb u = case snd (getDownloader u) of
		OtherDownloader -> False
		_ -> True

setTempUrl :: Key -> URLString -> Annex ()
setTempUrl key url = Annex.changeState $ \s ->
	s { Annex.tempurls = M.insert key url (Annex.tempurls s) }

removeTempUrl :: Key -> Annex ()
removeTempUrl key = Annex.changeState $ \s ->
	s { Annex.tempurls = M.delete key (Annex.tempurls s) }

data Downloader = WebDownloader | YoutubeDownloader | QuviDownloader | OtherDownloader
	deriving (Eq, Show, Enum, Bounded)

{- To keep track of how an url is downloaded, it's mangled slightly in
 - the log, with a prefix indicating when a Downloader is used. -}
setDownloader :: URLString -> Downloader -> String
setDownloader u WebDownloader = u
setDownloader u QuviDownloader = "quvi:" ++ u
setDownloader u YoutubeDownloader = "yt:" ++ u
setDownloader u OtherDownloader = ":" ++ u

setDownloader' :: URLString -> Remote -> String
setDownloader' u r
	| Remote.uuid r == webUUID = setDownloader u WebDownloader
	| otherwise = setDownloader u OtherDownloader

getDownloader :: URLString -> (URLString, Downloader)
getDownloader u = case separate (== ':') u of
	("yt", u') -> (u', YoutubeDownloader)
	-- quvi is not used any longer; youtube-dl should be able to handle
	-- all urls it did.
	("quvi", u') -> (u', YoutubeDownloader)
	("", u') -> (u', OtherDownloader)
	_ -> (u, WebDownloader)

{- Parses the content of an url log file, returning the urls that are
 - currently recorded. -}
parseUrlLog :: L.ByteString -> [URLByteString]
parseUrlLog = map fromLogInfo . getLog
