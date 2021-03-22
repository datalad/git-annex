{- Web url logs.
 -
 - Copyright 2011-2020 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE BangPatterns #-}

module Logs.Web (
	URLString,
	getUrls,
	getUrlsWithPrefix,
	setUrlPresent,
	setUrlMissing,
	withKnownUrls,
	Downloader(..),
	getDownloader,
	setDownloader,
	setDownloader',
	setTempUrl,
	removeTempUrl,
) where

import qualified Data.Map as M

import Annex.Common
import qualified Annex
import Logs
import Logs.Presence
import Logs.Location
import qualified Annex.Branch
import qualified Git.LsTree
import Git.CatFile (catObjectStreamLsTree)
import Git.FilePath
import Utility.Url
import Annex.UUID
import qualified Types.Remote as Remote

{- Gets all urls that a key might be available from. -}
getUrls :: Key -> Annex [URLString]
getUrls key = do
	config <- Annex.getGitConfig
	l <- go $ urlLogFile config key : oldurlLogs config key
	tmpl <- Annex.getState (maybeToList . M.lookup key . Annex.tempurls)
	return (tmpl ++ l)
  where
	go [] = return []
	go (l:ls) = do
		us <- currentLogInfo l
		if null us
			then go ls
			else return $ map (decodeBS  . fromLogInfo) us

getUrlsWithPrefix :: Key -> String -> Annex [URLString]
getUrlsWithPrefix key prefix = filter (prefix `isPrefixOf`) 
	. map (fst . getDownloader)
	<$> getUrls key

setUrlPresent :: Key -> URLString -> Annex ()
setUrlPresent key url = do
	us <- getUrls key
	unless (url `elem` us) $ do
		config <- Annex.getGitConfig
		addLog (urlLogFile config key)
			=<< logNow InfoPresent (LogInfo (encodeBS url))
	-- If the url does not have an OtherDownloader, it must be present
	-- in the web.
	case snd (getDownloader url) of
		OtherDownloader -> return ()
		_ -> logChange key webUUID InfoPresent

setUrlMissing :: Key -> URLString -> Annex ()
setUrlMissing key url = do
	-- Avoid making any changes if the url was not registered.
	us <- getUrls key
	when (url `elem` us) $ do
		config <- Annex.getGitConfig
		addLog (urlLogFile config key)
			=<< logNow InfoMissing (LogInfo (encodeBS url))
		-- If the url was a web url and none of the remaining urls
		-- for the key are web urls, the key must not be present
		-- in the web.
		when (isweb url && null (filter isweb $ filter (/= url) us)) $
			logChange key webUUID InfoMissing
  where
	isweb u = case snd (getDownloader u) of
		OtherDownloader -> False
		_ -> True

{- Finds all known urls. -}
withKnownUrls :: (Annex (Maybe (Key, [URLString])) -> Annex a) -> Annex a
withKnownUrls a = do
	{- Ensure any journalled changes are committed to the git-annex
	 - branch, since we're going to look at its tree. -}
	_ <- Annex.Branch.update
	Annex.Branch.commit =<< Annex.Branch.commitMessage
	(l, cleanup) <- inRepo $ Git.LsTree.lsTree
		Git.LsTree.LsTreeRecursive
		Annex.Branch.fullname
	g <- Annex.gitRepo
	let want = urlLogFileKey . getTopFilePath . Git.LsTree.file
	catObjectStreamLsTree l want g (\reader -> a (go reader))
		`finally` void (liftIO cleanup)
  where
	go reader = liftIO reader >>= \case
		Just (k, Just content) ->
			case geturls content of
				[] -> go reader
				us -> return (Just (k, us))
		Just (_, Nothing) -> go reader
		Nothing -> return Nothing
	
	geturls = map (decodeBS . fromLogInfo) . getLog

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
