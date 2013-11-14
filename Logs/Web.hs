{- Web url logs.
 -
 - Copyright 2011, 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Logs.Web (
	URLString,
	webUUID,
	getUrls,
	setUrlPresent,
	setUrlMissing,
	knownUrls,
	Downloader(..),
	getDownloader,
	setDownloader,
) where

import qualified Data.ByteString.Lazy.Char8 as L
import Data.Tuple.Utils

import Common.Annex
import Logs
import Logs.Presence
import Logs.Location
import qualified Annex.Branch
import Annex.CatFile
import qualified Git
import qualified Git.LsFiles

type URLString = String

-- Dummy uuid for the whole web. Do not alter.
webUUID :: UUID
webUUID = UUID "00000000-0000-0000-0000-000000000001"

{- Gets all urls that a key might be available from. -}
getUrls :: Key -> Annex [URLString]
getUrls key = go $ urlLogFile key : oldurlLogs key
  where
	go [] = return []
	go (l:ls) = do
		us <- currentLog l
		if null us
			then go ls
			else return us

setUrlPresent :: Key -> URLString -> Annex ()
setUrlPresent key url = do
	us <- getUrls key
	unless (url `elem` us) $ do
		addLog (urlLogFile key) =<< logNow InfoPresent url
		-- update location log to indicate that the web has the key
		logChange key webUUID InfoPresent

setUrlMissing :: Key -> URLString -> Annex ()
setUrlMissing key url = do
	addLog (urlLogFile key) =<< logNow InfoMissing url
	whenM (null <$> getUrls key) $
		logChange key webUUID InfoMissing

{- Finds all known urls. -}
knownUrls :: Annex [URLString]
knownUrls = do
	{- Ensure the git-annex branch's index file is up-to-date and
	 - any journaled changes are reflected in it, since we're going
	 - to query its index directly. -}
	Annex.Branch.update
	Annex.Branch.commit "update"
	Annex.Branch.withIndex $ do
		top <- fromRepo Git.repoPath
		(l, cleanup) <- inRepo $ Git.LsFiles.stagedDetails [top]
		r <- mapM (geturls . snd3) $ filter (isUrlLog . fst3) l
		void $ liftIO cleanup
		return $ concat r
  where
	geturls Nothing = return []
  	geturls (Just logsha) = getLog . L.unpack <$> catObject logsha

data Downloader = DefaultDownloader | QuviDownloader

{- Determines the downloader for an URL.
 -
 - Some URLs are not downloaded by normal means, and this is indicated
 - by prefixing them with downloader: when they are recorded in the url
 - logs. -}
getDownloader :: URLString -> (URLString, Downloader)
getDownloader u = case separate (== ':') u of
	("quvi", u') -> (u', QuviDownloader)
	_ -> (u, DefaultDownloader)

setDownloader :: URLString -> Downloader -> URLString
setDownloader u DefaultDownloader = u
setDownloader u QuviDownloader = "quvi:" ++ u
