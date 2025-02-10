{- WebDAV locations.
 -
 - Copyright 2014 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}

module Remote.WebDAV.DavLocation where

import Types
import Types.Export
import Annex.Locations
import Utility.Url (URLString)
#ifdef mingw32_HOST_OS
import Utility.Split
#endif
import Utility.OsPath

import qualified System.FilePath.Posix as UrlPath
import Network.Protocol.HTTP.DAV (inDAVLocation, DAVT)
import Control.Monad.IO.Class (MonadIO)
import Network.URI
import Data.Default

-- Relative to the top of the DAV url.
type DavLocation = String

{- Runs action with a new location relative to the current location. -}
inLocation :: (MonadIO m) => DavLocation -> DAVT m a -> DAVT m a
inLocation d = inDAVLocation (UrlPath.</> d')
  where
	d' = escapeURIString isUnescapedInURI d

{- The directory where files(s) for a key are stored. -}
keyDir :: Key -> DavLocation
keyDir k = UrlPath.addTrailingPathSeparator $ 
	hashdir UrlPath.</> fromOsPath (keyFile k)
  where
#ifndef mingw32_HOST_OS
	hashdir = fromOsPath $ hashDirLower def k
#else
	hashdir = replace "\\" "/" (fromOsPath $ hashDirLower def k)
#endif

keyLocation :: Key -> DavLocation
keyLocation k = keyDir k ++ fromOsPath (keyFile k)

{- Paths containing # or ? cannot be represented in an url, so fails on
 - those. -}
exportLocation :: ExportLocation -> Either String DavLocation
exportLocation l =
	let p = fromOsPath $ fromExportLocation l
	in if any (`elem` p) illegalinurl
		then Left ("Cannot store file containing '#' or '?' on webdav: " ++ p)
		else Right p
  where
	illegalinurl = ['#', '?'] :: [Char]

{- Where we store temporary data for a key as it's being uploaded. -}
keyTmpLocation :: Key -> DavLocation
keyTmpLocation = tmpLocation . fromOsPath . keyFile

{- Where we store temporary data for a file as it's being exported.
 -
 - This could be just the keyTmpLocation, but when the file is in a
 - subdirectory, the temp file is put in there. Partly this is to keep
 - it close to the final destination; also certain webdav servers
 - seem to be buggy when renaming files from the root into a subdir, 
 - and so writing to the subdir avoids such problems.
 -}
exportTmpLocation :: ExportLocation -> Key -> DavLocation
exportTmpLocation l k
	| length (UrlPath.splitDirectories p) > 1 = 
		UrlPath.takeDirectory p UrlPath.</> keyTmpLocation k
	| otherwise = keyTmpLocation k
  where
	p = fromOsPath (fromExportLocation l)

tmpLocation :: FilePath -> DavLocation
tmpLocation f = "git-annex-webdav-tmp-" ++ f

locationParent :: String -> Maybe String
locationParent loc
	| loc `elem` tops || parent `elem` tops = Nothing
	| otherwise = Just parent
  where
	tops = ["/", "", "."]
	parent = UrlPath.takeDirectory loc

locationUrl :: URLString -> DavLocation -> URLString
locationUrl baseurl loc = baseurl UrlPath.</> loc
