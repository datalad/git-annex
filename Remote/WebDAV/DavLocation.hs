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
import Utility.FileSystemEncoding

import System.FilePath.Posix -- for manipulating url paths
import Network.Protocol.HTTP.DAV (inDAVLocation, DAVT)
import Control.Monad.IO.Class (MonadIO)
import Network.URI
import Data.Default

-- Relative to the top of the DAV url.
type DavLocation = String

{- Runs action with a new location relative to the current location. -}
inLocation :: (MonadIO m) => DavLocation -> DAVT m a -> DAVT m a
inLocation d = inDAVLocation (</> d')
  where
	d' = escapeURIString isUnescapedInURI d

{- The directory where files(s) for a key are stored. -}
keyDir :: Key -> DavLocation
keyDir k = addTrailingPathSeparator $ hashdir </> fromRawFilePath (keyFile k)
  where
#ifndef mingw32_HOST_OS
	hashdir = fromRawFilePath $ hashDirLower def k
#else
	hashdir = replace "\\" "/" (fromRawFilePath $ hashDirLower def k)
#endif

keyLocation :: Key -> DavLocation
keyLocation k = keyDir k ++ fromRawFilePath (keyFile k)

{- Paths containing # or ? cannot be represented in an url, so fails on
 - those. -}
exportLocation :: ExportLocation -> Either String DavLocation
exportLocation l =
	let p = fromRawFilePath $ fromExportLocation l
	in if any (`elem` p) illegalinurl
		then Left ("Cannot store file containing '#' or '?' on webdav: " ++ p)
		else Right p
  where
	illegalinurl = ['#', '?'] :: [Char]

{- Where we store temporary data for a key as it's being uploaded. -}
keyTmpLocation :: Key -> DavLocation
keyTmpLocation = tmpLocation . fromRawFilePath . keyFile

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
	| length (splitDirectories p) > 1 = takeDirectory p </> keyTmpLocation k
	| otherwise = keyTmpLocation k
  where
	p = fromRawFilePath (fromExportLocation l)

tmpLocation :: FilePath -> DavLocation
tmpLocation f = "git-annex-webdav-tmp-" ++ f

locationParent :: String -> Maybe String
locationParent loc
	| loc `elem` tops || parent `elem` tops = Nothing
	| otherwise = Just parent
  where
	tops = ["/", "", "."]
	parent = takeDirectory loc

locationUrl :: URLString -> DavLocation -> URLString
locationUrl baseurl loc = baseurl </> loc
