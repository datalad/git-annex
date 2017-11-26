{- WebDAV locations.
 -
 - Copyright 2014 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
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
keyDir k = addTrailingPathSeparator $ hashdir </> keyFile k
  where
#ifndef mingw32_HOST_OS
	hashdir = hashDirLower def k
#else
	hashdir = replace "\\" "/" (hashDirLower def k)
#endif

keyLocation :: Key -> DavLocation
keyLocation k = keyDir k ++ keyFile k

exportLocation :: ExportLocation -> DavLocation
exportLocation = fromExportLocation

{- Where we store temporary data for a key as it's being uploaded. -}
keyTmpLocation :: Key -> DavLocation
keyTmpLocation = tmpLocation . keyFile

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
