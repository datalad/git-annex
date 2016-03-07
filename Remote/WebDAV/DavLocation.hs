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
import Annex.Locations
import Utility.Url (URLString)

import System.FilePath.Posix -- for manipulating url paths
import Network.Protocol.HTTP.DAV (inDAVLocation, DAVT)
import Control.Monad.IO.Class (MonadIO)
import Data.Default
#ifdef mingw32_HOST_OS
import Data.String.Utils
#endif

-- Relative to the top of the DAV url.
type DavLocation = String

{- Runs action in subdirectory, relative to the current location. -}
inLocation :: (MonadIO m) => DavLocation -> DAVT m a -> DAVT m a
inLocation d = inDAVLocation (</> d)

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

{- Where we store temporary data for a key as it's being uploaded. -}
keyTmpLocation :: Key -> DavLocation
keyTmpLocation = tmpLocation . keyFile

tmpLocation :: FilePath -> DavLocation
tmpLocation f = tmpDir </> f

tmpDir :: DavLocation
tmpDir = "tmp"

locationParent :: String -> Maybe String
locationParent loc
	| loc `elem` tops = Nothing
	| otherwise = Just (takeDirectory loc)
  where
	tops = ["/", "", "."]

locationUrl :: URLString -> DavLocation -> URLString
locationUrl baseurl loc = baseurl </> loc
