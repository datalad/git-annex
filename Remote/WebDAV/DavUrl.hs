{- WebDAV urls.
 -
 - Copyright 2014 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Remote.WebDAV.DavUrl where

import Types
import Locations

import Network.URI (normalizePathSegments)
import System.FilePath.Posix
#ifdef mingw32_HOST_OS
import Data.String.Utils
#endif

type DavUrl = String

{- The directory where files(s) for a key are stored. -}
davLocation :: DavUrl -> Key -> DavUrl
davLocation baseurl k = addTrailingPathSeparator $
	davUrl baseurl $ hashdir </> keyFile k
  where
#ifndef mingw32_HOST_OS
	hashdir = hashDirLower k
#else
	hashdir = replace "\\" "/" (hashDirLower k)
#endif

{- Where we store temporary data for a key as it's being uploaded. -}
tmpLocation :: DavUrl -> Key -> DavUrl
tmpLocation baseurl k = addTrailingPathSeparator $
	davUrl baseurl $ "tmp" </> keyFile k

davUrl :: DavUrl -> FilePath -> DavUrl
davUrl baseurl file = baseurl </> file

urlParent :: DavUrl -> DavUrl
urlParent url = dropTrailingPathSeparator $
	normalizePathSegments (dropTrailingPathSeparator url ++ "/..")
