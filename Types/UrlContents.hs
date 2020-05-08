{- git-annex URL contents
 -
 - Copyright 2014 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Types.UrlContents (
	UrlContents(..),
	SafeFilePath,
	mkSafeFilePath,
	fromSafeFilePath
) where

import Utility.Url
import Annex.UntrustedFilePath

import System.FilePath

data UrlContents
	-- An URL contains a file, whose size may be known.
	-- There might be a nicer filename to use.
	= UrlContents (Maybe Integer) (Maybe SafeFilePath)
	-- Sometimes an URL points to multiple files, each accessible
	-- by their own URL.
	| UrlMulti [(URLString, Maybe Integer, SafeFilePath)]

-- This is a FilePath, from an untrusted source,
-- sanitized so it doesn't contain any directory traversal tricks
-- and is always relative. It can still contain subdirectories.
-- Any unusual characters are also filtered out.
newtype SafeFilePath = SafeFilePath FilePath
	deriving (Show)

mkSafeFilePath :: FilePath -> SafeFilePath
mkSafeFilePath p = SafeFilePath $ if null p' then "file" else p'
  where
	p' = joinPath $ map sanitizeFilePath $ splitDirectories p

fromSafeFilePath :: SafeFilePath -> FilePath
fromSafeFilePath (SafeFilePath p) = p
