{- git-annex URL contents
 -
 - Copyright 2014 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Types.UrlContents (
	UrlContents(..),
) where

import Utility.Url

data UrlContents
	-- An URL contains a file, whose size may be known.
	-- There might be a nicer filename to use.
	= UrlContents (Maybe Integer) (Maybe FilePath)
	-- Sometimes an URL points to multiple files, each accessible
	-- by their own URL.
	| UrlMulti [(URLString, Maybe Integer, FilePath)]
