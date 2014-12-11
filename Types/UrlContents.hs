{- git-annex URL contents
 -
 - Copyright 2014 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Types.UrlContents where

import Utility.Url

data UrlContents
	-- An URL contains a file, whose size may be known.
	-- A default filename will be provided, and can be overridded
	-- or built on.
	= UrlContents (Maybe Integer) (FilePath -> FilePath)
	-- Sometimes an URL points to multiple files, each accessible
	-- by their own URL.
	| UrlNested [(URLString, UrlContents)]
