{- git diff-tree item
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Git.DiffTreeItem (
	DiffTreeItem(..),
) where

import System.Posix.Types

import Git.FilePath
import Git.Types

data DiffTreeItem = DiffTreeItem
	{ srcmode :: FileMode
	, dstmode :: FileMode
	, srcsha :: Sha -- nullSha if file was added
	, dstsha :: Sha -- nullSha if file was deleted
	, status :: String
	, file :: TopFilePath
	} deriving Show
