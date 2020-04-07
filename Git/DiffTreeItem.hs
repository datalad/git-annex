{- git diff-tree item
 -
 - Copyright 2012 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Git.DiffTreeItem (
	DiffTreeItem(..),
) where

import System.Posix.Types
import qualified Data.ByteString as S

import Git.FilePath
import Git.Types

data DiffTreeItem = DiffTreeItem
	{ srcmode :: FileMode
	, dstmode :: FileMode
	, srcsha :: Sha -- null sha if file was added
	, dstsha :: Sha -- null sha if file was deleted
	, status :: S.ByteString
	, file :: TopFilePath
	} deriving Show
