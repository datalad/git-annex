{- git ls-tree interface
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Git.LsTree (
	TreeItem(..),
	lsTree,
	parseLsTree
) where

import Numeric
import Control.Applicative
import System.Posix.Types
import qualified Data.ByteString.Lazy.Char8 as L

import Git.ByteString
import Utility.SafeCommand

type Treeish = String

data TreeItem = TreeItem
	{ mode :: FileMode
	, typeobj :: String
	, sha :: String
	, file :: FilePath
	} deriving Show

{- Lists the contents of a Treeish -}
lsTree :: Repo -> Treeish -> IO [TreeItem]
lsTree repo t = map parseLsTree <$>
	pipeNullSplit repo [Params "ls-tree --full-tree -z -r --", File t]

{- Parses a line of ls-tree output.
 - (The --long format is not currently supported.) -}
parseLsTree :: L.ByteString -> TreeItem
parseLsTree l = TreeItem
		(fst $ head $ readOct $ L.unpack m)
		(L.unpack t)
		(L.unpack s)
		(decodeGitFile $ L.unpack f)
	where
		-- l = <mode> SP <type> SP <sha> TAB <file>
		-- All fields are fixed, so we can pull them out of
		-- specific positions in the line.
		(m, past_m) = L.splitAt 7 l
		(t, past_t) = L.splitAt 4 past_m
		(s, past_s) = L.splitAt 40 $ L.tail past_t
		f = L.tail past_s
