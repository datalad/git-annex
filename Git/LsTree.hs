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

import Common
import Git
import Git.Command
import qualified Git.Filename

data TreeItem = TreeItem
	{ mode :: FileMode
	, typeobj :: String
	, sha :: String
	, file :: FilePath
	} deriving Show

{- Lists the contents of a Ref -}
lsTree :: Ref -> Repo -> IO [TreeItem]
lsTree t repo = map parseLsTree <$>
	pipeNullSplitB [Params "ls-tree --full-tree -z -r --", File $ show t] repo

{- Parses a line of ls-tree output.
 - (The --long format is not currently supported.) -}
parseLsTree :: L.ByteString -> TreeItem
parseLsTree l = TreeItem 
	{ mode = fst $ Prelude.head $ readOct $ L.unpack m
	, typeobj = L.unpack t
	, sha = L.unpack s
	, file = Git.Filename.decode $ L.unpack f
	}
	where
		-- l = <mode> SP <type> SP <sha> TAB <file>
		-- All fields are fixed, so we can pull them out of
		-- specific positions in the line.
		(m, past_m) = L.splitAt 7 l
		(t, past_t) = L.splitAt 4 past_m
		(s, past_s) = L.splitAt 40 $ L.tail past_t
		f = L.tail past_s
