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
	pipeNullSplit [Params "ls-tree --full-tree -z -r --", File $ show t] repo

{- Parses a line of ls-tree output.
 - (The --long format is not currently supported.) -}
parseLsTree :: String -> TreeItem
parseLsTree l = TreeItem 
	{ mode = fst $ Prelude.head $ readOct m
	, typeobj = t
	, sha = s
	, file = Git.Filename.decode f
	}
	where
		-- l = <mode> SP <type> SP <sha> TAB <file>
		-- All fields are fixed, so we can pull them out of
		-- specific positions in the line.
		(m, past_m) = splitAt 7 l
		(t, past_t) = splitAt 4 past_m
		(s, past_s) = splitAt 40 $ Prelude.tail past_t
		f = Prelude.tail past_s
