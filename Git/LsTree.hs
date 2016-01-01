{- git ls-tree interface
 -
 - Copyright 2011-2016 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Git.LsTree (
	TreeItem(..),
	lsTree,
	lsTreeParams,
	lsTreeFiles,
	parseLsTree
) where

import Common
import Git
import Git.Command
import Git.Sha
import Git.FilePath
import qualified Git.Filename

import Numeric
import System.Posix.Types

data TreeItem = TreeItem
	{ mode :: FileMode
	, typeobj :: String
	, sha :: Ref
	, file :: TopFilePath
	} deriving Show

{- Lists the complete contents of a tree, recursing into sub-trees,
 - with lazy output. -}
lsTree :: Ref -> Repo -> IO ([TreeItem], IO Bool)
lsTree t repo = do
	(l, cleanup) <- pipeNullSplit (lsTreeParams t []) repo
	return (map parseLsTree l, cleanup)

lsTreeParams :: Ref -> [CommandParam] -> [CommandParam]
lsTreeParams r ps =
	[ Param "ls-tree"
	, Param "--full-tree"
	, Param "-z"
	, Param "-r"
	] ++ ps ++
	[ Param "--"
	, File $ fromRef r
	]

{- Lists specified files in a tree. -}
lsTreeFiles :: Ref -> [FilePath] -> Repo -> IO [TreeItem]
lsTreeFiles t fs repo = map parseLsTree <$> pipeNullSplitStrict ps repo
  where
	ps =
		[ Param "ls-tree"
		, Param "--full-tree"
		, Param "-z"
		, Param "--"
		, File $ fromRef t
		] ++ map File fs

{- Parses a line of ls-tree output.
 - (The --long format is not currently supported.) -}
parseLsTree :: String -> TreeItem
parseLsTree l = TreeItem 
	{ mode = fst $ Prelude.head $ readOct m
	, typeobj = t
	, sha = Ref s
	, file = asTopFilePath $ Git.Filename.decode f
	}
  where
	-- l = <mode> SP <type> SP <sha> TAB <file>
	-- All fields are fixed, so we can pull them out of
	-- specific positions in the line.
	(m, past_m) = splitAt 7 l
	(t, past_t) = splitAt 4 past_m
	(s, past_s) = splitAt shaSize $ Prelude.tail past_t
	f = Prelude.tail past_s
