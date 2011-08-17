{- git-union-merge library
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Git.UnionMerge (
	merge,
	update_index,
	update_index_line,
	ls_tree
) where

import System.Cmd.Utils
import Data.List
import Data.Maybe
import Data.String.Utils

import Git
import Utility

{- Performs a union merge between two branches, staging it in the index.
 - Any previously staged changes in the index will be lost.
 -
 - When only one branch is specified, it is merged into the index.
 - In this case, previously staged changes in the index are preserved.
 -
 - Should be run with a temporary index file configured by Git.useIndex.
 -}
merge :: Repo -> [String] -> IO ()
merge g (x:y:[]) = do
	a <- ls_tree g x
	b <- merge_trees g x y
	update_index g (a++b)
merge g [x] = merge_tree_index g x >>= update_index g
merge _ _ = error "wrong number of branches to merge"

{- Feeds a list into update-index. Later items in the list can override
 - earlier ones, so the list can be generated from any combination of
 - ls_tree, merge_trees, and merge_tree_index. -}
update_index :: Repo -> [String] -> IO ()
update_index g l = togit ["update-index", "-z", "--index-info"] (join "\0" l)
	where
		togit ps content = pipeWrite g (map Param ps) content
			>>= forceSuccess

{- Generates a line suitable to be fed into update-index, to add
 - a given file with a given sha. -}
update_index_line :: String -> FilePath -> String
update_index_line sha file = "100644 blob " ++ sha ++ "\t" ++ file

{- Gets the contents of a tree in a format suitable for update_index. -}
ls_tree :: Repo -> String -> IO [String]
ls_tree g x = pipeNullSplit g $ 
	map Param ["ls-tree", "-z", "-r", "--full-tree", x]

{- For merging two trees. -}
merge_trees :: Repo -> String -> String -> IO [String]
merge_trees g x y = calc_merge g $ "diff-tree":diff_opts ++ [x, y]

{- For merging a single tree into the index. -}
merge_tree_index :: Repo -> String -> IO [String]
merge_tree_index g x = calc_merge g $ "diff-index":diff_opts ++ ["--cached", x]

diff_opts :: [String]
diff_opts = ["--raw", "-z", "-r", "--no-renames", "-l0"]

{- Calculates how to perform a merge, using git to get a raw diff,
 - and returning a list suitable for update_index. -}
calc_merge :: Repo -> [String] -> IO [String]
calc_merge g differ = do
	diff <- pipeNullSplit g $ map Param differ
	l <- mapM (mergeFile g) (pairs diff)
	return $ catMaybes l
	where
		pairs [] = []
		pairs (_:[]) = error "calc_merge parse error"
		pairs (a:b:rest) = (a,b):pairs rest

{- Given an info line from a git raw diff, and the filename, generates
 - a line suitable for update_index that union merges the two sides of the
 - diff. -}
mergeFile :: Repo -> (String, FilePath) -> IO (Maybe String)
mergeFile g (info, file) = case filter (/= nullsha) [asha, bsha] of
	[] -> return Nothing
	(sha:[]) -> return $ Just $ update_index_line sha file
	shas -> do
		content <- pipeRead g $ map Param ("show":shas)
		sha <- hashObject g $ unionmerge content
		return $ Just $ update_index_line sha file
	where
		[_colonamode, _bmode, asha, bsha, _status] = words info
		nullsha = replicate shaSize '0'
		unionmerge = unlines . nub . lines
