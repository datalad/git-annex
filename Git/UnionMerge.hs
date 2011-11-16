{- git-union-merge library
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Git.UnionMerge (
	merge,
	merge_index,
	update_index,
	update_index_line,
	ls_tree
) where

import System.Cmd.Utils
import Data.List
import Data.Maybe
import qualified Data.ByteString.Lazy.Char8 as L

import Common
import Git
import Git.CatFile

{- Performs a union merge between two branches, staging it in the index.
 - Any previously staged changes in the index will be lost.
 -
 - Should be run with a temporary index file configured by Git.useIndex.
 -}
merge :: String -> String -> Repo -> IO ()
merge x y repo = do
	a <- ls_tree x repo
	h <- catFileStart repo
	b <- merge_trees x y h repo
	catFileStop h
	update_index repo (a++b)

{- Merges a list of branches into the index. Previously staged changed in
 - the index are preserved (and participate in the merge). -}
merge_index :: CatFileHandle -> Repo -> [String] -> IO ()
merge_index h repo bs =
	update_index repo =<< concat <$> mapM (\b -> merge_tree_index b h repo) bs

{- Feeds a list into update-index. Later items in the list can override
 - earlier ones, so the list can be generated from any combination of
 - ls_tree, merge_trees, and merge_tree_index. -}
update_index :: Repo -> [String] -> IO ()
update_index repo l = do
	(p, h) <- hPipeTo "git" (toCommand $ Git.gitCommandLine params repo)
	mapM_ (\s -> hPutStr h s >> hPutStr h "\0") l
	hClose h
	forceSuccess p
	where
		params = map Param ["update-index", "-z", "--index-info"]

{- Generates a line suitable to be fed into update-index, to add
 - a given file with a given sha. -}
update_index_line :: String -> FilePath -> String
update_index_line sha file = "100644 blob " ++ sha ++ "\t" ++ file

{- Gets the contents of a tree in a format suitable for update_index. -}
ls_tree :: String -> Repo -> IO [String]
ls_tree x = pipeNullSplit params
	where
		params = map Param ["ls-tree", "-z", "-r", "--full-tree", x]

{- For merging two trees. -}
merge_trees :: String -> String -> CatFileHandle -> Repo -> IO [String]
merge_trees x y h = calc_merge h $ "diff-tree":diff_opts ++ [x, y]

{- For merging a single tree into the index. -}
merge_tree_index :: String -> CatFileHandle -> Repo -> IO [String]
merge_tree_index x h = calc_merge h $ "diff-index":diff_opts ++ ["--cached", x]

diff_opts :: [String]
diff_opts = ["--raw", "-z", "-r", "--no-renames", "-l0"]

{- Calculates how to perform a merge, using git to get a raw diff,
 - and returning a list suitable for update_index. -}
calc_merge :: CatFileHandle -> [String] -> Repo -> IO [String]
calc_merge h differ repo = do
	diff <- pipeNullSplit (map Param differ) repo
	l <- mapM (\p -> mergeFile p h repo) (pairs diff)
	return $ catMaybes l
	where
		pairs [] = []
		pairs (_:[]) = error "calc_merge parse error"
		pairs (a:b:rest) = (a,b):pairs rest

{- Given an info line from a git raw diff, and the filename, generates
 - a line suitable for update_index that union merges the two sides of the
 - diff. -}
mergeFile :: (String, FilePath) -> CatFileHandle -> Repo -> IO (Maybe String)
mergeFile (info, file) h repo = case filter (/= nullsha) [asha, bsha] of
	[] -> return Nothing
	(sha:[]) -> return $ Just $ update_index_line sha file
	shas -> do
		content <- L.concat <$> mapM (catObject h) shas
		sha <- hashObject (unionmerge content) repo
		return $ Just $ update_index_line sha file
	where
		[_colonamode, _bmode, asha, bsha, _status] = words info
		nullsha = replicate shaSize '0'
		unionmerge = L.unlines . nub . L.lines

{- Injects some content into git, returning its hash. -}
hashObject :: L.ByteString -> Repo -> IO String
hashObject content repo = getSha subcmd $ do
	(h, s) <- pipeWriteRead (map Param params) content repo
	L.length s `seq` do
		forceSuccess h
		reap -- XXX unsure why this is needed
		return $ L.unpack s
	where
		subcmd = "hash-object"
		params = [subcmd, "-w", "--stdin"]
