{- git-union-merge library
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Git.UnionMerge (
	merge,
	merge_index
) where

import qualified Data.Text.Lazy as L
import qualified Data.Text.Lazy.Encoding as L
import qualified Data.Set as S

import Common
import Git
import Git.Sha
import Git.CatFile
import Git.Command
import Git.UpdateIndex
import Git.HashObject
import Git.Types
import Git.FilePath

{- Performs a union merge between two branches, staging it in the index.
 - Any previously staged changes in the index will be lost.
 -
 - Should be run with a temporary index file configured by useIndex.
 -}
merge :: Ref -> Ref -> Repo -> IO ()
merge x y repo = do
	h <- catFileStart repo
	stream_update_index repo
		[ ls_tree x repo
		, merge_trees x y h repo
		]
	catFileStop h

{- Merges a list of branches into the index. Previously staged changed in
 - the index are preserved (and participate in the merge). -}
merge_index :: CatFileHandle -> Repo -> [Ref] -> IO ()
merge_index h repo bs =
	stream_update_index repo $ map (\b -> merge_tree_index b h repo) bs

{- For merging two trees. -}
merge_trees :: Ref -> Ref -> CatFileHandle -> Repo -> Streamer
merge_trees (Ref x) (Ref y) h = calc_merge h $ "diff-tree":diff_opts ++ [x, y]

{- For merging a single tree into the index. -}
merge_tree_index :: Ref -> CatFileHandle -> Repo -> Streamer
merge_tree_index (Ref x) h = calc_merge h $
	"diff-index" : diff_opts ++ ["--cached", x]

diff_opts :: [String]
diff_opts = ["--raw", "-z", "-r", "--no-renames", "-l0"]

{- Calculates how to perform a merge, using git to get a raw diff,
 - and generating update-index input. -}
calc_merge :: CatFileHandle -> [String] -> Repo -> Streamer
calc_merge ch differ repo streamer = gendiff >>= go
	where
		gendiff = pipeNullSplit (map Param differ) repo
		go [] = noop
		go (info:file:rest) = mergeFile info file ch repo >>=
			maybe (go rest) (\l -> streamer l >> go rest)
		go (_:[]) = error "calc_merge parse error"

{- Given an info line from a git raw diff, and the filename, generates
 - a line suitable for update-index that union merges the two sides of the
 - diff. -}
mergeFile :: String -> FilePath -> CatFileHandle -> Repo -> IO (Maybe String)
mergeFile info file h repo = case filter (/= nullSha) [Ref asha, Ref bsha] of
	[] -> return Nothing
	(sha:[]) -> use sha
	shas -> use
		=<< either return (\s -> hashObject BlobObject (unlines s) repo)
		=<< calcMerge . zip shas <$> mapM getcontents shas
	where
		[_colonmode, _bmode, asha, bsha, _status] = words info
		getcontents s = map L.unpack . L.lines .
			L.decodeUtf8 <$> catObject h s
		use sha = return $ Just $ update_index_line sha FileBlob $ asTopFilePath file

{- Calculates a union merge between a list of refs, with contents.
 -
 - When possible, reuses the content of an existing ref, rather than
 - generating new content.
 -}
calcMerge :: [(Ref, [String])] -> Either Ref [String]
calcMerge shacontents
	| null reuseable = Right $ new
	| otherwise = Left $ fst $ Prelude.head reuseable
	where
		reuseable = filter (\c -> sorteduniq (snd c) == new) shacontents
		new = sorteduniq $ concat $ map snd shacontents
		sorteduniq = S.toList . S.fromList
