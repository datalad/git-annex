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
	stream_update_index,
	update_index_line,
	ls_tree
) where

import System.Cmd.Utils
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Set as S

import Common
import Git
import Git.Sha
import Git.CatFile
import Git.Command

type Streamer = (String -> IO ()) -> IO ()

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

{- Feeds content into update-index. Later items in the list can override
 - earlier ones, so the list can be generated from any combination of
 - ls_tree, merge_trees, and merge_tree_index. -}
update_index :: Repo -> [String] -> IO ()
update_index repo ls = stream_update_index repo [(`mapM_` ls)]

{- Streams content into update-index. -}
stream_update_index :: Repo -> [Streamer] -> IO ()
stream_update_index repo as = do
	(p, h) <- hPipeTo "git" (toCommand $ gitCommandLine params repo)
	forM_ as (stream h)
	hClose h
	forceSuccess p
	where
		params = map Param ["update-index", "-z", "--index-info"]
		stream h a = a (streamer h)
		streamer h s = do
			hPutStr h s
			hPutStr h "\0"

{- Generates a line suitable to be fed into update-index, to add
 - a given file with a given sha. -}
update_index_line :: Sha -> FilePath -> String
update_index_line sha file = "100644 blob " ++ show sha ++ "\t" ++ file

{- Gets the current tree for a ref. -}
ls_tree :: Ref -> Repo -> Streamer
ls_tree (Ref x) repo streamer = mapM_ streamer =<< pipeNullSplit params repo
	where
		params = map Param ["ls-tree", "-z", "-r", "--full-tree", x]

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
		go [] = return ()
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
	shas -> use =<< either return (hashObject repo . L.unlines) =<<
		calcMerge . zip shas <$> mapM getcontents shas
	where
		[_colonmode, _bmode, asha, bsha, _status] = words info
		getcontents s = L.lines <$> catObject h s
		use sha = return $ Just $ update_index_line sha file

{- Injects some content into git, returning its Sha. -}
hashObject :: Repo -> L.ByteString -> IO Sha
hashObject repo content = getSha subcmd $ do
	(h, s) <- pipeWriteRead (map Param params) content repo
	L.length s `seq` do
		forceSuccess h
		reap -- XXX unsure why this is needed
		return $ L.unpack s
	where
		subcmd = "hash-object"
		params = [subcmd, "-w", "--stdin"]

{- Calculates a union merge between a list of refs, with contents.
 -
 - When possible, reuses the content of an existing ref, rather than
 - generating new content.
 -}
calcMerge :: [(Ref, [L.ByteString])] -> Either Ref [L.ByteString]
calcMerge shacontents
	| null reuseable = Right $ new
	| otherwise = Left $ fst $ Prelude.head reuseable
	where
		reuseable = filter (\c -> sorteduniq (snd c) == new) shacontents
		new = sorteduniq $ concat $ map snd shacontents
		sorteduniq = S.toList . S.fromList
