{- git trees
 -
 - Copyright 2016-2023 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE BangPatterns, TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Git.Tree (
	Tree(..),
	TreeContent(..),
	getTree,
	recordTree,
	recordTree',
	recordSubTree,
	TreeItem(..),
	treeItemsToTree,
	treeItemToLsTreeItem,
	lsTreeItemToTreeItem,
	adjustTree,
	graftTree,
	graftTree',
	withMkTreeHandle,
	MkTreeHandle,
	sendMkTree,
	finishMkTree,
	treeMode,
) where

import Common
import Git
import Git.FilePath
import Git.Types
import Git.Command
import Git.Sha
import qualified Git.LsTree as LsTree
import qualified Utility.CoProcess as CoProcess
import qualified Utility.OsString as OS

import Numeric
import System.Posix.Types
import Control.Monad.IO.Class
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.ByteString.Char8 as S8

newtype Tree = Tree [TreeContent]
	deriving (Show)

data TreeContent
	-- A blob object in the tree.
	= TreeBlob TopFilePath FileMode Sha
	-- A subtree that is already recorded in git, with a known sha.
	| RecordedSubTree TopFilePath Sha [TreeContent]
	-- A subtree that has not yet been recorded in git.
	| NewSubTree TopFilePath [TreeContent]
	-- A commit object that is part of a tree (used for submodules)
	| TreeCommit TopFilePath FileMode Sha
	deriving (Show, Eq, Ord)

{- Gets the Tree for a Ref. -}
getTree :: LsTree.LsTreeRecursive -> Ref -> Repo -> IO Tree
getTree recursive r repo = do
	(l, cleanup) <- lsTreeWithObjects recursive r repo
	let !t = either (\e -> giveup ("ls-tree parse error:" ++ e)) id
		(extractTree l)
	void cleanup
	return t

lsTreeWithObjects :: LsTree.LsTreeRecursive -> Ref -> Repo -> IO ([LsTree.TreeItem], IO Bool)
lsTreeWithObjects recursive = 
	LsTree.lsTree' [Param "-t"] recursive (LsTree.LsTreeLong False)

newtype MkTreeHandle = MkTreeHandle CoProcess.CoProcessHandle

withMkTreeHandle :: (MonadIO m, MonadMask m) => Repo -> (MkTreeHandle -> m a) -> m a
withMkTreeHandle repo a = bracketIO setup cleanup (a . MkTreeHandle)
  where
	setup = gitCoProcessStart False ps repo
	ps = [Param "mktree", Param "--missing", Param "--batch", Param "-z"]
	cleanup = CoProcess.stop

{- Records a Tree in the Repo, returning its Sha.
 - 
 - Efficiently handles subtrees, by only recording ones that have not
 - already been recorded before. And even when many subtrees need to be
 - recorded, it's done with a single call to git mktree, using its batch
 - interface.
 -}
recordTree :: Tree -> Repo -> IO Sha
recordTree t repo = withMkTreeHandle repo $ \h -> recordTree' h t

recordTree' :: MkTreeHandle -> Tree -> IO Sha
recordTree' h (Tree l) = mkTree h =<< mapM (recordSubTree h) l

{- Note that the returned RecordedSubTree does not have its [TreeContent]
 - list populated. This is a memory optimisation, since the list is not
 - used. -}
recordSubTree :: MkTreeHandle -> TreeContent -> IO TreeContent
recordSubTree h (NewSubTree d l) = do
	sha <- mkTree h =<< mapM (recordSubTree h) l
	return (RecordedSubTree d sha [])
recordSubTree _ alreadyrecorded = return alreadyrecorded

sendMkTree :: MkTreeHandle -> FileMode -> ObjectType -> Sha -> TopFilePath -> IO ()
sendMkTree (MkTreeHandle cp) fm ot s f =
	CoProcess.send cp $ \h -> 
		hPutStr h (mkTreeOutput fm ot s f)

finishMkTree :: MkTreeHandle -> IO Sha
finishMkTree (MkTreeHandle cp) = do
	CoProcess.send cp $ \h ->
		-- NUL to signal end of tree to --batch
		hPutStr h "\NUL"
	getSha "mktree" (CoProcess.receive cp S8.hGetLine)

mkTree :: MkTreeHandle -> [TreeContent] -> IO Sha
mkTree h l = do
	forM_ l $ \case
		TreeBlob f fm s -> sendMkTree h fm BlobObject s f
		RecordedSubTree f s _ -> sendMkTree h treeMode TreeObject s f
		NewSubTree _ _ -> error "recordSubTree internal error; unexpected NewSubTree"
		TreeCommit f fm s -> sendMkTree h fm CommitObject s f
	finishMkTree h

treeMode :: FileMode
treeMode = 0o040000

mkTreeOutput :: FileMode -> ObjectType -> Sha -> TopFilePath -> String
mkTreeOutput fm ot s f = concat
	[ showOct fm ""
	, " "
	, decodeBS (fmtObjectType ot)
	, " "
	, fromRef s
	, "\t"
	, fromOsPath (takeFileName (getTopFilePath f))
	, "\NUL"
	]

data TreeItem = TreeItem TopFilePath FileMode Sha
	deriving (Show, Eq)

treeItemToTreeContent :: TreeItem -> TreeContent
treeItemToTreeContent (TreeItem f m s) = case toTreeItemType m of
	Just TreeSubmodule -> TreeCommit f m s
	_ -> TreeBlob f m s

treeItemToLsTreeItem :: TreeItem -> LsTree.TreeItem
treeItemToLsTreeItem (TreeItem f mode sha) = LsTree.TreeItem
	{ LsTree.mode = mode
	, LsTree.typeobj = fmtObjectType $ case toTreeItemType mode of
		Just TreeSubmodule -> CommitObject
		Just TreeSubtree -> TreeObject
		_ -> BlobObject
	, LsTree.sha = sha
	, LsTree.size = Nothing
	, LsTree.file = f
	}

lsTreeItemToTreeItem :: LsTree.TreeItem -> TreeItem
lsTreeItemToTreeItem ti = TreeItem
	(LsTree.file ti)
	(LsTree.mode ti)
	(LsTree.sha ti)

treeItemsToTree :: [TreeItem] -> Tree
treeItemsToTree = go M.empty
  where
	go m [] = Tree $ filter inTopTree (M.elems m)
	go m (i:is)
		| inTopTree p =
			go (M.insert p (treeItemToTreeContent i) m) is
		| otherwise = case M.lookup idir m of
			Just (NewSubTree d l) ->
				go (addsubtree idir m (NewSubTree d (c:l))) is
			_ ->
				go (addsubtree idir m (NewSubTree (asTopFilePath idir) [c])) is
	  where
		p = gitPath i
		idir = takeDirectory p
		c = treeItemToTreeContent i

	addsubtree d m t
		| not (inTopTree d) = 
			let m' = M.insert d t m
			in case M.lookup parent m' of
				Just (NewSubTree d' l) ->
					let l' = filter (\ti -> gitPath ti /= d) l
					in addsubtree parent m' (NewSubTree d' (t:l'))
				_ -> addsubtree parent m' (NewSubTree (asTopFilePath parent) [t])
		| otherwise = M.insert d t m
	  where
		parent = takeDirectory d

{- Flattens the top N levels of a Tree. -}
flattenTree :: Int -> Tree -> Tree
flattenTree 0 t = t
flattenTree n (Tree l) = Tree (concatMap (go n) l)
  where
	go 0 c = [c]
	go _ b@(TreeBlob _ _ _) = [b]
	go n' (RecordedSubTree _ _ l') = concatMap (go (n'-1)) l'
	go n' (NewSubTree _ l') = concatMap (go (n'-1)) l'
	go _ c@(TreeCommit _ _ _) = [c]

{- Applies an adjustment to items in a tree.
 -
 - While less flexible than using getTree and recordTree,
 - this avoids buffering the whole tree in memory.
 -}
adjustTree
	:: (Functor m, MonadIO m, MonadMask m)
	=> (TreeItem -> m (Maybe TreeItem))
	-- ^ Adjust an item in the tree. Nothing deletes the item.
	-- Cannot move the item to a different tree.
	-> [TreeItem]
	-- ^ New items to add to the tree.
	-> (TreeContent -> TreeContent -> TreeContent)
	-- ^ When adding a new item to the tree and an item with the same
	-- name already exists, this function picks which to use.
	-- The first one is the item that was already in the tree.
	-> [TopFilePath]
	-- ^ Files to remove from the tree.
	-> Ref
	-> Repo
	-> m Sha
adjustTree adjusttreeitem addtreeitems resolveaddconflict removefiles r repo =
	withMkTreeHandle repo $ \h -> do
		(l, cleanup) <- liftIO $ lsTreeWithObjects LsTree.LsTreeRecursive r repo
		(l', _, _) <- go h False [] 1 inTopTree l
		l'' <- adjustlist h 0 inTopTree topTreePath l'
		sha <- liftIO $ mkTree h l''
		void $ liftIO cleanup
		return sha
  where
	go _ wasmodified c _ _ [] = return (c, wasmodified, [])
	go h wasmodified c depth intree (i:is)
		| intree i = case readObjectType (LsTree.typeobj i) of
			Just BlobObject -> do
				let ti = TreeItem (LsTree.file i) (LsTree.mode i) (LsTree.sha i)
				v <- adjusttreeitem ti
				case v of
					Nothing -> go h True c depth intree is
					Just ti'@(TreeItem f m s) ->
						let !modified = wasmodified || ti' /= ti
						    blob = TreeBlob f m s
						in go h modified (blob:c) depth intree is
			Just TreeObject -> do
				(sl, modified, is') <- go h False [] (depth+1) (beneathSubTree i) is
				sl' <- adjustlist h depth (inTree i) (gitPath i) sl
				let slmodified = sl' /= sl
				subtree <- if modified || slmodified
					then liftIO $ recordSubTree h $ NewSubTree (LsTree.file i) sl'
					else return $ RecordedSubTree (LsTree.file i) (LsTree.sha i) [] 
				let !modified' = modified || slmodified || wasmodified
				go h modified' (subtree : c) depth intree is'
			Just CommitObject -> do
				let ti = TreeItem (LsTree.file i) (LsTree.mode i) (LsTree.sha i)
				v <- adjusttreeitem ti
				case v of
					Nothing -> go h True c depth intree is
					Just (TreeItem f m s) -> 
						let commit = TreeCommit f m s
						in go h wasmodified (commit:c) depth intree is
			_ -> giveup ("unexpected object type \"" ++ decodeBS (LsTree.typeobj i) ++ "\"")
		| otherwise = return (c, wasmodified, i:is)

	adjustlist h depth ishere herepath l = do
		let addhere = fromMaybe [] $ M.lookup herepath addtreeitempathmap
		let l' = filter (not . removed) $
			addoldnew l (map treeItemToTreeContent addhere)
		let inl i = any (\t -> beneathSubTree t i) l'
		let (Tree addunderhere) = flattenTree depth $ treeItemsToTree $
			filter (not . inl) $ if herepath == topTreePath
				then filter (not . ishere) addtreeitems
				else fromMaybe [] $
					M.lookup (subTreePrefix herepath) addtreeitemprefixmap
		addunderhere' <- liftIO $ mapM (recordSubTree h) addunderhere
		return (addoldnew l' addunderhere')

	addtreeitempathmap = mkPathMap addtreeitems
	addtreeitemprefixmap = mkSubTreePathPrefixMap addtreeitems

	removeset = S.fromList $ map (normalise . gitPath) removefiles
	removed (TreeBlob f _ _) = S.member (normalise (gitPath f)) removeset
	removed (TreeCommit f _ _) = S.member (normalise (gitPath f)) removeset
	removed (RecordedSubTree _ _ _) = False
	removed (NewSubTree _ _) = False

	addoldnew [] new = new
	addoldnew old [] = old
	addoldnew old new = addoldnew' (M.fromList $ map (\i -> (mkk i, i)) old) new
	addoldnew' oldm (n:ns) = 
			let k = mkk n
			in case M.lookup k oldm of
				Just o -> 
					resolveaddconflict o n
					: 
					addoldnew' (M.delete k oldm) ns
				Nothing -> n : addoldnew' oldm ns
	addoldnew' oldm [] = M.elems oldm
	mkk = normalise . gitPath

{- Grafts subtree into the basetree at the specified location, replacing
 - anything that the basetree already had at that location.
 -
 - This is generally much more efficient than using getTree and recordTree,
 - or adjustTree, since it only needs to traverse from the top of the tree
 - down to the graft location. It does not buffer the whole tree in memory.
 -}
graftTree
	:: Sha
	-> TopFilePath
	-> Sha
	-> Repo
	-> IO Sha
graftTree subtree graftloc basetree repo =
	withMkTreeHandle repo $ graftTree' subtree graftloc basetree repo

graftTree'
	:: Sha
	-> TopFilePath
	-> Sha
	-> Repo
	-> MkTreeHandle
	-> IO Sha
graftTree' subtree graftloc basetree repo hdl = go basetree subdirs graftdirs 
  where
	go tsha (subdir:restsubdirs) (topmostgraphdir:restgraphdirs) = do
		Tree t <- getTree LsTree.LsTreeNonRecursive tsha repo
		let abovegraftpoint i = gitPath i == gitPath subdir
		t' <- case partition abovegraftpoint t of
			-- the graft point is not already in the tree,
			-- so graft it in, keeping the existing tree
			-- content
			([], _) -> do
				graft <- graftin (topmostgraphdir:restgraphdirs)
				return (graft:t)
			(matching, rest) -> do
				newshas <- forM matching $ \case
					RecordedSubTree tloc tsha' _
						| null restgraphdirs -> return $
							RecordedSubTree tloc subtree []
						| otherwise -> do
							tsha'' <- go tsha' restsubdirs restgraphdirs
							return $ RecordedSubTree tloc tsha'' []
					_ -> graftin (topmostgraphdir:restgraphdirs)
				return (newshas ++ rest)
		mkTree hdl t'
	go _ _ [] = return subtree
	go _ [] _ = return subtree
	
	graftin t = recordSubTree hdl $ graftin' t
	graftin' [] = RecordedSubTree graftloc subtree []
	graftin' (d:rest) 
		| d == graftloc = graftin' []
		| otherwise = NewSubTree d [graftin' rest]

	subdirs = splitDirectories $ gitPath graftloc

	graftdirs = map (asTopFilePath . toInternalGitPath) $
		pathPrefixes subdirs

{- Assumes the list is ordered, with tree objects coming right before their
 - contents. -}
extractTree :: [LsTree.TreeItem] -> Either String Tree
extractTree l = case go [] inTopTree l of
	Right (t, []) -> Right (Tree t)
	Right _ -> parseerr "unexpected tree form"
	Left e -> parseerr e
  where
	go t _ [] = Right (t, [])
	go t intree (i:is)
		| intree i = case readObjectType (LsTree.typeobj i) of
			Just BlobObject ->
				let b = TreeBlob (LsTree.file i) (LsTree.mode i) (LsTree.sha i)
				in go (b:t) intree is
			Just TreeObject -> case go [] (beneathSubTree i) is of
				Right (subtree, is') ->
					let st = RecordedSubTree (LsTree.file i) (LsTree.sha i) subtree
					in go (st:t) intree is'
				Left e -> Left e
			Just CommitObject ->
				let c = TreeCommit (LsTree.file i) (LsTree.mode i) (LsTree.sha i)
				in go (c:t) intree is
			_ -> parseerr ("unexpected object type \"" ++ decodeBS (LsTree.typeobj i) ++ "\"")
		| otherwise = Right (t, i:is)
	parseerr = Left

class GitPath t where
	gitPath :: t -> OsPath

instance GitPath OsPath where
	gitPath = id

instance GitPath FilePath where
	gitPath = toOsPath

instance GitPath TopFilePath where
	gitPath = getTopFilePath

instance GitPath TreeItem where
	gitPath (TreeItem f _ _) = gitPath f

instance GitPath LsTree.TreeItem where
	gitPath = gitPath . LsTree.file

instance GitPath TreeContent where
	gitPath (TreeBlob f _ _) = gitPath f
	gitPath (RecordedSubTree f _ _) = gitPath f
	gitPath (NewSubTree f _) = gitPath f
	gitPath (TreeCommit f _ _) = gitPath f

inTopTree :: GitPath t => t -> Bool
inTopTree = inTree topTreePath

topTreePath :: OsPath
topTreePath = literalOsPath "."

inTree :: (GitPath t, GitPath f) => t -> f -> Bool
inTree t f = gitPath t == takeDirectory (gitPath f)

beneathSubTree :: (GitPath t, GitPath f) => t -> f -> Bool
beneathSubTree t f = subTreePrefix t `OS.isPrefixOf` subTreePath f

subTreePath :: GitPath t => t -> OsPath
subTreePath = normalise . gitPath

subTreePrefix :: GitPath t => t -> OsPath
subTreePrefix t
	| OS.null tp = tp
	| otherwise = addTrailingPathSeparator (normalise tp)
  where
	tp = gitPath t

{- Makes a Map where the keys are directories, and the values
 - are the items located in that directory.
 -
 - Values that are not in any subdirectory are placed in
 - the topTreePath key.
 -}
mkPathMap :: GitPath t => [t] -> M.Map OsPath [t]
mkPathMap l = M.fromListWith (++) $
	map (\ti -> (takeDirectory (gitPath ti), [ti])) l

{- Input is eg splitDirectories "foo/bar/baz",
 - for which it will output ["foo", "foo/bar", "foo/bar/baz"] -}
pathPrefixes :: [OsPath] -> [OsPath]
pathPrefixes = go []
  where
	go _ [] = []
	go base (d:rest) = (joinPath base </> d) : go (base ++ [d]) rest

{- Makes a Map where the keys are all subtree path prefixes, 
 - and the values are items with that subtree path prefix.
 -}
mkSubTreePathPrefixMap :: GitPath t => [t] -> M.Map OsPath [t]
mkSubTreePathPrefixMap l = M.fromListWith (++) $ concatMap go l
  where
	go ti = map (\p -> (p, [ti]))
		(map subTreePrefix $ pathPrefixes $ splitDirectories $ subTreePath ti)
