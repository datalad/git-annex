{- git trees
 -
 - Copyright 2016 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE BangPatterns, TypeSynonymInstances, FlexibleInstances #-}

module Git.Tree (
	Tree(..),
	TreeContent(..),
	getTree,
	recordTree,
	TreeItem(..),
	adjustTree,
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

import Numeric
import System.Posix.Types
import Control.Monad.IO.Class
import qualified Data.Set as S
import qualified Data.Map as M

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
getTree :: Ref -> Repo -> IO Tree
getTree r repo = do
	(l, cleanup) <- lsTreeWithObjects r repo
	let !t = either (\e -> error ("ls-tree parse error:" ++ e)) id
		(extractTree l)
	void cleanup
	return t

lsTreeWithObjects :: Ref -> Repo -> IO ([LsTree.TreeItem], IO Bool)
lsTreeWithObjects = LsTree.lsTree' [Param "-t"]

newtype MkTreeHandle = MkTreeHandle CoProcess.CoProcessHandle

withMkTreeHandle :: (MonadIO m, MonadMask m) => Repo -> (MkTreeHandle -> m a) -> m a
withMkTreeHandle repo a = bracketIO setup cleanup (a . MkTreeHandle)
  where
	setup = gitCoProcessStart False ps repo
	ps = [Param "mktree", Param "--batch", Param "-z"]
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
 
mkTree :: MkTreeHandle -> [TreeContent] -> IO Sha
mkTree (MkTreeHandle cp) l = CoProcess.query cp send receive
  where
	send h = do
		forM_ l $ \i -> hPutStr h $ case i of
			TreeBlob f fm s -> mkTreeOutput fm BlobObject s f
			RecordedSubTree f s _ -> mkTreeOutput treeMode TreeObject s f
			NewSubTree _ _ -> error "recordSubTree internal error; unexpected NewSubTree"
			TreeCommit f fm s -> mkTreeOutput fm CommitObject s f
		hPutStr h "\NUL" -- signal end of tree to --batch
	receive h = getSha "mktree" (hGetLine h)

treeMode :: FileMode
treeMode = 0o040000

mkTreeOutput :: FileMode -> ObjectType -> Sha -> TopFilePath -> String
mkTreeOutput fm ot s f = concat
	[ showOct fm ""
	, " "
	, show ot
	, " "
	, fromRef s
	, "\t"
	, takeFileName (getTopFilePath f)
	, "\NUL"
	]

data TreeItem = TreeItem TopFilePath FileMode Sha
	deriving (Show, Eq)

treeItemToTreeContent :: TreeItem -> TreeContent
treeItemToTreeContent (TreeItem f m s) = TreeBlob f m s

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
	-> [TopFilePath]
	-- ^ Files to remove from the tree.
	-> Ref
	-> Repo
	-> m Sha
adjustTree adjusttreeitem addtreeitems removefiles r repo =
	withMkTreeHandle repo $ \h -> do
		(l, cleanup) <- liftIO $ lsTreeWithObjects r repo
		(l', _, _) <- go h False [] 1 inTopTree l
		l'' <- adjustlist h 0 inTopTree (const True) l'
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
				sl' <- adjustlist h depth (inTree i) (beneathSubTree i) sl
				let slmodified = sl' /= sl
				subtree <- if modified || slmodified
					then liftIO $ recordSubTree h $ NewSubTree (LsTree.file i) sl'
					else return $ RecordedSubTree (LsTree.file i) (LsTree.sha i) [] 
				let !modified' = modified || slmodified || wasmodified
				go h modified' (subtree : c) depth intree is'
			Just CommitObject -> do
				let ti = TreeCommit (LsTree.file i) (LsTree.mode i)  (LsTree.sha i)
				go h wasmodified (ti:c) depth intree is
			_ -> error ("unexpected object type \"" ++ LsTree.typeobj i ++ "\"")
		| otherwise = return (c, wasmodified, i:is)

	adjustlist h depth ishere underhere l = do
		let (addhere, rest) = partition ishere addtreeitems
		let l' = filter (not . removed) $
			map treeItemToTreeContent addhere ++ l
		let inl i = any (\t -> beneathSubTree t i) l'
		let (Tree addunderhere) = flattenTree depth $ treeItemsToTree $
			filter (\i -> underhere i && not (inl i)) rest
		addunderhere' <- liftIO $ mapM (recordSubTree h) addunderhere
		return (addunderhere'++l')

	removeset = S.fromList $ map (normalise . gitPath) removefiles
	removed (TreeBlob f _ _) = S.member (normalise (gitPath f)) removeset
	removed _ = False

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
			_ -> parseerr ("unexpected object type \"" ++ LsTree.typeobj i ++ "\"")
		| otherwise = Right (t, i:is)
	parseerr = Left

class GitPath t where
	gitPath :: t -> FilePath

instance GitPath FilePath where
	gitPath = id

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
inTopTree = inTree "."

inTree :: (GitPath t, GitPath f) => t -> f -> Bool
inTree t f = gitPath t == takeDirectory (gitPath f)

beneathSubTree :: (GitPath t, GitPath f) => t -> f -> Bool
beneathSubTree t f = prefix `isPrefixOf` normalise (gitPath f)
  where
	tp = gitPath t
	prefix = if null tp then tp else addTrailingPathSeparator (normalise tp)
