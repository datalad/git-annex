{- git trees
 -
 - Copyright 2016 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE BangPatterns #-}

module Git.Tree (
	Tree(..),
	TreeContent(..),
	getTree,
	recordTree,
	TreeItem(..),
	adjustTree,
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

newtype Tree = Tree [TreeContent]
	deriving (Show)

data TreeContent
	= TreeBlob TopFilePath FileMode Sha
	-- A subtree that is already recorded in git, with a known sha.
	| RecordedSubTree TopFilePath Sha [TreeContent]
	-- A subtree that has not yet been recorded in git.
	| NewSubTree TopFilePath [TreeContent]
	deriving (Show)

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
	setup = CoProcess.rawMode =<< gitCoProcessStart False ps repo
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
			RecordedSubTree f s _ -> mkTreeOutput 0o040000 TreeObject s f
			NewSubTree _ _ -> error "recordSubTree internal error; unexpected NewSubTree"
		hPutStr h "\NUL" -- signal end of tree to --batch
	receive h = getSha "mktree" (hGetLine h)

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

{- Applies an adjustment to items in a tree.
 - Can also add new items to the tree.
 -
 - While less flexible than using getTree and recordTree, this avoids
 - buffering the whole tree in memory.
 -}
adjustTree :: (MonadIO m, MonadMask m) => (TreeItem -> m (Maybe TreeItem)) -> [TreeItem] -> Ref -> Repo -> m Sha
adjustTree adjusttreeitem addtreeitems r repo = withMkTreeHandle repo $ \h -> do
	(l, cleanup) <- liftIO $ lsTreeWithObjects r repo
	(l', _, _) <- go h False [] inTopTree l
	sha <- liftIO $ mkTree h (map treeItemToTreeContent addedtotop ++ l')
	void $ liftIO cleanup
	return sha
  where
	go _ wasmodified c _ [] = return (c, wasmodified, [])
	go h wasmodified c intree (i:is)
		| intree i = case readObjectType (LsTree.typeobj i) of
			Just BlobObject -> do
				let ti = TreeItem (LsTree.file i) (LsTree.mode i) (LsTree.sha i)
				v <- adjusttreeitem ti
				case v of
					Nothing -> go h True c intree is
					Just ti'@(TreeItem f m s) ->
						let !modified = wasmodified || ti' /= ti
						    blob = TreeBlob f m s
						in go h modified (blob:c) intree is
			Just TreeObject -> do
				(sl, modified, is') <- go h False [] (beneathSubTree i) is
				let added = filter (inTree i) addtreeitems
				subtree <- if modified || not (null added)
					then liftIO $ recordSubTree h $ NewSubTree (LsTree.file i)
						(map treeItemToTreeContent added ++ sl)
					else return $ RecordedSubTree (LsTree.file i) (LsTree.sha i) [] 
				let !modified' = modified || wasmodified
				go h modified' (subtree : c) intree is'
			_ -> error ("unexpected object type \"" ++ LsTree.typeobj i ++ "\"")
		| otherwise = return (c, wasmodified, i:is)
	addedtotop = filter (\(TreeItem f _ _) -> inTopTree' f) addtreeitems

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
			_ -> parseerr ("unexpected object type \"" ++ LsTree.typeobj i ++ "\"")
		| otherwise = Right (t, i:is)
	parseerr = Left

type InTree = LsTree.TreeItem -> Bool

inTopTree :: InTree
inTopTree = inTopTree' . LsTree.file

inTopTree' :: TopFilePath -> Bool
inTopTree' f = takeDirectory (getTopFilePath f) == "."

beneathSubTree :: LsTree.TreeItem -> InTree
beneathSubTree t =
	let prefix = getTopFilePath (LsTree.file t) ++ "/"
	in (\i -> prefix `isPrefixOf` getTopFilePath (LsTree.file i))

inTree :: LsTree.TreeItem -> TreeItem -> Bool
inTree t (TreeItem f _ _) = takeDirectory (getTopFilePath f) == takeDirectory (getTopFilePath (LsTree.file t))
