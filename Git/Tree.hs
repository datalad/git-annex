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

{- Records a Tree in the Repo, returning its Sha.
 - 
 - Efficiently handles subtrees, by only recording ones that have not
 - already been recorded before. And even when many subtrees need to be
 - recorded, it's done with a single call to git mktree, using its batch
 - interface.
 -}
recordTree :: Tree -> Repo -> IO Sha
recordTree t repo = do
	h <- startRecordTree repo
	sha <- recordTree' h t
	CoProcess.stop h
	return sha

startRecordTree :: Repo -> IO CoProcess.CoProcessHandle
startRecordTree repo = CoProcess.rawMode =<< gitCoProcessStart False ps repo
  where
	ps = [Param "mktree", Param "--batch", Param "-z"]

recordTree' :: CoProcess.CoProcessHandle -> Tree -> IO Sha
recordTree' h (Tree l) = mkTree h =<< mapM (recordSubTree h) l

{- Note that the returned RecordedSubTree does not have its [TreeContent]
 - list populated. This is a memory optimisation, since the list is not
 - used. -}
recordSubTree :: CoProcess.CoProcessHandle -> TreeContent -> IO TreeContent
recordSubTree h (NewSubTree d l) = do
	sha <- mkTree h =<< mapM (recordSubTree h) l
	return (RecordedSubTree d sha [])
recordSubTree _ alreadyrecorded = return alreadyrecorded
 
mkTree :: CoProcess.CoProcessHandle -> [TreeContent] -> IO Sha
mkTree cp l = CoProcess.query cp send receive
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
	deriving (Eq)

{- Applies an adjustment to items in a tree.
 -
 - While less flexible than using getTree and recordTree, this avoids
 - buffering the whole tree in memory.
 -}
adjustTree :: MonadIO m => (TreeItem -> m (Maybe TreeItem)) -> Ref -> Repo -> m Sha
adjustTree adjust r repo = do
	(l, cleanup) <- liftIO $ lsTreeWithObjects r repo
	h <- liftIO $ startRecordTree repo
	(l', _, _) <- go h False [] topTree l
	sha <- liftIO $ mkTree h l'
	liftIO $ CoProcess.stop h
	void $ liftIO cleanup
	return sha
  where
	go _ wasmodified c _ [] = return (c, wasmodified, [])
	go h wasmodified c intree (i:is)
		| intree i =
			case readObjectType (LsTree.typeobj i) of
				Just BlobObject -> do
					let ti = TreeItem (LsTree.file i) (LsTree.mode i) (LsTree.sha i)
					v <- adjust ti
					case v of
						Nothing -> go h True c intree is
						Just ti'@(TreeItem f m s) ->
							let !modified = wasmodified || ti' /= ti
							    blob = TreeBlob f m s
							in go h modified (blob:c) intree is
				Just TreeObject -> do
					(sl, modified, is') <- go h False [] (subTree i) is
					subtree <- if modified
						then liftIO $ recordSubTree h $ NewSubTree (LsTree.file i) sl
						else return $ RecordedSubTree (LsTree.file i) (LsTree.sha i) [] 
					let !modified' = modified || wasmodified
					go h modified' (subtree : c) intree is'
				_ -> error ("unexpected object type \"" ++ LsTree.typeobj i ++ "\"")
		| otherwise = return (c, wasmodified, i:is)

{- Assumes the list is ordered, with tree objects coming right before their
 - contents. -}
extractTree :: [LsTree.TreeItem] -> Either String Tree
extractTree l = case go [] topTree l of
	Right (t, []) -> Right (Tree t)
	Right _ -> parseerr "unexpected tree form"
	Left e -> parseerr e
  where
	go t _ [] = Right (t, [])
	go t intree (i:is)
		| intree i = 
			case readObjectType (LsTree.typeobj i) of
				Just BlobObject ->
					let b = TreeBlob (LsTree.file i) (LsTree.mode i) (LsTree.sha i)
					in go (b:t) intree is
				Just TreeObject -> case go [] (subTree i) is of
					Right (subtree, is') ->
						let st = RecordedSubTree (LsTree.file i) (LsTree.sha i) subtree
						in go (st:t) intree is'
					Left e -> Left e
				_ -> parseerr ("unexpected object type \"" ++ LsTree.typeobj i ++ "\"")
		| otherwise = Right (t, i:is)
	parseerr = Left

type InTree = LsTree.TreeItem -> Bool

topTree :: InTree
topTree = notElem '/' . getTopFilePath . LsTree.file

subTree :: LsTree.TreeItem -> InTree
subTree t =
	let prefix = getTopFilePath (LsTree.file t) ++ "/"
	in (\i -> prefix `isPrefixOf` getTopFilePath (LsTree.file i))
