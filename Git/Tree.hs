{- git trees
 -
 - Copyright 2016 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Git.Tree (
	Tree(..),
	TreeContent(..),
	getTree,
	recordTree,
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
getTree :: Ref -> Repo -> IO (Tree, IO Bool)
getTree r repo = do
	-- Pass -t to get the tree object shas, which are normally omitted.
	(l, cleanup) <- LsTree.lsTree' [Param "-t"] r repo
	let t = either (\e -> error ("ls-tree parse error:" ++ e)) id (extractTree l)
	return (t, cleanup)

{- Assumes the list is ordered, with tree objects coming right before their
 - contents. -}
extractTree :: [LsTree.TreeItem] -> Either String Tree
extractTree l = case go [] "" l of
	Right (t, []) -> Right (Tree t)
	Right _ -> parseerr "unexpected tree form"
	Left e -> parseerr e
  where
	go t _ [] = Right (t, [])
	go t prefix (i:is)
		| prefix `isPrefixOf` getTopFilePath (LsTree.file i) = 
			case readObjectType (LsTree.typeobj i) of
				Just BlobObject ->
					let b = TreeBlob (LsTree.file i) (LsTree.mode i) (LsTree.sha i)
					in go (b:t) prefix is
				Just TreeObject -> case go [] (getTopFilePath (LsTree.file i) ++ "/") is of
					Right (subtree, is') ->
						let st = RecordedSubTree (LsTree.file i) (LsTree.sha i) subtree
						in go (st:t) prefix is'
					Left e -> Left e
				_ -> parseerr ("unexpected object type \"" ++ LsTree.typeobj i ++ "\"")
		| otherwise = Right (t, i:is)
	parseerr = Left

{- Records a Tree in the Repo, returning its Sha.
 - 
 - Efficiently handles subtrees, by only recording ones that have not
 - already been recorded before. And even when many subtrees need to be
 - recorded, it's done with a single call to git mktree, using its batch
 - interface.
 -}
recordTree :: Repo -> Tree -> IO Sha
recordTree repo t = do
	h <- CoProcess.rawMode =<< gitCoProcessStart False ps repo
	sha <- recordTree' h t
	CoProcess.stop h
	return sha
  where
	ps = [Param "mktree", Param "--batch", Param "-z"]

recordTree' :: CoProcess.CoProcessHandle -> Tree -> IO Sha
recordTree' h (Tree l) = mkTree h =<< mapM (recordSubTree h) l

recordSubTree :: CoProcess.CoProcessHandle -> TreeContent -> IO TreeContent
recordSubTree h (NewSubTree d l) = do
	l' <- mapM (recordSubTree h) l
	sha <- mkTree h l'
	return (RecordedSubTree d sha l')
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
