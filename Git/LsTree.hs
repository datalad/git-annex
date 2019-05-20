{- git ls-tree interface
 -
 - Copyright 2011-2019 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE BangPatterns #-}

module Git.LsTree (
	TreeItem(..),
	LsTreeMode(..),
	lsTree,
	lsTree',
	lsTreeParams,
	lsTreeFiles,
	parseLsTree,
	formatLsTree,
) where

import Common
import Git
import Git.Command
import Git.Sha
import Git.FilePath
import qualified Git.Filename

import Numeric
import Data.Char
import System.Posix.Types

data TreeItem = TreeItem
	{ mode :: FileMode
	, typeobj :: String
	, sha :: Ref
	, file :: TopFilePath
	} deriving Show

data LsTreeMode = LsTreeRecursive | LsTreeNonRecursive

{- Lists the contents of a tree, with lazy output. -}
lsTree :: LsTreeMode -> Ref -> Repo -> IO ([TreeItem], IO Bool)
lsTree = lsTree' []

lsTree' :: [CommandParam] -> LsTreeMode -> Ref -> Repo -> IO ([TreeItem], IO Bool)
lsTree' ps lsmode t repo = do
	(l, cleanup) <- pipeNullSplit (lsTreeParams lsmode t ps) repo
	return (map parseLsTree l, cleanup)

lsTreeParams :: LsTreeMode -> Ref -> [CommandParam] -> [CommandParam]
lsTreeParams lsmode r ps =
	[ Param "ls-tree"
	, Param "--full-tree"
	, Param "-z"
	] ++ recursiveparams ++ ps ++
	[ Param "--"
	, File $ fromRef r
	]
  where
	recursiveparams = case lsmode of
		LsTreeRecursive -> [ Param "-r" ]
		LsTreeNonRecursive -> []

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

{- Parses a line of ls-tree output, in format:
 - mode SP type SP sha TAB file
 -
 - (The --long format is not currently supported.) -}
parseLsTree :: String -> TreeItem
parseLsTree l = TreeItem 
	{ mode = smode
	, typeobj = t
	, sha = Ref s
	, file = sfile
	}
  where
	(m, past_m) = splitAt 7 l -- mode is 6 bytes
	(!t, past_t) = separate isSpace past_m
	(!s, past_s) = splitAt shaSize past_t
	!f = drop 1 past_s
	!smode = fst $ Prelude.head $ readOct m
	!sfile = asTopFilePath $ Git.Filename.decode f

{- Inverse of parseLsTree -}
formatLsTree :: TreeItem -> String
formatLsTree ti = unwords
	[ showOct (mode ti) ""
	, typeobj ti
	, fromRef (sha ti)
	, getTopFilePath (file ti)
	]
