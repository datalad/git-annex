{- git diff-tree interface
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Git.DiffTree (
	DiffTreeItem(..),
	diffTree,
	diffTreeRecursive,
	parseDiffTree
) where

import Numeric
import System.Posix.Types

import Common
import Git
import Git.Sha
import Git.Command
import qualified Git.Filename

data DiffTreeItem = DiffTreeItem
	{ srcmode :: FileMode
	, dstmode :: FileMode
	, srcsha :: Sha -- nullSha if file was added
	, dstsha :: Sha -- nullSha if file was deleted
	, status :: String
	, file :: FilePath
	} deriving Show

{- Diffs two tree Refs. -}
diffTree :: Ref -> Ref -> Repo -> IO ([DiffTreeItem], IO Bool)
diffTree = diffTree' []

{- Diffs two tree Refs, recursing into sub-trees -}
diffTreeRecursive :: Ref -> Ref -> Repo -> IO ([DiffTreeItem], IO Bool)
diffTreeRecursive = diffTree' [Param "-r"]

diffTree' :: [CommandParam] -> Ref -> Ref -> Repo -> IO ([DiffTreeItem], IO Bool)
diffTree' params src dst repo = do
	(diff, cleanup) <- pipeNullSplit ps repo
	return (parseDiffTree diff, cleanup)
  where
	ps = Params "diff-tree -z --raw --no-renames -l0" : params ++
		[Param (show src), Param (show dst)]

{- Parses diff-tree output. -}
parseDiffTree :: [String] -> [DiffTreeItem]
parseDiffTree l = go l []
  where
	go [] c = c
	go (info:f:rest) c = go rest (mk info f : c)
	go (s:[]) _ = error $ "diff-tree parse error " ++ s

	mk info f = DiffTreeItem 
		{ srcmode = readmode srcm
		, dstmode = readmode dstm
		, srcsha = fromMaybe (error "bad srcsha") $ extractSha ssha
		, dstsha = fromMaybe (error "bad dstsha") $ extractSha dsha
		, status = s
		, file = Git.Filename.decode f
		}
	  where
		readmode = fst . Prelude.head . readOct

		-- info = :<srcmode> SP <dstmode> SP <srcsha> SP <dstsha> SP <status>
		-- All fields are fixed, so we can pull them out of
		-- specific positions in the line.
		(srcm, past_srcm) = splitAt 7 $ drop 1 info
		(dstm, past_dstm) = splitAt 7 past_srcm
		(ssha, past_ssha) = splitAt shaSize past_dstm
		(dsha, past_dsha) = splitAt shaSize $ drop 1 past_ssha
		s = drop 1 past_dsha
