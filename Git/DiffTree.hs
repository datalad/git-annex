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
	diffIndex,
	diffWorkTree,
) where

import Numeric
import System.Posix.Types

import Common
import Git
import Git.Sha
import Git.Command
import Git.FilePath
import qualified Git.Filename
import qualified Git.Ref

data DiffTreeItem = DiffTreeItem
	{ srcmode :: FileMode
	, dstmode :: FileMode
	, srcsha :: Sha -- nullSha if file was added
	, dstsha :: Sha -- nullSha if file was deleted
	, status :: String
	, file :: TopFilePath
	} deriving Show

{- Diffs two tree Refs. -}
diffTree :: Ref -> Ref -> Repo -> IO ([DiffTreeItem], IO Bool)
diffTree src dst = getdiff (Param "diff-tree")
	[Param (show src), Param (show dst)]

{- Diffs two tree Refs, recursing into sub-trees -}
diffTreeRecursive :: Ref -> Ref -> Repo -> IO ([DiffTreeItem], IO Bool)
diffTreeRecursive src dst = getdiff (Param "diff-tree")
	[Param "-r", Param (show src), Param (show dst)]

{- Diffs between a tree and the index. Does nothing if there is not yet a
 - commit in the repository. -}
diffIndex :: Ref -> Repo -> IO ([DiffTreeItem], IO Bool)
diffIndex ref = diffIndex' ref [Param "--cached"]

{- Diffs between a tree and the working tree. Does nothing if there is not
 - yet a commit in the repository, of if the repository is bare. -}
diffWorkTree :: Ref -> Repo -> IO ([DiffTreeItem], IO Bool)
diffWorkTree ref repo =
	ifM (Git.Ref.headExists repo)
                ( diffIndex' ref [] repo
		, return ([], return True)
		)

diffIndex' :: Ref -> [CommandParam] -> Repo -> IO ([DiffTreeItem], IO Bool)
diffIndex' ref params repo =
	ifM (Git.Ref.headExists repo)
		( getdiff (Param "diff-index")
			( params ++ [Param $ show ref] )
			repo
		, return ([], return True)
		)

getdiff :: CommandParam -> [CommandParam] -> Repo -> IO ([DiffTreeItem], IO Bool)
getdiff command params repo = do
	(diff, cleanup) <- pipeNullSplit ps repo
	return (parseDiffTree diff, cleanup)
  where
	ps = command : Params "-z --raw --no-renames -l0" : params

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
		, file = asTopFilePath $ Git.Filename.decode f
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
