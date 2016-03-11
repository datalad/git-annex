{- git diff-tree interface
 -
 - Copyright 2012 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Git.DiffTree (
	DiffTreeItem(..),
	isDiffOf,
	diffTree,
	diffTreeRecursive,
	diffIndex,
	diffWorkTree,
	diffFiles,
	diffLog,
	commitDiff,
) where

import Numeric

import Common
import Git
import Git.Sha
import Git.Command
import Git.FilePath
import Git.DiffTreeItem
import qualified Git.Filename
import qualified Git.Ref

{- Checks if the DiffTreeItem modifies a file with a given name
 - or under a directory by that name. -}
isDiffOf :: DiffTreeItem -> TopFilePath -> Bool
isDiffOf diff f = case getTopFilePath f of
	"" -> True -- top of repo contains all
	d -> d `dirContains` getTopFilePath (file diff)

{- Diffs two tree Refs. -}
diffTree :: Ref -> Ref -> Repo -> IO ([DiffTreeItem], IO Bool)
diffTree src dst = getdiff (Param "diff-tree")
	[Param (fromRef src), Param (fromRef dst), Param "--"]

{- Diffs two tree Refs, recursing into sub-trees -}
diffTreeRecursive :: Ref -> Ref -> Repo -> IO ([DiffTreeItem], IO Bool)
diffTreeRecursive src dst = getdiff (Param "diff-tree")
	[Param "-r", Param (fromRef src), Param (fromRef dst), Param "--"]

{- Diffs between a tree and the index. Does nothing if there is not yet a
 - commit in the repository. -}
diffIndex :: Ref -> Repo -> IO ([DiffTreeItem], IO Bool)
diffIndex ref = diffIndex' ref [Param "--cached"]

{- Diffs between a tree and the working tree. Does nothing if there is not
 - yet a commit in the repository, or if the repository is bare. -}
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
			( params ++ [Param $ fromRef ref] ++ [Param "--"] )
			repo
		, return ([], return True)
		)

{- Diff between the index and work tree. -}
diffFiles :: [CommandParam] -> Repo -> IO ([DiffTreeItem], IO Bool)
diffFiles = getdiff (Param "diff-files")

{- Runs git log in --raw mode to get the changes that were made in
 - a particular commit to particular files. The output format
 - is adjusted to be the same as diff-tree --raw._-}
diffLog :: [CommandParam] -> Repo -> IO ([DiffTreeItem], IO Bool)
diffLog params = getdiff (Param "log")
	(Param "-n1" : Param "--abbrev=40" : Param "--pretty=format:" : params)

{- Uses git show to get the changes made by a commit.
 -
 - Does not support merge commits, and will fail on them. -}
commitDiff :: Sha -> Repo -> IO ([DiffTreeItem], IO Bool)
commitDiff ref = getdiff (Param "show")
	[ Param "--abbrev=40", Param "--pretty=", Param "--raw", Param (fromRef ref) ]

getdiff :: CommandParam -> [CommandParam] -> Repo -> IO ([DiffTreeItem], IO Bool)
getdiff command params repo = do
	(diff, cleanup) <- pipeNullSplit ps repo
	return (fromMaybe (error $ "git " ++ show (toCommand ps) ++ " parse failed") (parseDiffRaw diff), cleanup)
  where
	ps = 
		command :
		Param "-z" :
		Param "--raw" :
		Param "--no-renames" :
		Param "-l0" :
		params

{- Parses --raw output used by diff-tree and git-log. -}
parseDiffRaw :: [String] -> Maybe [DiffTreeItem]
parseDiffRaw l = go l []
  where
	go [] c = Just c
	go (info:f:rest) c = case mk info f of
		Nothing -> Nothing
		Just i -> go rest (i:c)
	go (s:[]) _ = Nothing

	mk info f = DiffTreeItem
		<$> readmode srcm
		<*> readmode dstm
		<*> extractSha ssha
		<*> extractSha dsha
		<*> pure s
		<*> pure (asTopFilePath $ fromInternalGitPath $ Git.Filename.decode f)
	  where
		readmode = fst <$$> headMaybe . readOct

		-- info = :<srcmode> SP <dstmode> SP <srcsha> SP <dstsha> SP <status>
		-- All fields are fixed, so we can pull them out of
		-- specific positions in the line.
		(srcm, past_srcm) = splitAt 7 $ drop 1 info
		(dstm, past_dstm) = splitAt 7 past_srcm
		(ssha, past_ssha) = splitAt shaSize past_dstm
		(dsha, past_dsha) = splitAt shaSize $ drop 1 past_ssha
		s = drop 1 past_dsha
