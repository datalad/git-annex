{- git diff-tree interface
 -
 - Copyright 2012-2020 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
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
	parseDiffRaw,
) where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Attoparsec.ByteString.Lazy as A
import qualified Data.Attoparsec.ByteString.Char8 as A8

import Common
import Git
import Git.Sha
import Git.Command
import Git.FilePath
import Git.DiffTreeItem
import qualified Git.Quote
import qualified Git.Ref
import Utility.Attoparsec

{- Checks if the DiffTreeItem modifies a file with a given name
 - or under a directory by that name. -}
isDiffOf :: DiffTreeItem -> TopFilePath -> Bool
isDiffOf diff f = 
	let f' = getTopFilePath f
	in if B.null f'
		then True -- top of repo contains all
		else f' `dirContains` getTopFilePath (file diff)

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
	(Param "-n1" : Param "--no-abbrev" : Param "--pretty=format:" : params)

{- Uses git show to get the changes made by a commit.
 -
 - Does not support merge commits, and will fail on them. -}
commitDiff :: Sha -> Repo -> IO ([DiffTreeItem], IO Bool)
commitDiff ref = getdiff (Param "show")
	[ Param "--no-abbrev", Param "--pretty=", Param "--raw", Param (fromRef ref) ]

getdiff :: CommandParam -> [CommandParam] -> Repo -> IO ([DiffTreeItem], IO Bool)
getdiff command params repo = do
	(diff, cleanup) <- pipeNullSplit ps repo
	return (parseDiffRaw diff, cleanup)
  where
	ps = 
		command :
		Param "-z" :
		Param "--raw" :
		Param "--no-renames" :
		Param "-l0" :
		params

{- Parses --raw output used by diff-tree and git-log. -}
parseDiffRaw :: [L.ByteString] -> [DiffTreeItem]
parseDiffRaw l = go l
  where
	go [] = []
	go (info:f:rest) = case A.parse (parserDiffRaw (L.toStrict f)) info of
		A.Done _ r -> r : go rest
		A.Fail _ _ err -> giveup $ "diff-tree parse error: " ++ err
	go (s:[]) = giveup $ "diff-tree parse error near \"" ++ decodeBL s ++ "\""

-- :<srcmode> SP <dstmode> SP <srcsha> SP <dstsha> SP <status>
--
-- May be prefixed with a newline, which git log --pretty=format
-- adds to the first line of the diff, even with -z.
parserDiffRaw :: RawFilePath -> A.Parser DiffTreeItem
parserDiffRaw f = DiffTreeItem
	<$ A.option '\n' (A8.char '\n')
	<* A8.char ':'
	<*> octal
	<* A8.char ' '
	<*> octal
	<* A8.char ' '
	<*> (maybe (fail "bad srcsha") return . extractSha =<< nextword)
	<* A8.char ' '
	<*> (maybe (fail "bad dstsha") return . extractSha =<< nextword)
	<* A8.char ' '
	<*> A.takeByteString
	<*> pure (asTopFilePath $ fromInternalGitPath $ Git.Quote.unquote f)
  where
	nextword = A8.takeTill (== ' ')
