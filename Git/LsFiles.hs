{- git ls-files interface
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Git.LsFiles (
	inRepo,
	notInRepo,
	staged,
	stagedNotDeleted,
	changedUnstaged,
	typeChanged,
	typeChangedStaged,
) where

import Git
import Utility

{- Scans for files that are checked into git at the specified locations. -}
inRepo :: Repo -> [FilePath] -> IO [FilePath]
inRepo repo l = pipeNullSplit repo $
	Params "ls-files --cached -z --" : map File l

{- Scans for files at the specified locations that are not checked into
 - git. -}
notInRepo :: Repo -> Bool -> [FilePath] -> IO [FilePath]
notInRepo repo include_ignored l =
	pipeNullSplit repo $ [Params "ls-files --others"]++exclude++[Params "-z --"] ++ map File l
	where
		exclude = if include_ignored then [] else [Param "--exclude-standard"]

{- Returns a list of all files that are staged for commit. -}
staged :: Repo -> [FilePath] -> IO [FilePath]
staged repo l = staged' repo l []

{- Returns a list of the files, staged for commit, that are being added,
 - moved, or changed (but not deleted), from the specified locations. -}
stagedNotDeleted :: Repo -> [FilePath] -> IO [FilePath]
stagedNotDeleted repo l = staged' repo l [Param "--diff-filter=ACMRT"]

staged' :: Repo -> [FilePath] -> [CommandParam] -> IO [FilePath]
staged' repo l middle = pipeNullSplit repo $ start ++ middle ++ end
	where
		start = [Params "diff --cached --name-only -z"]
		end = Param "--" : map File l

{- Returns a list of files that have unstaged changes. -}
changedUnstaged :: Repo -> [FilePath] -> IO [FilePath]
changedUnstaged repo l = pipeNullSplit repo $
	Params "diff --name-only -z --" : map File l

{- Returns a list of the files in the specified locations that are staged
 - for commit, and whose type has changed. -}
typeChangedStaged :: Repo -> [FilePath] -> IO [FilePath]
typeChangedStaged repo l = typeChanged' repo l [Param "--cached"]

{- Returns a list of the files in the specified locations whose type has
 - changed.  Files only staged for commit will not be included. -}
typeChanged :: Repo -> [FilePath] -> IO [FilePath]
typeChanged repo l = typeChanged' repo l []

typeChanged' :: Repo -> [FilePath] -> [CommandParam] -> IO [FilePath]
typeChanged' repo l middle = pipeNullSplit repo $ start ++ middle ++ end
	where
		start = [Params "diff --name-only --diff-filter=T -z"]
		end = Param "--" : map File l
