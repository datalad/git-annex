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

import Common
import Git
import Git.Command

{- Scans for files that are checked into git at the specified locations. -}
inRepo :: [FilePath] -> Repo -> IO [FilePath]
inRepo l = pipeNullSplit $ Params "ls-files --cached -z --" : map File l

{- Scans for files at the specified locations that are not checked into git. -}
notInRepo :: Bool -> [FilePath] -> Repo -> IO [FilePath]
notInRepo include_ignored l repo = pipeNullSplit params repo
	where
		params = [Params "ls-files --others"] ++ exclude ++
			[Params "-z --"] ++ map File l
		exclude
			| include_ignored = []
			| otherwise = [Param "--exclude-standard"]

{- Returns a list of all files that are staged for commit. -}
staged :: [FilePath] -> Repo -> IO [FilePath]
staged = staged' []

{- Returns a list of the files, staged for commit, that are being added,
 - moved, or changed (but not deleted), from the specified locations. -}
stagedNotDeleted :: [FilePath] -> Repo -> IO [FilePath]
stagedNotDeleted = staged' [Param "--diff-filter=ACMRT"]

staged' :: [CommandParam] -> [FilePath] -> Repo -> IO [FilePath]
staged' ps l = pipeNullSplit $ prefix ++ ps ++ suffix
	where
		prefix = [Params "diff --cached --name-only -z"]
		suffix = Param "--" : map File l

{- Returns a list of files that have unstaged changes. -}
changedUnstaged :: [FilePath] -> Repo -> IO [FilePath]
changedUnstaged l = pipeNullSplit params
	where
		params = Params "diff --name-only -z --" : map File l

{- Returns a list of the files in the specified locations that are staged
 - for commit, and whose type has changed. -}
typeChangedStaged :: [FilePath] -> Repo -> IO [FilePath]
typeChangedStaged = typeChanged' [Param "--cached"]

{- Returns a list of the files in the specified locations whose type has
 - changed.  Files only staged for commit will not be included. -}
typeChanged :: [FilePath] -> Repo -> IO [FilePath]
typeChanged = typeChanged' []

typeChanged' :: [CommandParam] -> [FilePath] -> Repo -> IO [FilePath]
typeChanged' ps l repo = do
	fs <- pipeNullSplit (prefix ++ ps ++ suffix) repo
	-- git diff returns filenames relative to the top of the git repo;
	-- convert to filenames relative to the cwd, like git ls-files.
	let top = workTree repo
	cwd <- getCurrentDirectory
	return $ map (\f -> relPathDirToFile cwd $ top </> f) fs
	where
		prefix = [Params "diff --name-only --diff-filter=T -z"]
		suffix = Param "--" : map File l
