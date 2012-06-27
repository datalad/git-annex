{- git ls-files interface
 -
 - Copyright 2010,2012 Joey Hess <joey@kitenet.net>
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
	Conflicting(..),
	Unmerged(..),
	unmerged,
) where

import Common
import Git
import Git.Command
import Git.Types
import Git.Sha

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
	let top = repoPath repo
	cwd <- getCurrentDirectory
	return $ map (\f -> relPathDirToFile cwd $ top </> f) fs
	where
		prefix = [Params "diff --name-only --diff-filter=T -z"]
		suffix = Param "--" : map File l

{- A item in conflict has two possible values.
 - Either can be Nothing, when that side deleted the file. -}
data Conflicting v = Conflicting
	{ valUs :: Maybe v
	, valThem :: Maybe v
	} deriving (Show)

isConflicting :: Eq a => Conflicting a -> Bool
isConflicting (Conflicting a b) = a /= b

data Unmerged = Unmerged
	{ unmergedFile :: FilePath
	, unmergedBlobType :: Conflicting BlobType
	, unmergedSha :: Conflicting Sha
	} deriving (Show)

{- Returns a list of the files in the specified locations that have
 - unresolved merge conflicts.
 -
 - ls-files outputs multiple lines per conflicting file, each with its own
 - stage number:
 -   1 = old version, can be ignored
 -   2 = us
 -   3 = them
 - If a line is omitted, that side deleted the file.
 -}
unmerged :: [FilePath] -> Repo -> IO [Unmerged]
unmerged l repo = reduceUnmerged [] . catMaybes . map parseUnmerged <$> list repo
	where
		files = map File l
		list = pipeNullSplit $ Params "ls-files --unmerged -z --" : files

data InternalUnmerged = InternalUnmerged
	{ isus :: Bool
	, ifile :: FilePath
	, iblobtype :: Maybe BlobType
	, isha :: Maybe Sha
	} deriving (Show)

parseUnmerged :: String -> Maybe InternalUnmerged
parseUnmerged s
	| null file || length ws < 3 = Nothing
	| otherwise = do
		stage <- readish (ws !! 2)
		unless (stage == 2 || stage == 3) $
			fail undefined -- skip stage 1
		blobtype <- readBlobType (ws !! 0)
		sha <- extractSha (ws !! 1)
		return $ InternalUnmerged (stage == 2) file (Just blobtype) (Just sha)
	where
		(metadata, file) = separate (== '\t') s
		ws = words metadata

reduceUnmerged :: [Unmerged] -> [InternalUnmerged] -> [Unmerged]
reduceUnmerged c [] = c
reduceUnmerged c (i:is) = reduceUnmerged (new:c) rest
	where
		(rest, sibi) = findsib i is
		(blobtypeA, blobtypeB, shaA, shaB)
			| isus i    = (iblobtype i, iblobtype sibi, isha i, isha sibi)
			| otherwise = (iblobtype sibi, iblobtype i, isha sibi, isha i)
		new = Unmerged
			{ unmergedFile = ifile i
			, unmergedBlobType = Conflicting blobtypeA blobtypeB
			, unmergedSha = Conflicting shaA shaB
			}
		findsib templatei [] = ([], deleted templatei)
		findsib templatei (i:is)
			| ifile i == ifile templatei = (is, i)
			| otherwise = (i:is, deleted templatei)
		deleted templatei = templatei
			{ isus = not (isus templatei)
			, iblobtype = Nothing
			, isha = Nothing
			}
