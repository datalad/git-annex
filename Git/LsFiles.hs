{- git ls-files interface
 -
 - Copyright 2010,2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Git.LsFiles (
	inRepo,
	notInRepo,
	allFiles,
	deleted,
	modified,
	modifiedOthers,
	staged,
	stagedNotDeleted,
	stagedOthersDetails,
	stagedDetails,
	typeChanged,
	typeChangedStaged,
	Conflicting(..),
	Unmerged(..),
	unmerged,
	StagedDetails,
) where

import Common
import Git
import Git.Command
import Git.Types
import Git.Sha

import Numeric
import System.Posix.Types

{- Scans for files that are checked into git at the specified locations. -}
inRepo :: [FilePath] -> Repo -> IO ([FilePath], IO Bool)
inRepo l = pipeNullSplit $ Params "ls-files --cached -z --" : map File l

{- Scans for files at the specified locations that are not checked into git. -}
notInRepo :: Bool -> [FilePath] -> Repo -> IO ([FilePath], IO Bool)
notInRepo include_ignored l repo = pipeNullSplit params repo
  where
	params = [Params "ls-files --others"] ++ exclude ++
		[Params "-z --"] ++ map File l
	exclude
		| include_ignored = []
		| otherwise = [Param "--exclude-standard"]

{- Finds all files in the specified locations, whether checked into git or
 - not. -}
allFiles :: [FilePath] -> Repo -> IO ([FilePath], IO Bool)
allFiles l = pipeNullSplit $ Params "ls-files --cached --others -z --" : map File l

{- Returns a list of files in the specified locations that have been
 - deleted. -}
deleted :: [FilePath] -> Repo -> IO ([FilePath], IO Bool)
deleted l repo = pipeNullSplit params repo
  where
	params = [Params "ls-files --deleted -z --"] ++ map File l

{- Returns a list of files in the specified locations that have been
 - modified. -}
modified :: [FilePath] -> Repo -> IO ([FilePath], IO Bool)
modified l repo = pipeNullSplit params repo
  where
	params = [Params "ls-files --modified -z --"] ++ map File l

{- Files that have been modified or are not checked into git (and are not
 - ignored). -}
modifiedOthers :: [FilePath] -> Repo -> IO ([FilePath], IO Bool)
modifiedOthers l repo = pipeNullSplit params repo
  where
	params = [Params "ls-files --modified --others --exclude-standard -z --"] ++ map File l

{- Returns a list of all files that are staged for commit. -}
staged :: [FilePath] -> Repo -> IO ([FilePath], IO Bool)
staged = staged' []

{- Returns a list of the files, staged for commit, that are being added,
 - moved, or changed (but not deleted), from the specified locations. -}
stagedNotDeleted :: [FilePath] -> Repo -> IO ([FilePath], IO Bool)
stagedNotDeleted = staged' [Param "--diff-filter=ACMRT"]

staged' :: [CommandParam] -> [FilePath] -> Repo -> IO ([FilePath], IO Bool)
staged' ps l = pipeNullSplit $ prefix ++ ps ++ suffix
  where
	prefix = [Params "diff --cached --name-only -z"]
	suffix = Param "--" : map File l

type StagedDetails = (FilePath, Maybe Sha, Maybe FileMode)

{- Returns details about files that are staged in the index,
 - as well as files not yet in git. Skips ignored files. -}
stagedOthersDetails :: [FilePath] -> Repo -> IO ([StagedDetails], IO Bool)
stagedOthersDetails = stagedDetails' [Params "--others --exclude-standard"]

{- Returns details about all files that are staged in the index. -}
stagedDetails :: [FilePath] -> Repo -> IO ([StagedDetails], IO Bool)
stagedDetails = stagedDetails' []

{- Gets details about staged files, including the Sha of their staged
 - contents. -}
stagedDetails' :: [CommandParam] -> [FilePath] -> Repo -> IO ([StagedDetails], IO Bool)
stagedDetails' ps l repo = do
	(ls, cleanup) <- pipeNullSplit params repo
	return (map parse ls, cleanup)
  where
	params = Params "ls-files --stage -z" : ps ++ 
		Param "--" : map File l
	parse s
		| null file = (s, Nothing, Nothing)
		| otherwise = (file, extractSha $ take shaSize rest, readmode mode)
	  where
		(metadata, file) = separate (== '\t') s
		(mode, rest) = separate (== ' ') metadata
		readmode = fst <$$> headMaybe . readOct

{- Returns a list of the files in the specified locations that are staged
 - for commit, and whose type has changed. -}
typeChangedStaged :: [FilePath] -> Repo -> IO ([FilePath], IO Bool)
typeChangedStaged = typeChanged' [Param "--cached"]

{- Returns a list of the files in the specified locations whose type has
 - changed.  Files only staged for commit will not be included. -}
typeChanged :: [FilePath] -> Repo -> IO ([FilePath], IO Bool)
typeChanged = typeChanged' []

typeChanged' :: [CommandParam] -> [FilePath] -> Repo -> IO ([FilePath], IO Bool)
typeChanged' ps l repo = do
	(fs, cleanup) <- pipeNullSplit (prefix ++ ps ++ suffix) repo
	-- git diff returns filenames relative to the top of the git repo;
	-- convert to filenames relative to the cwd, like git ls-files.
	let top = repoPath repo
	cwd <- getCurrentDirectory
	return (map (\f -> relPathDirToFile cwd $ top </> f) fs, cleanup)
  where
	prefix = [Params "diff --name-only --diff-filter=T -z"]
	suffix = Param "--" : (if null l then [File "."] else map File l)

{- A item in conflict has two possible values.
 - Either can be Nothing, when that side deleted the file. -}
data Conflicting v = Conflicting
	{ valUs :: Maybe v
	, valThem :: Maybe v
	} deriving (Show)

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
 - If a line is omitted, that side removed the file.
 -}
unmerged :: [FilePath] -> Repo -> IO ([Unmerged], IO Bool)
unmerged l repo = do
	(fs, cleanup) <- pipeNullSplit params repo
	return (reduceUnmerged [] $ catMaybes $ map parseUnmerged fs, cleanup)
  where
	params = Params "ls-files --unmerged -z --" : map File l

data InternalUnmerged = InternalUnmerged
	{ isus :: Bool
	, ifile :: FilePath
	, iblobtype :: Maybe BlobType
	, isha :: Maybe Sha
	} deriving (Show)

parseUnmerged :: String -> Maybe InternalUnmerged
parseUnmerged s
	| null file = Nothing
	| otherwise = case words metadata of
		(rawblobtype:rawsha:rawstage:_) -> do
			stage <- readish rawstage :: Maybe Int
			unless (stage == 2 || stage == 3) $
				fail undefined -- skip stage 1
			blobtype <- readBlobType rawblobtype
			sha <- extractSha rawsha
			return $ InternalUnmerged (stage == 2) file
				(Just blobtype) (Just sha)
		_ -> Nothing
  where
	(metadata, file) = separate (== '\t') s

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
	findsib templatei [] = ([], removed templatei)
	findsib templatei (l:ls)
		| ifile l == ifile templatei = (ls, l)
		| otherwise = (l:ls, removed templatei)
	removed templatei = templatei
		{ isus = not (isus templatei)
		, iblobtype = Nothing
		, isha = Nothing
		}
