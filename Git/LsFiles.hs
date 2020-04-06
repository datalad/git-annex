{- git ls-files interface
 -
 - Copyright 2010-2019 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Git.LsFiles (
	inRepo,
	inRepoOrBranch,
	notInRepo,
	notInRepoIncludingEmptyDirectories,
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
	inodeCaches,
) where

import Common
import Git
import Git.Command
import Git.Types
import Git.Sha
import Utility.InodeCache
import Utility.TimeStamp

import Numeric
import System.Posix.Types
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as L

{- It's only safe to use git ls-files on the current repo, not on a remote.
 -
 - Git has some strange behavior when git ls-files is used with repos
 - that are not the one that the cwd is in:
 - git --git-dir=../foo/.git --worktree=../foo ../foo fails saying 
 - "../foo is outside repository".
 - That does not happen when an absolute path is provided.
 -
 - Also, the files output by ls-files are relative to the cwd. 
 - Unless it's run on remote. Then it's relative to the top of the remote
 - repo.
 -
 - So, best to avoid that class of problems.
 -}
safeForLsFiles :: Repo -> Bool
safeForLsFiles r = isNothing (remoteName r)

guardSafeForLsFiles :: Repo -> IO a -> IO a
guardSafeForLsFiles r a
	| safeForLsFiles r = a
	| otherwise = error $ "git ls-files is unsafe to run on repository " ++ repoDescribe r

{- Lists files that are checked into git's index at the specified paths.
 - With no paths, all files are listed.
 -}
inRepo :: [RawFilePath] -> Repo -> IO ([RawFilePath], IO Bool)
inRepo = inRepo' [] 

inRepo' :: [CommandParam] -> [RawFilePath] -> Repo -> IO ([RawFilePath], IO Bool)
inRepo' ps l repo = guardSafeForLsFiles repo $ pipeNullSplit' params repo
  where
	params = 
		Param "ls-files" :
		Param "--cached" :
		Param "-z" :
		ps ++
		(Param "--" : map (File . fromRawFilePath) l)

{- Files that are checked into the index or have been committed to a
 - branch. -}
inRepoOrBranch :: Branch -> [RawFilePath] -> Repo -> IO ([RawFilePath], IO Bool)
inRepoOrBranch (Ref b) = inRepo' [Param $ "--with-tree=" ++ b]

{- Scans for files at the specified locations that are not checked into git. -}
notInRepo :: Bool -> [RawFilePath] -> Repo -> IO ([RawFilePath], IO Bool)
notInRepo = notInRepo' []

notInRepo' :: [CommandParam] -> Bool -> [RawFilePath] -> Repo -> IO ([RawFilePath], IO Bool)
notInRepo' ps include_ignored l repo = guardSafeForLsFiles repo $
	pipeNullSplit' params repo
  where
	params = concat
		[ [ Param "ls-files", Param "--others"]
		, ps
		, exclude
		, [ Param "-z", Param "--" ]
		, map (File . fromRawFilePath) l
		]
	exclude
		| include_ignored = []
		| otherwise = [Param "--exclude-standard"]

{- Scans for files at the specified locations that are not checked into
 - git. Empty directories are included in the result. -}
notInRepoIncludingEmptyDirectories :: Bool -> [RawFilePath] -> Repo -> IO ([RawFilePath], IO Bool)
notInRepoIncludingEmptyDirectories = notInRepo' [Param "--directory"]

{- Finds all files in the specified locations, whether checked into git or
 - not. -}
allFiles :: [RawFilePath] -> Repo -> IO ([RawFilePath], IO Bool)
allFiles l repo = guardSafeForLsFiles repo $ pipeNullSplit' params repo
  where
	params =
		Param "ls-files" :
		Param "--cached" :
		Param "--others" :
		Param "-z" :
		Param "--" :
		map (File . fromRawFilePath) l

{- Returns a list of files in the specified locations that have been
 - deleted. -}
deleted :: [RawFilePath] -> Repo -> IO ([RawFilePath], IO Bool)
deleted l repo = guardSafeForLsFiles repo $ pipeNullSplit' params repo
  where
	params =
		Param "ls-files" :
		Param "--deleted" :
		Param "-z" :
		Param "--" :
		map (File . fromRawFilePath) l

{- Returns a list of files in the specified locations that have been
 - modified. -}
modified :: [RawFilePath] -> Repo -> IO ([RawFilePath], IO Bool)
modified l repo = guardSafeForLsFiles repo $ pipeNullSplit' params repo
  where
	params = 
		Param "ls-files" :
		Param "--modified" :
		Param "-z" :
		Param "--" :
		map (File . fromRawFilePath) l

{- Files that have been modified or are not checked into git (and are not
 - ignored). -}
modifiedOthers :: [RawFilePath] -> Repo -> IO ([RawFilePath], IO Bool)
modifiedOthers l repo = guardSafeForLsFiles repo $ pipeNullSplit' params repo
  where
	params = 
		Param "ls-files" :
		Param "--modified" :
		Param "--others" :
		Param "--exclude-standard" :
		Param "-z" :
		Param "--" :
		map (File . fromRawFilePath) l

{- Returns a list of all files that are staged for commit. -}
staged :: [RawFilePath] -> Repo -> IO ([RawFilePath], IO Bool)
staged = staged' []

{- Returns a list of the files, staged for commit, that are being added,
 - moved, or changed (but not deleted), from the specified locations. -}
stagedNotDeleted :: [RawFilePath] -> Repo -> IO ([RawFilePath], IO Bool)
stagedNotDeleted = staged' [Param "--diff-filter=ACMRT"]

staged' :: [CommandParam] -> [RawFilePath] -> Repo -> IO ([RawFilePath], IO Bool)
staged' ps l repo = guardSafeForLsFiles repo $
	pipeNullSplit' (prefix ++ ps ++ suffix) repo
  where
	prefix = [Param "diff", Param "--cached", Param "--name-only", Param "-z"]
	suffix = Param "--" : map (File . fromRawFilePath) l

type StagedDetails = (RawFilePath, Maybe Sha, Maybe FileMode)

{- Returns details about files that are staged in the index,
 - as well as files not yet in git. Skips ignored files. -}
stagedOthersDetails :: [RawFilePath] -> Repo -> IO ([StagedDetails], IO Bool)
stagedOthersDetails = stagedDetails' [Param "--others", Param "--exclude-standard"]

{- Returns details about all files that are staged in the index. -}
stagedDetails :: [RawFilePath] -> Repo -> IO ([StagedDetails], IO Bool)
stagedDetails = stagedDetails' []

{- Gets details about staged files, including the Sha of their staged
 - contents. -}
stagedDetails' :: [CommandParam] -> [RawFilePath] -> Repo -> IO ([StagedDetails], IO Bool)
stagedDetails' ps l repo = guardSafeForLsFiles repo $ do
	(ls, cleanup) <- pipeNullSplit params repo
	return (map parseStagedDetails ls, cleanup)
  where
	params = Param "ls-files" : Param "--stage" : Param "-z" : ps ++ 
		Param "--" : map (File . fromRawFilePath) l

parseStagedDetails :: L.ByteString -> StagedDetails
parseStagedDetails s
	| null file = (L.toStrict s, Nothing, Nothing)
	| otherwise = (toRawFilePath file, extractSha sha, readmode mode)
  where
	(metadata, file) = separate (== '\t') (decodeBL' s)
	(mode, metadata') = separate (== ' ') metadata
	(sha, _) = separate (== ' ') metadata'
	readmode = fst <$$> headMaybe . readOct

{- Returns a list of the files in the specified locations that are staged
 - for commit, and whose type has changed. -}
typeChangedStaged :: [RawFilePath] -> Repo -> IO ([RawFilePath], IO Bool)
typeChangedStaged = typeChanged' [Param "--cached"]

{- Returns a list of the files in the specified locations whose type has
 - changed.  Files only staged for commit will not be included. -}
typeChanged :: [RawFilePath] -> Repo -> IO ([RawFilePath], IO Bool)
typeChanged = typeChanged' []

typeChanged' :: [CommandParam] -> [RawFilePath] -> Repo -> IO ([RawFilePath], IO Bool)
typeChanged' ps l repo = guardSafeForLsFiles repo $ do
	(fs, cleanup) <- pipeNullSplit (prefix ++ ps ++ suffix) repo
	-- git diff returns filenames relative to the top of the git repo;
	-- convert to filenames relative to the cwd, like git ls-files.
	top <- absPath (fromRawFilePath (repoPath repo))
	currdir <- getCurrentDirectory
	return (map (\f -> toRawFilePath (relPathDirToFileAbs currdir $ top </> decodeBL' f)) fs, cleanup)
  where
	prefix = 
		[ Param "diff"
		, Param "--name-only"
		, Param "--diff-filter=T"
		, Param "-z"
		]
	suffix = Param "--" : (if null l then [File "."] else map (File . fromRawFilePath) l)

{- A item in conflict has two possible values.
 - Either can be Nothing, when that side deleted the file. -}
data Conflicting v = Conflicting
	{ valUs :: Maybe v
	, valThem :: Maybe v
	} deriving (Show)

data Unmerged = Unmerged
	{ unmergedFile :: RawFilePath
	, unmergedTreeItemType :: Conflicting TreeItemType
	, unmergedSha :: Conflicting Sha
	}

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
unmerged :: [RawFilePath] -> Repo -> IO ([Unmerged], IO Bool)
unmerged l repo = guardSafeForLsFiles repo $ do
	(fs, cleanup) <- pipeNullSplit params repo
	return (reduceUnmerged [] $ catMaybes $ map (parseUnmerged . decodeBL') fs, cleanup)
  where
	params = 
		Param "ls-files" :
		Param "--unmerged" :
		Param "-z" :
		Param "--" :
		map (File . fromRawFilePath) l

data InternalUnmerged = InternalUnmerged
	{ isus :: Bool
	, ifile :: RawFilePath
	, itreeitemtype :: Maybe TreeItemType
	, isha :: Maybe Sha
	}

parseUnmerged :: String -> Maybe InternalUnmerged
parseUnmerged s
	| null file = Nothing
	| otherwise = case words metadata of
		(rawtreeitemtype:rawsha:rawstage:_) -> do
			stage <- readish rawstage :: Maybe Int
			if stage /= 2 && stage /= 3
				then Nothing
				else do
					treeitemtype <- readTreeItemType (encodeBS rawtreeitemtype)
					sha <- extractSha (encodeBS' rawsha)
					return $ InternalUnmerged (stage == 2) (toRawFilePath file)
						(Just treeitemtype) (Just sha)
		_ -> Nothing
  where
	(metadata, file) = separate (== '\t') s

reduceUnmerged :: [Unmerged] -> [InternalUnmerged] -> [Unmerged]
reduceUnmerged c [] = c
reduceUnmerged c (i:is) = reduceUnmerged (new:c) rest
  where
	(rest, sibi) = findsib i is
	(treeitemtypeA, treeitemtypeB, shaA, shaB)
		| isus i    = (itreeitemtype i, itreeitemtype sibi, isha i, isha sibi)
		| otherwise = (itreeitemtype sibi, itreeitemtype i, isha sibi, isha i)
	new = Unmerged
		{ unmergedFile = ifile i
		, unmergedTreeItemType = Conflicting treeitemtypeA treeitemtypeB
		, unmergedSha = Conflicting shaA shaB
		}
	findsib templatei [] = ([], removed templatei)
	findsib templatei (l:ls)
		| ifile l == ifile templatei = (ls, l)
		| otherwise = (l:ls, removed templatei)
	removed templatei = templatei
		{ isus = not (isus templatei)
		, itreeitemtype = Nothing
		, isha = Nothing
		}

{- Gets the InodeCache equivilant information stored in the git index.
 -
 - Note that this uses a --debug option whose output could change at some
 - point in the future. If the output is not as expected, will use Nothing.
 -}
inodeCaches :: [RawFilePath] -> Repo -> IO ([(FilePath, Maybe InodeCache)], IO Bool)
inodeCaches locs repo = guardSafeForLsFiles repo $ do
	(ls, cleanup) <- pipeNullSplit params repo
	return (parse Nothing (map decodeBL ls), cleanup)
  where
	params = 
		Param "ls-files" :
		Param "--cached" :
		Param "-z" :
		Param "--debug" :
		Param "--" :
		map (File . fromRawFilePath) locs
	
	parse Nothing (f:ls) = parse (Just f) ls
	parse (Just f) (s:[]) = 
		let i = parsedebug s
		in (f, i) : []
	parse (Just f) (s:ls) =
		let (d, f') = splitdebug s
		    i = parsedebug d
		in (f, i) : parse (Just f') ls
	parse _ _ = []

	-- First 5 lines are --debug output, remainder is the next filename.
	-- This assumes that --debug does not start outputting more lines.
	splitdebug s = case splitc '\n' s of
		(d1:d2:d3:d4:d5:rest) ->
			( intercalate "\n" [d1, d2, d3, d4, d5]
			, intercalate "\n" rest
			)
		_ -> ("", s)
	
	-- This parser allows for some changes to the --debug output,
	-- including reordering, or adding more items.
	parsedebug s = do
		let l = words s
		let iskey v = ":" `isSuffixOf` v
		let m = M.fromList $ zip
			(filter iskey l)
			(filter (not . iskey) l)
		mkInodeCache
			<$> (readish =<< M.lookup "ino:" m)
			<*> (readish =<< M.lookup "size:" m)
			<*> (parsePOSIXTime =<< (replace ":" "." <$> M.lookup "mtime:" m))
