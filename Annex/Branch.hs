{- management of the git-annex branch
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Annex.Branch (
	create,
	update,
	get,
	change,
	commit,
	files,
	refExists,
	hasOrigin,
	hasSomeBranch,
	name	
) where

import System.IO.Binary
import System.Exit
import qualified Data.ByteString.Lazy.Char8 as L

import Common.Annex
import Annex.Exception
import Types.BranchState
import qualified Git
import qualified Git.UnionMerge
import qualified Annex
import Annex.CatFile

type GitRef = String

{- Name of the branch that is used to store git-annex's information. -}
name :: GitRef
name = "git-annex"

{- Fully qualified name of the branch. -}
fullname :: GitRef
fullname = "refs/heads/" ++ name

{- Branch's name in origin. -}
originname :: GitRef
originname = "origin/" ++ name

{- A separate index file for the branch. -}
index :: Git.Repo -> FilePath
index g = gitAnnexDir g </> "index"

{- Populates the branch's index file with the current branch contents.
 - 
 - Usually, this is only done when the index doesn't yet exist, and
 - the index is used to build up changes to be commited to the branch,
 - and merge in changes from other branches.
 -}
genIndex :: Git.Repo -> IO ()
genIndex g = Git.UnionMerge.ls_tree g fullname >>= Git.UnionMerge.update_index g

{- Runs an action using the branch's index file. -}
withIndex :: Annex a -> Annex a
withIndex = withIndex' False
withIndex' :: Bool -> Annex a -> Annex a
withIndex' bootstrapping a = do
	g <- gitRepo
	let f = index g

	bracketIO (Git.useIndex f) id $ do
		unlessM (liftIO $ doesFileExist f) $ do
			unless bootstrapping create
			liftIO $ createDirectoryIfMissing True $ takeDirectory f
			unless bootstrapping $ liftIO $ genIndex g
		a

withIndexUpdate :: Annex a -> Annex a
withIndexUpdate a = update >> withIndex a

getState :: Annex BranchState
getState = Annex.getState Annex.branchstate

setState :: BranchState -> Annex ()
setState state = Annex.changeState $ \s -> s { Annex.branchstate = state }

setCache :: FilePath -> String -> Annex ()
setCache file content = do
	state <- getState
	setState state { cachedFile = Just file, cachedContent = content }

invalidateCache :: Annex ()
invalidateCache = do
	state <- getState
	setState state { cachedFile = Nothing, cachedContent = "" }

getCache :: FilePath -> Annex (Maybe String)
getCache file = getState >>= go
	where
		go state
			| cachedFile state == Just file =
				return $ Just $ cachedContent state
			| otherwise = return Nothing

{- Creates the branch, if it does not already exist. -}
create :: Annex ()
create = unlessM hasBranch $ do
	g <- gitRepo
	e <- hasOrigin
	if e
		then liftIO $ Git.run g "branch" [Param name, Param originname]
		else withIndex' True $
			liftIO $ Git.commit g "branch created" fullname []

{- Stages the journal, and commits staged changes to the branch. -}
commit :: String -> Annex ()
commit message = do
	fs <- getJournalFiles
	when (not $ null fs) $ lockJournal $ do
		stageJournalFiles fs
		g <- gitRepo
		withIndex $ liftIO $ Git.commit g message fullname [fullname]

{- Ensures that the branch is up-to-date; should be called before
 - data is read from it. Runs only once per git-annex run. -}
update :: Annex ()
update = do
	state <- getState
	unless (branchUpdated state) $ do
		-- check what needs updating before taking the lock
		fs <- getJournalFiles
		refs <- filterM checkref =<< siblingBranches
		unless (null fs && null refs) $ withIndex $ lockJournal $ do
			{- Before refs are merged into the index, it's
			 - important to first stage the journal into the
			 - index. Otherwise, any changes in the journal
			 - would later get staged, and might overwrite
			 - changes made during the merge.
			 -
			 - It would be cleaner to handle the merge by
			 - updating the journal, not the index, with changes
			 - from the branches.
			 -}
			unless (null fs) $ stageJournalFiles fs
			mapM_ mergeref refs
			g <- gitRepo
			liftIO $ Git.commit g "update" fullname (fullname:refs)
			Annex.changeState $ \s -> s { Annex.branchstate = state { branchUpdated = True } }
			invalidateCache
	where
		checkref ref = do
			g <- gitRepo
			-- checking with log to see if there have been changes
			-- is less expensive than always merging
			diffs <- liftIO $ Git.pipeRead g [
				Param "log",
				Param (name++".."++ref),
				Params "--oneline -n1"
				]
			return $ not $ L.null diffs
		mergeref ref = do
			showSideAction $ "merging " ++
				Git.refDescribe ref ++ " into " ++ name
			{- By passing only one ref, it is actually
			 - merged into the index, preserving any
			 - changes that may already be staged.
			 -
			 - However, any changes in the git-annex
			 - branch that are *not* reflected in the
			 - index will be removed. So, documentation
			 - advises users not to directly modify the
			 - branch.
			 -}
			g <- gitRepo
			liftIO $ Git.UnionMerge.merge g [ref]
			return $ Just ref

{- Checks if a git ref exists. -}
refExists :: GitRef -> Annex Bool
refExists ref = do
	g <- gitRepo
	liftIO $ Git.runBool g "show-ref"
		[Param "--verify", Param "-q", Param ref]

{- Does the main git-annex branch exist? -}
hasBranch :: Annex Bool
hasBranch = refExists fullname

{- Does origin/git-annex exist? -}
hasOrigin :: Annex Bool
hasOrigin = refExists originname

{- Does the git-annex branch or a foo/git-annex branch exist? -}
hasSomeBranch :: Annex Bool
hasSomeBranch = not . null <$> siblingBranches

{- List of all git-annex branches, including the main one and any
 - from remotes. -}
siblingBranches :: Annex [String]
siblingBranches = do
	g <- gitRepo
	r <- liftIO $ Git.pipeRead g [Param "show-ref", Param name]
	return $ map (last . words . L.unpack) (L.lines r)

{- Applies a function to modifiy the content of a file. -}
change :: FilePath -> (String -> String) -> Annex ()
change file a = lockJournal $ get file >>= return . a >>= set file

{- Records new content of a file into the journal. -}
set :: FilePath -> String -> Annex ()
set file content = do
	setJournalFile file content
	setCache file content

{- Gets the content of a file on the branch, or content from the journal, or
 - staged in the index.
 -
 - Returns an empty string if the file doesn't exist yet. -}
get :: FilePath -> Annex String
get file = do
	cached <- getCache file
	case cached of
		Just content -> return content
		Nothing -> do
			j <- getJournalFile file
			case j of
				Just content -> do
					setCache file content
					return content
				Nothing -> withIndexUpdate $ do
					content <- catFile fullname file
					setCache file content
					return content

{- Lists all files on the branch. There may be duplicates in the list. -}
files :: Annex [FilePath]
files = withIndexUpdate $ do
	g <- gitRepo
	bfiles <- liftIO $ Git.pipeNullSplit g
		[Params "ls-tree --name-only -r -z", Param fullname]
	jfiles <- getJournalledFiles
	return $ jfiles ++ bfiles

{- Records content for a file in the branch to the journal.
 -
 - Using the journal, rather than immediatly staging content to the index
 - avoids git needing to rewrite the index after every change. -}
setJournalFile :: FilePath -> String -> Annex ()
setJournalFile file content = do
	g <- gitRepo
	liftIO $ catch (write g) $ const $ do
		createDirectoryIfMissing True $ gitAnnexJournalDir g
		createDirectoryIfMissing True $ gitAnnexTmpDir g
		write g
	where
		-- journal file is written atomically
		write g = do
			let jfile = journalFile g file
			let tmpfile = gitAnnexTmpDir g </> takeFileName jfile
			writeBinaryFile tmpfile content
			renameFile tmpfile jfile

{- Gets any journalled content for a file in the branch. -}
getJournalFile :: FilePath -> Annex (Maybe String)
getJournalFile file = do
	g <- gitRepo
	liftIO $ catch (liftM Just . readFileStrict $ journalFile g file)
		(const $ return Nothing)

{- List of files that have updated content in the journal. -}
getJournalledFiles :: Annex [FilePath]
getJournalledFiles = map fileJournal <$> getJournalFiles

{- List of existing journal files. -}
getJournalFiles :: Annex [FilePath]
getJournalFiles = do
	g <- gitRepo
	fs <- liftIO $ catch (getDirectoryContents $ gitAnnexJournalDir g)
		(const $ return [])
	return $ filter (`notElem` [".", ".."]) fs

{- Stages the specified journalfiles. -}
stageJournalFiles :: [FilePath] -> Annex ()
stageJournalFiles fs = do
	g <- gitRepo
	withIndex $ liftIO $ do
		let dir = gitAnnexJournalDir g
		let paths = map (dir </>) fs
		-- inject all the journal files directly into git
		-- in one quick command
		(pid, fromh, toh) <- hPipeBoth "git" $ toCommand $
			Git.gitCommandLine g [Param "hash-object", Param "-w", Param "--stdin-paths"]
		_ <- forkProcess $ do
			hPutStr toh $ unlines paths
			hClose toh
			exitSuccess
		hClose toh
		s <- hGetContents fromh
		-- update the index, also in just one command
		Git.UnionMerge.update_index g $
			index_lines (lines s) $ map fileJournal fs
		hClose fromh
		forceSuccess pid
		mapM_ removeFile paths
	where
		index_lines shas = map genline . zip shas
		genline (sha, file) = Git.UnionMerge.update_index_line sha file

{- Produces a filename to use in the journal for a file on the branch.
 -
 - The journal typically won't have a lot of files in it, so the hashing
 - used in the branch is not necessary, and all the files are put directly
 - in the journal directory.
 -}
journalFile :: Git.Repo -> FilePath -> FilePath
journalFile repo file = gitAnnexJournalDir repo </> concatMap mangle file
	where
		mangle '/' = "_"
		mangle '_' = "__"
		mangle c = [c]

{- Converts a journal file (relative to the journal dir) back to the
 - filename on the branch. -}
fileJournal :: FilePath -> FilePath
fileJournal = replace "//" "_" . replace "_" "/"

{- Runs an action that modifies the journal, using locking to avoid
 - contention with other git-annex processes. -}
lockJournal :: Annex a -> Annex a
lockJournal a = do
	g <- gitRepo
	let file = gitAnnexJournalLock g
	bracketIO (lock file) unlock a
	where
		lock file = do
			l <- createFile file stdFileMode
			waitToSetLock l (WriteLock, AbsoluteSeek, 0, 0)
			return l
		unlock = closeFd
