{- management of the git-annex journal and cache
 -
 - The journal is used to queue up changes before they are committed to the
 - git-annex branch. Amoung other things, it ensures that if git-annex is
 - interrupted, its recorded data is not lost.
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Annex.Journal where

import System.IO.Binary

import Common.Annex
import Annex.Exception
import qualified Git

{- Records content for a file in the branch to the journal.
 -
 - Using the journal, rather than immediatly staging content to the index
 - avoids git needing to rewrite the index after every change. -}
setJournalFile :: FilePath -> String -> Annex ()
setJournalFile file content = do
	g <- gitRepo
	liftIO $ doRedo (write g) $ do
		createDirectoryIfMissing True $ gitAnnexJournalDir g
		createDirectoryIfMissing True $ gitAnnexTmpDir g
	where
		-- journal file is written atomically
		write g = do
			let jfile = journalFile g file
			let tmpfile = gitAnnexTmpDir g </> takeFileName jfile
			writeBinaryFile tmpfile content
			moveFile tmpfile jfile

{- Gets any journalled content for a file in the branch. -}
getJournalFile :: FilePath -> Annex (Maybe String)
getJournalFile file = inRepo $ \g -> catchMaybeIO $
	readFileStrict $ journalFile g file

{- List of files that have updated content in the journal. -}
getJournalledFiles :: Annex [FilePath]
getJournalledFiles = map fileJournal <$> getJournalFiles

{- List of existing journal files. -}
getJournalFiles :: Annex [FilePath]
getJournalFiles = do
	g <- gitRepo
	fs <- liftIO $
		catchDefaultIO (getDirectoryContents $ gitAnnexJournalDir g) []
	return $ filter (`notElem` [".", ".."]) fs

{- Checks if there are changes in the journal. -}
journalDirty :: Annex Bool
journalDirty = not . null <$> getJournalFiles

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
	file <- fromRepo gitAnnexJournalLock
	bracketIO (lock file) unlock a
	where
		lock file = do
			l <- doRedo (createFile file stdFileMode) $
				createDirectoryIfMissing True $ takeDirectory file
			waitToSetLock l (WriteLock, AbsoluteSeek, 0, 0)
			return l
		unlock = closeFd

{- Runs an action, catching failure and running something to fix it up, and
 - retrying if necessary. -}
doRedo :: IO a -> IO b -> IO a
doRedo a b = catchIO a $ const $ b >> a
