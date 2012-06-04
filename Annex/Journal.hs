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
import Annex.Perms

{- Records content for a file in the branch to the journal.
 -
 - Using the journal, rather than immediatly staging content to the index
 - avoids git needing to rewrite the index after every change. -}
setJournalFile :: FilePath -> String -> Annex ()
setJournalFile file content = do
	createAnnexDirectory =<< fromRepo gitAnnexJournalDir
	createAnnexDirectory =<< fromRepo gitAnnexTmpDir
	-- journal file is written atomically
	jfile <- fromRepo $ journalFile file
	tmp <- fromRepo gitAnnexTmpDir
	let tmpfile = tmp </> takeFileName jfile
	liftIO $ do
		writeBinaryFile tmpfile content
		moveFile tmpfile jfile

{- Gets any journalled content for a file in the branch. -}
getJournalFile :: FilePath -> Annex (Maybe String)
getJournalFile file = inRepo $ \g -> catchMaybeIO $
	readFileStrict $ journalFile file g

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
journalFile :: FilePath -> Git.Repo -> FilePath
journalFile file repo = gitAnnexJournalDir repo </> concatMap mangle file
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
	createAnnexDirectory $ takeDirectory file
	mode <- annexFileMode
	bracketIO (lock file mode) unlock a
	where
		lock file mode = do
			l <- noUmask mode $ createFile file mode
			waitToSetLock l (WriteLock, AbsoluteSeek, 0, 0)
			return l
		unlock = closeFd
