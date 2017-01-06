{- management of the git-annex journal
 -
 - The journal is used to queue up changes before they are committed to the
 - git-annex branch. Among other things, it ensures that if git-annex is
 - interrupted, its recorded data is not lost.
 -
 - Copyright 2011-2013 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Annex.Journal where

import Annex.Common
import qualified Git
import Annex.Perms
import Annex.LockFile

{- Records content for a file in the branch to the journal.
 -
 - Using the journal, rather than immediatly staging content to the index
 - avoids git needing to rewrite the index after every change.
 - 
 - The file in the journal is updated atomically, which allows
 - getJournalFileStale to always return a consistent journal file
 - content, although possibly not the most current one.
 -}
setJournalFile :: JournalLocked -> FilePath -> String -> Annex ()
setJournalFile _jl file content = do
	tmp <- fromRepo gitAnnexTmpMiscDir
	createAnnexDirectory =<< fromRepo gitAnnexJournalDir
	createAnnexDirectory tmp
	-- journal file is written atomically
	jfile <- fromRepo $ journalFile file
	let tmpfile = tmp </> takeFileName jfile
	liftIO $ do
		withFile tmpfile WriteMode $ \h -> do
#ifdef mingw32_HOST_OS
			hSetNewlineMode h noNewlineTranslation
#endif
			hPutStr h content
		moveFile tmpfile jfile

{- Gets any journalled content for a file in the branch. -}
getJournalFile :: JournalLocked -> FilePath -> Annex (Maybe String)
getJournalFile _jl = getJournalFileStale

{- Without locking, this is not guaranteed to be the most recent
 - version of the file in the journal, so should not be used as a basis for
 - changes. -}
getJournalFileStale :: FilePath -> Annex (Maybe String)
getJournalFileStale file = inRepo $ \g -> catchMaybeIO $
	readFileStrict $ journalFile file g

{- List of files that have updated content in the journal. -}
getJournalledFiles :: JournalLocked -> Annex [FilePath]
getJournalledFiles jl = map fileJournal <$> getJournalFiles jl

getJournalledFilesStale :: Annex [FilePath]
getJournalledFilesStale = map fileJournal <$> getJournalFilesStale

{- List of existing journal files. -}
getJournalFiles :: JournalLocked -> Annex [FilePath]
getJournalFiles _jl = getJournalFilesStale

{- List of existing journal files, but without locking, may miss new ones
 - just being added, or may have false positives if the journal is staged
 - as it is run. -}
getJournalFilesStale :: Annex [FilePath]
getJournalFilesStale = do
	g <- gitRepo
	fs <- liftIO $ catchDefaultIO [] $
		getDirectoryContents $ gitAnnexJournalDir g
	return $ filter (`notElem` [".", ".."]) fs

withJournalHandle :: (DirectoryHandle -> IO a) -> Annex a
withJournalHandle a = do
	d <- fromRepo gitAnnexJournalDir
	bracketIO (openDirectory d) closeDirectory (liftIO . a)

{- Checks if there are changes in the journal. -}
journalDirty :: Annex Bool
journalDirty = do
	d <- fromRepo gitAnnexJournalDir
	liftIO $ 
		(not <$> isDirectoryEmpty d)
			`catchIO` (const $ doesDirectoryExist d)

{- Produces a filename to use in the journal for a file on the branch.
 -
 - The journal typically won't have a lot of files in it, so the hashing
 - used in the branch is not necessary, and all the files are put directly
 - in the journal directory.
 -}
journalFile :: FilePath -> Git.Repo -> FilePath
journalFile file repo = gitAnnexJournalDir repo </> concatMap mangle file
  where
	mangle c
		| c == pathSeparator = "_"
		| c == '_' = "__"
		| otherwise = [c]

{- Converts a journal file (relative to the journal dir) back to the
 - filename on the branch. -}
fileJournal :: FilePath -> FilePath
fileJournal = replace [pathSeparator, pathSeparator] "_" .
	replace "_" [pathSeparator]

{- Sentinal value, only produced by lockJournal; required
 - as a parameter by things that need to ensure the journal is
 - locked. -}
data JournalLocked = ProduceJournalLocked

{- Runs an action that modifies the journal, using locking to avoid
 - contention with other git-annex processes. -}
lockJournal :: (JournalLocked -> Annex a) -> Annex a
lockJournal a = withExclusiveLock gitAnnexJournalLock $ a ProduceJournalLocked
