{- management of the git-annex journal
 -
 - The journal is used to queue up changes before they are committed to the
 - git-annex branch. Among other things, it ensures that if git-annex is
 - interrupted, its recorded data is not lost.
 -
 - Copyright 2011-2019 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Annex.Journal where

import Annex.Common
import qualified Git
import Annex.Perms
import Annex.Tmp
import Annex.LockFile
import Utility.Directory.Stream

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as S
import qualified System.FilePath.ByteString as P
import Data.ByteString.Builder
import Data.Char

class Journalable t where
	writeJournalHandle :: Handle -> t -> IO ()
	journalableByteString :: t -> L.ByteString

instance Journalable L.ByteString where
	writeJournalHandle = L.hPut
	journalableByteString = id

-- This is more efficient than the ByteString instance.
instance Journalable Builder where
	writeJournalHandle = hPutBuilder
	journalableByteString = toLazyByteString

{- Records content for a file in the branch to the journal.
 -
 - Using the journal, rather than immediatly staging content to the index
 - avoids git needing to rewrite the index after every change.
 - 
 - The file in the journal is updated atomically, which allows
 - getJournalFileStale to always return a consistent journal file
 - content, although possibly not the most current one.
 -}
setJournalFile :: Journalable content => JournalLocked -> RawFilePath -> content -> Annex ()
setJournalFile _jl file content = withOtherTmp $ \tmp -> do
	createAnnexDirectory =<< fromRepo gitAnnexJournalDir
	-- journal file is written atomically
	jfile <- fromRawFilePath <$> fromRepo (journalFile file)
	let tmpfile = tmp </> takeFileName jfile
	liftIO $ do
		withFile tmpfile WriteMode $ \h -> writeJournalHandle h content
		moveFile tmpfile jfile

{- Gets any journalled content for a file in the branch. -}
getJournalFile :: JournalLocked -> RawFilePath -> Annex (Maybe L.ByteString)
getJournalFile _jl = getJournalFileStale

{- Without locking, this is not guaranteed to be the most recent
 - version of the file in the journal, so should not be used as a basis for
 - changes.
 -
 - The file is read strictly so that its content can safely be fed into
 - an operation that modifies the file. While setJournalFile doesn't
 - write directly to journal files and so probably avoids problems with
 - writing to the same file that's being read, but there could be
 - concurrency or other issues with a lazy read, and the minor loss of
 - laziness doesn't matter much, as the files are not very large.
 -}
getJournalFileStale :: RawFilePath -> Annex (Maybe L.ByteString)
getJournalFileStale file = inRepo $ \g -> catchMaybeIO $
	L.fromStrict <$> S.readFile (fromRawFilePath $ journalFile file g)

{- List of existing journal files, but without locking, may miss new ones
 - just being added, or may have false positives if the journal is staged
 - as it is run. -}
getJournalledFilesStale :: Annex [FilePath]
getJournalledFilesStale = do
	g <- gitRepo
	fs <- liftIO $ catchDefaultIO [] $
		getDirectoryContents $ gitAnnexJournalDir g
	return $ filter (`notElem` [".", ".."]) $
		map (fromRawFilePath . fileJournal . toRawFilePath) fs

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
 - The input filename is assumed to not contain any '_' character,
 - since path separators are replaced with that.
 -
 - The journal typically won't have a lot of files in it, so the hashing
 - used in the branch is not necessary, and all the files are put directly
 - in the journal directory.
 -}
journalFile :: RawFilePath -> Git.Repo -> RawFilePath
journalFile file repo = gitAnnexJournalDir' repo P.</> S.map mangle file
  where
	mangle c
		| P.isPathSeparator c = fromIntegral (ord '_')
		| otherwise = c

{- Converts a journal file (relative to the journal dir) back to the
 - filename on the branch. -}
fileJournal :: RawFilePath -> RawFilePath
fileJournal = S.map unmangle
  where
	unmangle c
		| c == fromIntegral (ord '_') = P.pathSeparator
		| otherwise = c

{- Sentinal value, only produced by lockJournal; required
 - as a parameter by things that need to ensure the journal is
 - locked. -}
data JournalLocked = ProduceJournalLocked

{- Runs an action that modifies the journal, using locking to avoid
 - contention with other git-annex processes. -}
lockJournal :: (JournalLocked -> Annex a) -> Annex a
lockJournal a = withExclusiveLock gitAnnexJournalLock $ a ProduceJournalLocked
