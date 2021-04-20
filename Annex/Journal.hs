{- management of the git-annex journal
 -
 - The journal is used to queue up changes before they are committed to the
 - git-annex branch. Among other things, it ensures that if git-annex is
 - interrupted, its recorded data is not lost.
 -
 - Copyright 2011-2021 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

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

{- When a file in the git-annex branch is changed, this indicates what
 - repository UUID (or in some cases, UUIDs) a change is regarding.
 -
 - Using this lets changes regarding private UUIDs be written to the
 - private index, rather than to the main branch index, so it does
 - not get exposed to other remotes.
 -}
data RegardingUUID = RegardingUUID [UUID]

regardingPrivateUUID :: RegardingUUID -> Bool
regardingPrivateUUID _ = False -- TODO

-- Are any private UUIDs known to exist? If so, extra work has to be done,
-- to check for information separately recorded for them, outside the usual
-- locations.
privateUUIDsKnown :: Bool
privateUUIDsKnown = False -- TODO

{- Records content for a file in the branch to the journal.
 -
 - Using the journal, rather than immediatly staging content to the index
 - avoids git needing to rewrite the index after every change.
 - 
 - The file in the journal is updated atomically, which allows
 - getJournalFileStale to always return a consistent journal file
 - content, although possibly not the most current one.
 -}
setJournalFile :: Journalable content => JournalLocked -> RegardingUUID -> RawFilePath -> content -> Annex ()
setJournalFile _jl ru file content = withOtherTmp $ \tmp -> do
	jd <- fromRepo $ if regardingPrivateUUID ru
		then gitAnnexPrivateJournalDir
		else gitAnnexJournalDir
	createAnnexDirectory jd
	-- journal file is written atomically
	let jfile = journalFile file
	let tmpfile = fromRawFilePath (tmp P.</> jfile)
	liftIO $ do
		withFile tmpfile WriteMode $ \h -> writeJournalHandle h content
		moveFile tmpfile (fromRawFilePath (jd P.</> jfile))

{- Gets any journalled content for a file in the branch. -}
getJournalFile :: JournalLocked -> GetPrivate -> RawFilePath -> Annex (Maybe L.ByteString)
getJournalFile _jl = getJournalFileStale

data GetPrivate = GetPrivate Bool

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
getJournalFileStale :: GetPrivate -> RawFilePath -> Annex (Maybe L.ByteString)
getJournalFileStale (GetPrivate getprivate) file = inRepo $ \g -> 
	if getprivate
		then do
			x <- getfrom (gitAnnexJournalDir g)
			y <- getfrom (gitAnnexPrivateJournalDir g)
			-- This concacenation is the same as happens in a
			-- merge of two git-annex branches.
			return (x <> y)
		else getfrom (gitAnnexJournalDir g)
  where
	jfile = journalFile file
	getfrom d = catchMaybeIO $
		L.fromStrict <$> S.readFile (fromRawFilePath (d P.</> jfile))

{- List of existing journal files in a journal directory, but without locking,
 - may miss new ones just being added, or may have false positives if the
 - journal is staged as it is run. -}
getJournalledFilesStale :: (Git.Repo -> RawFilePath) -> Annex [RawFilePath]
getJournalledFilesStale getjournaldir = do
	g <- gitRepo
	fs <- liftIO $ catchDefaultIO [] $
		getDirectoryContents $ fromRawFilePath (getjournaldir g)
	return $ filter (`notElem` [".", ".."]) $
		map (fileJournal . toRawFilePath) fs

{- Directory handle open on a journal directory. -}
withJournalHandle :: (Git.Repo -> RawFilePath) -> (DirectoryHandle -> IO a) -> Annex a
withJournalHandle getjournaldir a = do
	d <- fromRawFilePath <$> fromRepo getjournaldir
	bracketIO (openDirectory d) closeDirectory (liftIO . a)

{- Checks if there are changes in the journal. -}
journalDirty :: (Git.Repo -> RawFilePath) -> Annex Bool
journalDirty getjournaldir = do
	d <- fromRawFilePath <$> fromRepo getjournaldir
	liftIO $ 
		(not <$> isDirectoryEmpty d)
			`catchIO` (const $ doesDirectoryExist d)

{- Produces a filename to use in the journal for a file on the branch.
 - The filename does not include the journal directory.
 -
 - The journal typically won't have a lot of files in it, so the hashing
 - used in the branch is not necessary, and all the files are put directly
 - in the journal directory.
 -}
journalFile :: RawFilePath -> RawFilePath
journalFile file = S.concatMap mangle file
  where
	mangle c
		| P.isPathSeparator c = S.singleton underscore
		| c == underscore = S.pack [underscore, underscore]
		| otherwise = S.singleton c
	underscore = fromIntegral (ord '_')

{- Converts a journal file (relative to the journal dir) back to the
 - filename on the branch. -}
fileJournal :: RawFilePath -> RawFilePath
fileJournal = go
  where
	go b = 
		let (h, t) = S.break (== underscore) b
		in h <> case S.uncons t of
			Nothing -> t
			Just (_u, t') -> case S.uncons t' of
				Nothing -> t'			
				Just (w, t'')
					| w == underscore ->
						S.cons underscore (go t'')
					| otherwise -> 
						S.cons P.pathSeparator (go t')
	
	underscore = fromIntegral (ord '_')

{- Sentinal value, only produced by lockJournal; required
 - as a parameter by things that need to ensure the journal is
 - locked. -}
data JournalLocked = ProduceJournalLocked

{- Runs an action that modifies the journal, using locking to avoid
 - contention with other git-annex processes. -}
lockJournal :: (JournalLocked -> Annex a) -> Annex a
lockJournal a = withExclusiveLock gitAnnexJournalLock $ a ProduceJournalLocked
