{- management of the git-annex journal
 -
 - The journal is used to queue up changes before they are committed to the
 - git-annex branch. Among other things, it ensures that if git-annex is
 - interrupted, its recorded data is not lost.
 -
 - All files in the journal must be a series of lines separated by
 - newlines.
 -
 - Copyright 2011-2022 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE CPP #-}

module Annex.Journal where

import Annex.Common
import qualified Annex
import qualified Git
import Annex.Perms
import Annex.Tmp
import Annex.LockFile
import Utility.Directory.Stream
import qualified Utility.RawFilePath as R

import qualified Data.Set as S
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as B
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
 - Using this lets changes regarding private UUIDs be stored separately
 - from the git-annex branch, so its information does not get exposed
 - outside the repo.
 -}
data RegardingUUID = RegardingUUID [UUID]

regardingPrivateUUID :: RegardingUUID -> Annex Bool
regardingPrivateUUID (RegardingUUID []) = pure False
regardingPrivateUUID (RegardingUUID us) = do
	s <- annexPrivateRepos <$> Annex.getGitConfig
	return (any (flip S.member s) us)

{- Are any private UUIDs known to exist? If so, extra work has to be done,
 - to check for information separately recorded for them, outside the usual
 - locations.
 -}
privateUUIDsKnown :: Annex Bool
privateUUIDsKnown = privateUUIDsKnown' <$> Annex.getState id

privateUUIDsKnown' :: Annex.AnnexState -> Bool
privateUUIDsKnown' = not . S.null . annexPrivateRepos . Annex.gitconfig

{- Records content for a file in the branch to the journal.
 -
 - Using the journal, rather than immediately staging content to the index
 - avoids git needing to rewrite the index after every change.
 - 
 - The file in the journal is updated atomically. This avoids an
 - interrupted write truncating information that was earlier read from the
 - file, and so losing data.
 -}
setJournalFile :: Journalable content => JournalLocked -> RegardingUUID -> RawFilePath -> content -> Annex ()
setJournalFile _jl ru file content = withOtherTmp $ \tmp -> do
	jd <- fromRepo =<< ifM (regardingPrivateUUID ru)
		( return gitAnnexPrivateJournalDir
		, return gitAnnexJournalDir
		)
	-- journal file is written atomically
	let jfile = journalFile file
	let tmpfile = tmp P.</> jfile
	liftIO $ withFile (fromRawFilePath tmpfile) WriteMode $ \h ->
		writeJournalHandle h content
	let mv = liftIO $ moveFile tmpfile (jd P.</> jfile)
	-- avoid overhead of creating the journal directory when it already
	-- exists
	mv `catchIO` (const (createAnnexDirectory jd >> mv))

newtype AppendableJournalFile = AppendableJournalFile (RawFilePath, RawFilePath)

{- If the journal file does not exist, it cannot be appended to, because
 - that would overwrite whatever content the file has in the git-annex
 - branch. -}
checkCanAppendJournalFile :: JournalLocked -> RegardingUUID -> RawFilePath -> Annex (Maybe AppendableJournalFile)
checkCanAppendJournalFile _jl ru file = do
	jd <- fromRepo =<< ifM (regardingPrivateUUID ru)
		( return gitAnnexPrivateJournalDir
		, return gitAnnexJournalDir
		)
	let jfile = jd P.</> journalFile file
	ifM (liftIO $ R.doesPathExist jfile)
		( return (Just (AppendableJournalFile (jd, jfile)))
		, return Nothing
		)

{- Appends content to an existing journal file.
 -
 - Appends are not necessarily atomic, though short appends often are.
 - So, when this is interrupted, it can leave only part of the content
 - written to the file. To deal with that situation, both this and
 - getJournalFileStale check if the file ends with a newline, and if
 - not discard the incomplete line.
 -
 - Due to the lack of atomicity, this should not be used when multiple
 - lines need to be written to the file as an atomic unit.
 -}
appendJournalFile :: Journalable content => JournalLocked -> AppendableJournalFile -> content -> Annex ()
appendJournalFile _jl (AppendableJournalFile (jd, jfile)) content = do
	let write = liftIO $ withFile (fromRawFilePath jfile) ReadWriteMode $ \h -> do
		sz <- hFileSize h
		when (sz /= 0) $ do
			hSeek h SeekFromEnd (-1)
			lastchar <- B.hGet h 1
			unless (lastchar == "\n") $ do
				hSeek h AbsoluteSeek 0
				goodpart <- L.length . discardIncompleteAppend
					<$> L.hGet h (fromIntegral sz)
				hSetFileSize h (fromIntegral goodpart)
				hSeek h SeekFromEnd 0
		writeJournalHandle h content
	write `catchIO` (const (createAnnexDirectory jd >> write))

data JournalledContent
	= NoJournalledContent
	| JournalledContent L.ByteString
	| PossiblyStaleJournalledContent L.ByteString
	-- ^ This is used when the journalled content may have been 
	-- supersceded by content in the git-annex branch. The returned
	-- content should be combined with content from the git-annex branch.
	-- This is particularly the case when a file is in the private
	-- journal, which does not get written to the git-annex branch,
	-- and so the git-annex branch can contain changes to non-private
	-- information that were made after that journal file was written.

{- Gets any journalled content for a file in the branch. -}
getJournalFile :: JournalLocked -> GetPrivate -> RawFilePath -> Annex JournalledContent
getJournalFile _jl = getJournalFileStale

data GetPrivate = GetPrivate Bool

{- Without locking, this is not guaranteed to be the most recent
 - content of the file in the journal, so should not be used as a basis for
 - making changes to the file.
 -
 - The file is read strictly so that its content can safely be fed into
 - an operation that modifies the file (when getJournalFile calls this). 
 - The minor loss of laziness doesn't matter much, as the files are not
 - very large.
 -
 - To recover from an append of a line that is interrupted part way through
 - (or is in progress when this is called), if the file content does not end
 - with a newline, it is truncated back to the previous newline.
 -}
getJournalFileStale :: GetPrivate -> RawFilePath -> Annex JournalledContent
getJournalFileStale (GetPrivate getprivate) file = do
	-- Optimisation to avoid a second MVar access.
	st <- Annex.getState id
	let g = Annex.repo st
	liftIO $
		if getprivate && privateUUIDsKnown' st
		then do
			x <- getfrom (gitAnnexJournalDir g)
			getfrom (gitAnnexPrivateJournalDir g) >>= \case
				Nothing -> return $ case x of
					Nothing -> NoJournalledContent
					Just b -> JournalledContent b
				Just y -> return $ PossiblyStaleJournalledContent $ case x of
					Nothing -> y
					-- This concacenation is the same as
					-- happens in a merge of two
					-- git-annex branches.
					Just x' -> x' <> y
		else getfrom (gitAnnexJournalDir g) >>= return . \case
			Nothing -> NoJournalledContent
			Just b -> JournalledContent b
  where
	jfile = journalFile file
	getfrom d = catchMaybeIO $
		discardIncompleteAppend . L.fromStrict
			<$> B.readFile (fromRawFilePath (d P.</> jfile))

-- Note that this forces read of the whole lazy bytestring.
discardIncompleteAppend :: L.ByteString -> L.ByteString
discardIncompleteAppend v
	| L.null v = v
	| L.last v == nl = v
	| otherwise = dropwhileend (/= nl) v
  where
	nl = fromIntegral (ord '\n')
#if MIN_VERSION_bytestring(0,11,2)
	dropwhileend = L.dropWhileEnd
#else
	dropwhileend p = L.reverse . L.dropWhile p . L.reverse
#endif

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
journalFile file = B.concatMap mangle file
  where
	mangle c
		| P.isPathSeparator c = B.singleton underscore
		| c == underscore = B.pack [underscore, underscore]
		| otherwise = B.singleton c
	underscore = fromIntegral (ord '_')

{- Converts a journal file (relative to the journal dir) back to the
 - filename on the branch. -}
fileJournal :: RawFilePath -> RawFilePath
fileJournal = go
  where
	go b = 
		let (h, t) = B.break (== underscore) b
		in h <> case B.uncons t of
			Nothing -> t
			Just (_u, t') -> case B.uncons t' of
				Nothing -> t'			
				Just (w, t'')
					| w == underscore ->
						B.cons underscore (go t'')
					| otherwise -> 
						B.cons P.pathSeparator (go t')
	
	underscore = fromIntegral (ord '_')

{- Sentinal value, only produced by lockJournal; required
 - as a parameter by things that need to ensure the journal is
 - locked. -}
data JournalLocked = ProduceJournalLocked

{- Runs an action that modifies the journal, using locking to avoid
 - contention with other git-annex processes. -}
lockJournal :: (JournalLocked -> Annex a) -> Annex a
lockJournal a = do
	lck <- fromRepo gitAnnexJournalLock
	withExclusiveLock lck $ a ProduceJournalLocked
