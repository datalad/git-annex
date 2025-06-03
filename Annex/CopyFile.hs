{- Copying files.
 -
 - Copyright 2011-2025 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Annex.CopyFile where

import Annex.Common
import qualified Annex
import Utility.Metered
import Utility.CopyFile
import Utility.FileMode
import Utility.Touch
import Utility.Hash (IncrementalVerifier(..))
import qualified Utility.FileIO as F
import qualified Utility.RawFilePath as R

import Control.Concurrent
import qualified Data.ByteString as S
import Data.Time.Clock.POSIX
import System.PosixCompat.Files (fileMode)

-- To avoid the overhead of trying copy-on-write every time, it's tried
-- once and if it fails, is not tried again.
newtype CopyCoWTried = CopyCoWTried (MVar Bool)

newCopyCoWTried :: IO CopyCoWTried
newCopyCoWTried = CopyCoWTried <$> newEmptyMVar

{- Copies a file if copy-on-write is supported. Otherwise, returns False.
 -
 - The destination file must not exist yet (or may exist but be empty), 
 - or it will fail to make a CoW copy, and will return false.
 -}
tryCopyCoW :: CopyCoWTried -> OsPath -> OsPath -> MeterUpdate -> IO Bool
tryCopyCoW (CopyCoWTried copycowtried) src dest meterupdate =
	-- If multiple threads reach this at the same time, they
	-- will both try CoW, which is acceptable.
	ifM (isEmptyMVar copycowtried)
		( ifM destfilealreadypopulated
			( return False
			, do
				ok <- docopycow
				void $ tryPutMVar copycowtried ok
				return ok
			)
		, ifM (readMVar copycowtried)
			( do
				-- CoW is known to work, so delete
				-- dest if it exists in order to do a fast
				-- CoW copy.
				void $ tryIO $ removeFile dest
				docopycow
			, return False
			)
		)
  where
	docopycow = watchFileSize dest meterupdate $ const $
		copyCoW CopyTimeStamps src dest

	-- Check if the dest file already exists, which would prevent
	-- probing CoW. If the file exists but is empty, there's no benefit
	-- to resuming from it when CoW does not work, so remove it.
	destfilealreadypopulated = 
		tryIO (R.getFileStatus (fromOsPath dest)) >>= \case
			Left _ -> return False
			Right st -> do
				sz <- getFileSize' dest st
				if sz == 0
					then tryIO (removeFile dest) >>= \case
						Right () -> return False
						Left _ -> return True
					else return True

data CopyMethod = CopiedCoW | Copied

-- Should cp be allowed to copy the file with --reflink=auto?
--
-- The benefit is that this lets it use the copy_file_range
-- syscall, which is not used with --reflink=always. The drawback is that
-- the IncrementalVerifier is not updated, so verification, if it is done,
-- will need to re-read the whole content of the file. And, interrupted
-- copies are not resumed but are restarted from the beginning.
-- 
-- Using this will result in CopiedCow being returned even in cases
-- where cp fell back to a slow copy.
newtype FastCopy = FastCopy Bool

getFastCopy :: RemoteGitConfig -> Annex FastCopy
getFastCopy gc = case remoteAnnexFastCopy gc of
	False -> FastCopy . annexFastCopy <$> Annex.getGitConfig
	True -> return (FastCopy True)

{- Copies from src to dest, updating a meter. Preserves mode and mtime.
 - Uses copy-on-write if it is supported. If the the destination already
 - exists, an interrupted copy will resume where it left off.
 -
 - The IncrementalVerifier is updated with the content of the file as it's
 - being copied. But it is not finalized at the end.
 -
 - When copy-on-write is used, the IncrementalVerifier is not fed
 - the content of the file, and verification using it will fail.
 -
 - Note that, when the destination file already exists, it's read both
 - to start calculating the hash, and also to verify that its content is
 - the same as the start of the source file. It's possible that the
 - destination file was created from some other source file,
 - (eg when isStableKey is false), and doing this avoids getting a
 - corrupted file in such cases.
 -}
fileCopier :: CopyCoWTried -> FastCopy -> OsPath -> OsPath -> MeterUpdate -> Maybe IncrementalVerifier -> IO CopyMethod
fileCopier copycowtried (FastCopy True) src dest meterupdate iv = do
	ok <- watchFileSize dest meterupdate $ const $
		copyFileExternal CopyTimeStamps src dest
	if ok
		then do
			maybe noop unableIncrementalVerifier iv
			return CopiedCoW
		else fileCopier copycowtried (FastCopy False) src dest meterupdate iv
#ifdef mingw32_HOST_OS
fileCopier _ _ src dest meterupdate iv =
	fileCopier' src dest meterupdate iv
#else
fileCopier copycowtried _ src dest meterupdate iv =
	ifM (tryCopyCoW copycowtried src dest meterupdate)
		( do
			maybe noop unableIncrementalVerifier iv
			return CopiedCoW
		, fileCopier' src dest meterupdate iv
		)
#endif

fileCopier' :: OsPath -> OsPath -> MeterUpdate -> Maybe IncrementalVerifier -> IO CopyMethod
fileCopier' src dest meterupdate iv = do
	-- The file might have had the write bit removed,
	-- so make sure we can write to it.
	void $ tryIO $ allowWrite dest

	F.withBinaryFile src ReadMode $ \hsrc ->
		fileContentCopier hsrc dest meterupdate iv
		
	-- Copy src mode and mtime.
	mode <- fileMode <$> R.getFileStatus (fromOsPath src)
	mtime <- utcTimeToPOSIXSeconds <$> getModificationTime src
	let dest' = fromOsPath dest
	R.setFileMode dest' mode
	touch dest' mtime False

	return Copied

{- Copies content from a handle to a destination file. Does not
 - use copy-on-write, and does not copy file mode and mtime.
 - Updates the IncementalVerifier with the content it copies.
 -}
fileContentCopier :: Handle -> OsPath -> MeterUpdate -> Maybe IncrementalVerifier -> IO ()
fileContentCopier hsrc dest meterupdate iv =
	F.withBinaryFile dest ReadWriteMode $ \hdest -> do
		sofar <- compareexisting hdest zeroBytesProcessed
		docopy hdest sofar
  where
	docopy hdest sofar = do
		s <- S.hGet hsrc defaultChunkSize
		if s == S.empty
			then return ()
			else do
				let sofar' = addBytesProcessed sofar (S.length s)
				S.hPut hdest s
				maybe noop (flip updateIncrementalVerifier s) iv
				meterupdate sofar'
				docopy hdest sofar'

	-- Leaves hdest and hsrc seeked to wherever the two diverge,
	-- so typically hdest will be seeked to end, and hsrc to the same
	-- position.
	compareexisting hdest sofar = do
		s <- S.hGet hdest defaultChunkSize
		if s == S.empty
			then return sofar
			else do
				s' <- getnoshort (S.length s) hsrc
				if s == s'
					then do
						maybe noop (flip updateIncrementalVerifier s) iv
						let sofar' = addBytesProcessed sofar (S.length s)
						meterupdate sofar'
						compareexisting hdest sofar'
					else do
						seekbefore hdest s
						seekbefore hsrc s'
						return sofar
	
	seekbefore h s = hSeek h RelativeSeek (fromIntegral (-1*S.length s))
	
	-- Like hGet, but never returns less than the requested number of
	-- bytes, unless it reaches EOF.
	getnoshort n h = do
		s <- S.hGet h n
		if S.length s == n || S.empty == s
			then return s
			else do
				s' <- getnoshort (n - S.length s) h
				return (s <> s')
