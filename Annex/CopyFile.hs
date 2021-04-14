{- Copying files.
 -
 - Copyright 2011-2021 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Annex.CopyFile where

import Annex.Common
import Types.Remote
import Utility.Metered
import Utility.CopyFile
import Utility.FileMode
import Utility.Touch
import Types.Backend
import Backend
import Annex.Verify

import Control.Concurrent
import qualified Data.ByteString as S
import Data.Time.Clock.POSIX

-- Copies from src to dest, updating a meter. If the copy finishes
-- successfully, calls a final check action, which must also succeed, or
-- returns false.
--
-- If either the remote or local repository wants to use hard links,
-- the copier will do so (falling back to copying if a hard link cannot be
-- made).
--
-- When a hard link is created, returns Verified; the repo being linked
-- from is implicitly trusted, so no expensive verification needs to be
-- done. Also returns Verified if the key's content is verified while
-- copying it.
type FileCopier = FilePath -> FilePath -> Key -> MeterUpdate -> Annex Bool -> VerifyConfig -> Annex (Bool, Verification)

-- To avoid the overhead of trying copy-on-write every time, it's tried
-- once and if it fails, is not tried again.
newtype CopyCoWTried = CopyCoWTried (MVar Bool)

newCopyCoWTried :: IO CopyCoWTried
newCopyCoWTried = CopyCoWTried <$> newEmptyMVar

{- Copys a file. Uses copy-on-write if it is supported. Otherwise,
 - copies the file itself. If the destination already exists,
 - an interruped copy will resume where it left off.
 -
 - When copy-on-write is used, returns UnVerified, because the content of
 - the file has not been verified to be correct. When the file has to be
 - read to copy it, a hash is calulated at the same time.
 -
 - Note that, when the destination file already exists, it's read both
 - to start calculating the hash, and also to verify that its content is
 - the same as the start of the source file. It's possible that the
 - destination file was created from some other source file,
 - (eg when isStableKey is false), and doing this avoids getting a
 - corrupted file in such cases.
 -}
fileCopier :: CopyCoWTried -> FileCopier
#ifdef mingw32_HOST_OS
fileCopier _ src dest k meterupdate check verifyconfig = docopy
  where
#else
fileCopier (CopyCoWTried copycowtried) src dest k meterupdate check verifyconfig =
	-- If multiple threads reach this at the same time, they
	-- will both try CoW, which is acceptable.
	ifM (liftIO $ isEmptyMVar copycowtried)
		( do
			ok <- docopycow
			void $ liftIO $ tryPutMVar copycowtried ok
			if ok
				then unVerified check
				else docopy
		, ifM (liftIO $ readMVar copycowtried)
			( do
				ok <- docopycow
				if ok
					then unVerified check
					else docopy
			, docopy
			)
		)
  where
	docopycow = liftIO $ watchFileSize dest meterupdate $
		copyCoW CopyTimeStamps src dest
#endif

	dest' = toRawFilePath dest

	docopy = do
		iv <- startVerifyKeyContentIncrementally verifyconfig k

		-- The file might have had the write bit removed,
		-- so make sure we can write to it.
		void $ liftIO $ tryIO $ allowWrite dest'

		liftIO $ withBinaryFile dest ReadWriteMode $ \hdest ->
			withBinaryFile src ReadMode $ \hsrc -> do
				sofar <- compareexisting iv hdest hsrc zeroBytesProcessed
				docopy' iv hdest hsrc sofar

		-- Copy src mode and mtime.
		mode <- liftIO $ fileMode <$> getFileStatus src
		mtime <- liftIO $ utcTimeToPOSIXSeconds <$> getModificationTime src
		liftIO $ setFileMode dest mode
		liftIO $ touch dest' mtime False

		ifM check
			( case iv of
				Just x -> ifM (liftIO $ finalizeIncremental x)
					( return (True, Verified)
					, return (False, UnVerified)
					)
				Nothing -> return (True, UnVerified)
			, return (False, UnVerified)
			)
	
	docopy' iv hdest hsrc sofar = do
		s <- S.hGet hsrc defaultChunkSize
		if s == S.empty
			then return ()
			else do
				let sofar' = addBytesProcessed sofar (S.length s)
				S.hPut hdest s
				maybe noop (flip updateIncremental s) iv
				meterupdate sofar'
				docopy' iv hdest hsrc sofar'

	-- Leaves hdest and hsrc seeked to wherever the two diverge,
	-- so typically hdest will be seeked to end, and hsrc to the same
	-- position.
	compareexisting iv hdest hsrc sofar = do
		s <- S.hGet hdest defaultChunkSize
		if s == S.empty
			then return sofar
			else do
				s' <- getnoshort (S.length s) hsrc
				if s == s'
					then do
						maybe noop (flip updateIncremental s) iv
						let sofar' = addBytesProcessed sofar (S.length s)
						meterupdate sofar'
						compareexisting iv hdest hsrc sofar'
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
