{- git-annex restage log file
 -
 - Copyright 2022 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Logs.Restage where

import Annex.Common
import Git.FilePath
import Logs.File
import Utility.InodeCache
import Annex.LockFile
import qualified Utility.FileIO as F

import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L

-- | Log a file whose pointer needs to be restaged in git.
-- The content of the file may not be a pointer, if it is populated with
-- the annex content. The InodeCache is used to verify that the file
-- still contains the content, and it's still safe to restage its pointer.
writeRestageLog :: TopFilePath -> InodeCache -> Annex ()
writeRestageLog f ic = do
	logf <- fromRepo gitAnnexRestageLog
	lckf <- fromRepo gitAnnexRestageLock
	appendLogFile logf lckf $ L.fromStrict $ formatRestageLog f ic

-- | Streams the content of the restage log.
--
-- First, the content of the log file is moved to the restage.old file.
-- If that file already exists, the content is appended, otherwise it's
-- renamed to that.
--
-- The log file is kept locked during that, but the lock is then
-- released. The processor may do something that itself needs to take the
-- lock, so it's important that the lock not be held while running it.
--
-- The content of restage.old file is then streamed to the processor, 
-- and then the finalizer is run, ending with emptying restage.old.
--
-- If the processor or finalizer is interrupted or throws an exception,
-- restage.old is left populated to be processed later.
streamRestageLog :: Annex () -> (TopFilePath -> InodeCache -> Annex ()) -> Annex ()
streamRestageLog finalizer processor = do
	logf <- fromRepo gitAnnexRestageLog
	oldf <- fromRepo gitAnnexRestageLogOld
	lckf <- fromRepo gitAnnexRestageLock
	
	withExclusiveLock lckf $ liftIO $
		whenM (doesPathExist logf) $
			ifM (doesPathExist oldf)
				( do
					h <- F.openFile oldf AppendMode
					hPutStr h =<< readFile (fromOsPath logf)
					hClose h
					liftIO $ removeWhenExistsWith removeFile logf
				, moveFile logf oldf
				)

	streamLogFileUnsafe oldf finalizer $ \l -> 
		case parseRestageLog l of
			Just (f, ic) -> processor f ic
			Nothing -> noop
	
	liftIO $ removeWhenExistsWith removeFile oldf

-- | Calculate over both the current restage log, and also over the old
-- one if it had started to be processed but did not get finished due
-- to an interruption.
calcRestageLog :: t -> ((TopFilePath, InodeCache) -> t -> t) -> Annex t
calcRestageLog start update = do
	logf <- fromRepo gitAnnexRestageLog
	oldf <- fromRepo gitAnnexRestageLogOld
	lckf <- fromRepo gitAnnexRestageLock
	withSharedLock lckf $ do
		mid <- calcLogFileUnsafe logf start process
		calcLogFileUnsafe oldf mid process
  where
	process l v = case parseRestageLog (decodeBL l) of
		Just pl -> update pl v
		Nothing -> v

formatRestageLog :: TopFilePath -> InodeCache -> S.ByteString
formatRestageLog f ic =
	encodeBS (showInodeCache ic) <> ":" <> fromOsPath (getTopFilePath f)

parseRestageLog :: String -> Maybe (TopFilePath, InodeCache)
parseRestageLog l = 
	let (ics, f) = separate (== ':') l
	in do
		ic <- readInodeCache ics
		return (asTopFilePath (toOsPath f), ic)
