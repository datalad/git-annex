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

import qualified Data.ByteString.Lazy as L

-- | Log a file whose pointer needs to be restaged in git.
-- The content of the file may not be a pointer, if it is populated with
-- the annex content. The InodeCache is used to verify that the file
-- still contains the content, and it's still safe to restage its pointer.
writeRestageLog :: TopFilePath -> InodeCache -> Annex ()
writeRestageLog f ic = do
	logf <- fromRepo gitAnnexRestageLog
	lckf <- fromRepo gitAnnexRestageLock
	appendLogFile logf lckf $ L.fromStrict $
		encodeBS (showInodeCache ic) <> ":" <> getTopFilePath f

-- | Streams the content of the restage log, and then empties the log at
-- the end.
--
-- If the action is interrupted or throws an exception, the log file is
-- left unchanged.
--
-- Locking is used to prevent new items being added to the log while this
-- is running.
streamSmudged :: (TopFilePath -> InodeCache -> Annex ()) -> Annex ()
streamSmudged a = do
	logf <- fromRepo gitAnnexRestageLog
	lckf <- fromRepo gitAnnexRestageLock
	streamLogFile (fromRawFilePath logf) lckf $ \l -> 
		case parse l of
			Nothing -> noop
			Just (k, f) -> a f ic
  where
	parse l = 
		let (ics, f) = separate (== ':') l
		in do
			ic <- readInodeCache ics
			return (asTopFilePath (toRawFilePath f), ic)

