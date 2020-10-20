{- git-annex smudge log file
 -
 - Copyright 2018 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Logs.Smudge where

import Annex.Common
import Git.FilePath
import Logs.File

import qualified Data.ByteString.Lazy as L

-- | Log a smudged file.
smudgeLog :: Key -> TopFilePath -> Annex ()
smudgeLog k f = do
	logf <- fromRepo gitAnnexSmudgeLog
	appendLogFile logf gitAnnexSmudgeLock $ L.fromStrict $
		serializeKey' k <> " " <> getTopFilePath f

-- | Streams all smudged files, and then empties the log at the end.
--
-- If the action is interrupted or throws an exception, the log file is
-- left unchanged.
--
-- Locking is used to prevent new items being added to the log while this
-- is running.
streamSmudged :: (Key -> TopFilePath -> Annex ()) -> Annex ()
streamSmudged a = do
	logf <- fromRepo gitAnnexSmudgeLog
	streamLogFile logf gitAnnexSmudgeLock $ \l -> 
		case parse l of
			Nothing -> noop
			Just (k, f) -> a k f
  where
	parse l = 
		let (ks, f) = separate (== ' ') l
		in do
			k <- deserializeKey ks
			return (k, asTopFilePath (toRawFilePath f))
