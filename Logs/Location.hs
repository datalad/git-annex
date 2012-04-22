{-# LANGUAGE BangPatterns #-}

{- git-annex location log
 -
 - git-annex keeps track of which repositories have the contents of annexed
 - files.
 -
 - Repositories record their UUID and the date when they --get or --drop
 - a value.
 - 
 - Copyright 2010-2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Logs.Location (
	LogStatus(..),
	logChange,
	loggedLocations,
	loggedKeys,
	loggedKeysFor,
	logFile,
	logFileKey
) where

import Common.Annex
import qualified Annex.Branch
import Logs.Presence

{- Log a change in the presence of a key's value in a repository. -}
logChange :: Key -> UUID -> LogStatus -> Annex ()
logChange key (UUID u) s = addLog (logFile key) =<< logNow s u
logChange _ NoUUID _ = noop

{- Returns a list of repository UUIDs that, according to the log, have
 - the value of a key.
 -}
loggedLocations :: Key -> Annex [UUID]
loggedLocations key = map toUUID <$> (currentLog . logFile) key

{- Finds all keys that have location log information.
 - (There may be duplicate keys in the list.) -}
loggedKeys :: Annex [Key]
loggedKeys = mapMaybe (logFileKey . takeFileName) <$> Annex.Branch.files

{- Finds all keys that have location log information indicating
 - they are present for the specified repository. -}
loggedKeysFor :: UUID -> Annex [Key]
loggedKeysFor u = filterM isthere =<< loggedKeys
	where
		{- This should run strictly to avoid the filterM
		 - building many thunks containing keyLocations data. -}
		isthere k = do
			us <- loggedLocations k
			let !there = u `elem` us
			return there

{- The filename of the log file for a given key. -}
logFile :: Key -> String
logFile key = hashDirLower key ++ keyFile key ++ ".log"

{- Converts a log filename into a key. -}
logFileKey :: FilePath -> Maybe Key
logFileKey file
	| ext == ".log" = fileKey base
	| otherwise = Nothing
	where
		(base, ext) = splitAt (length file - 4) file
