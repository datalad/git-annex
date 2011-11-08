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
	readLog,
	keyLocations,
	loggedKeys,
	loggedKeysFor,
	logFile,
	logFileKey
) where

import Common.Annex
import qualified Git
import qualified Annex.Branch
import Logs.Presence

{- Log a change in the presence of a key's value in a repository. -}
logChange :: Git.Repo -> Key -> UUID -> LogStatus -> Annex ()
logChange _ key (UUID u) s = addLog (logFile key) =<< logNow s u
logChange repo _ NoUUID _ = error $
	"unknown UUID for " ++ Git.repoDescribe repo ++ 
	" (have you run git annex init there?)"

{- Returns a list of repository UUIDs that, according to the log, have
 - the value of a key. -}
keyLocations :: Key -> Annex [UUID]
keyLocations key = map toUUID <$> (currentLog . logFile) key

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
			us <- keyLocations k
			let !there = u `elem` us
			return there

{- The filename of the log file for a given key. -}
logFile :: Key -> String
logFile key = hashDirLower key ++ keyFile key ++ ".log"

{- Converts a log filename into a key. -}
logFileKey :: FilePath -> Maybe Key
logFileKey file
	| end == ".log" = fileKey beginning
	| otherwise = Nothing
	where
		(beginning, end) = splitAt (length file - 4) file
