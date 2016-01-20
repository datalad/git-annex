{- git-annex general metadata storage log
 -
 - A line of the log will look like "timestamp field [+-]value [...]"
 -
 - Note that unset values are preserved. Consider this case:
 -
 - We have:
 -
 - 100 foo +x
 - 200 foo -x
 -
 - An unmerged remote has:
 -
 - 150 foo +x
 - 
 - After union merge, because the foo -x was preserved, we know that
 - after the other remote redundantly set foo +x, it was unset,
 - and so foo currently has no value.
 -
 -
 - Copyright 2014 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Logs.MetaData (
	getCurrentMetaData,
	addMetaData,
	addMetaData',
	currentMetaData,
	copyMetaData,
) where

import Annex.Common
import Types.MetaData
import Annex.MetaData.StandardFields
import qualified Annex.Branch
import qualified Annex
import Logs
import Logs.SingleValue
import Logs.TimeStamp

import qualified Data.Set as S
import qualified Data.Map as M
import Data.Time.Clock.POSIX

instance SingleValueSerializable MetaData where
	serialize = Types.MetaData.serialize
	deserialize = Types.MetaData.deserialize

getMetaDataLog :: Key -> Annex (Log MetaData)
getMetaDataLog key = do
	config <- Annex.getGitConfig
	readLog $ metaDataLogFile config key

{- Go through the log from oldest to newest, and combine it all
 - into a single MetaData representing the current state.
 -
 - Automatically generates a lastchanged metadata for each field that's
 - currently set, based on timestamps in the log.
 -}
getCurrentMetaData :: Key -> Annex MetaData
getCurrentMetaData k = do
	ls <- S.toAscList <$> getMetaDataLog k
	let loggedmeta = currentMetaData $ combineMetaData $ map value ls
	return $ currentMetaData $ unionMetaData loggedmeta
		(lastchanged ls loggedmeta)
  where
	lastchanged [] _ = emptyMetaData
	lastchanged ls (MetaData currentlyset) =
		let m = foldl' (flip M.union) M.empty (map genlastchanged ls)
		in MetaData $
			-- Add a overall lastchanged using the oldest log
			-- item (log is in ascending order).
			M.insert lastChangedField (lastchangedval $ Prelude.last ls) $
			M.mapKeys mkLastChangedField $
			-- Only include fields that are currently set.
			m `M.intersection` currentlyset
	-- Makes each field have the timestamp as its value.
	genlastchanged l =
		let MetaData m = value l
		    ts = lastchangedval l
		in M.map (const ts) m
	lastchangedval l = S.singleton $ toMetaValue $ showts $ changed l
	showts = formatPOSIXTime "%F@%H-%M-%S"

{- Adds in some metadata, which can override existing values, or unset
 - them, but otherwise leaves any existing metadata as-is. -}
addMetaData :: Key -> MetaData -> Annex ()
addMetaData k metadata = addMetaData' k metadata =<< liftIO getPOSIXTime

{- Reusing the same timestamp when making changes to the metadata
 - of multiple keys is a nice optimisation. The same metadata lines
 - will tend to be generated across the different log files, and so
 - git will be able to pack the data more efficiently. -}
addMetaData' :: Key -> MetaData -> POSIXTime -> Annex ()
addMetaData' k d@(MetaData m) now
	| d == emptyMetaData = noop
	| otherwise = do
		config <- Annex.getGitConfig
		Annex.Branch.change (metaDataLogFile config k) $
			showLog . simplifyLog 
				. S.insert (LogEntry now metadata)
				. parseLog
  where
	metadata = MetaData $ M.filterWithKey (\f _ -> not (isLastChangedField f)) m

{- Simplify a log, removing historical values that are no longer
 - needed. 
 -
 - This is not as simple as just making a single log line with the newest
 - state of all metadata. Consider this case:
 -
 - We have:
 -
 - 100 foo +x bar +y
 - 200 foo -x
 -
 - An unmerged remote has:
 -
 - 150 bar -y baz +w
 -
 - If what we have were simplified to "200 foo -x bar +y" then when the line
 - from the remote became available, it would be older than the simplified
 - line, and its change to bar would not take effect. That is wrong.
 -
 - Instead, simplify it to:
 -
 - 100 bar +y
 - 200 foo -x
 -
 - (Note that this ends up with the same number of lines as the
 - unsimplified version, so there's really no point in updating
 - the log to this version. Doing so would only add data to git,
 - with little benefit.)
 -
 - Now merging with the remote yields:
 -
 - 100 bar +y
 - 150 bar -y baz +w
 - 200 foo -x
 -
 - Simplifying again:
 -
 - 150 bar +z baz +w
 - 200 foo -x
 -}
simplifyLog :: Log MetaData -> Log MetaData
simplifyLog s = case sl of
	(newest:rest) -> 
		let sl' = go [newest] (value newest) rest
		in if length sl' < length sl
			then S.fromList sl'
			else s
	_ -> s
  where
	sl = S.toDescList s

	go c _ [] = c
	go c newer (l:ls)
		| unique == emptyMetaData = go c newer ls
		| otherwise = go (l { value = unique } : c)
			(unionMetaData unique newer) ls
	  where
		older = value l
		unique = older `differenceMetaData` newer

{- Copies the metadata from the old key to the new key.
 -
 - The exact content of the metadata file is copied, so that the timestamps
 - remain the same, and because this is more space-efficient in the git
 - repository.
 - 
 - Any metadata already attached to the new key is not preserved.
 -}
copyMetaData :: Key -> Key -> Annex ()
copyMetaData oldkey newkey
	| oldkey == newkey = noop
	| otherwise = do
		l <- getMetaDataLog oldkey
		unless (S.null l) $ do
			config <- Annex.getGitConfig
			Annex.Branch.change (metaDataLogFile config newkey) $
				const $ showLog l
