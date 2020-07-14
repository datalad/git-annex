{- git-annex general metadata storage log and per-remote metadata storage log.
 -
 - A line of the log will look like "timestamp field [+-]value [...]"
 -
 - (In the per-remote log, each field is prefixed with "uuid:")
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
 - Copyright 2014-2020 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Logs.MetaData (
	getCurrentMetaData,
	parseCurrentMetaData,
	getCurrentRemoteMetaData,
	addMetaData,
	addRemoteMetaData,
	addMetaDataClocked,
	currentMetaData,
	copyMetaData,
) where

import Annex.Common
import Types.MetaData
import Types.RemoteState
import Annex.MetaData.StandardFields
import Annex.VectorClock
import qualified Annex.Branch
import qualified Annex
import Logs
import Utility.TimeStamp
import Logs.MetaData.Pure

import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.ByteString.Lazy as L

{- Go through the log from oldest to newest, and combine it all
 - into a single MetaData representing the current state.
 -
 - Automatically generates a lastchanged metadata for each field that's
 - currently set, based on timestamps in the log.
 -}
getCurrentMetaData :: Key -> Annex MetaData
getCurrentMetaData = getCurrentMetaData' metaDataLogFile

getCurrentMetaData' :: (GitConfig -> Key -> RawFilePath) -> Key -> Annex MetaData
getCurrentMetaData' getlogfile k = do
	config <- Annex.getGitConfig
	parseCurrentMetaData <$> Annex.Branch.get (getlogfile config k)

parseCurrentMetaData :: L.ByteString -> MetaData
parseCurrentMetaData content =
	let ls = S.toAscList $ parseLog content
	    loggedmeta = logToCurrentMetaData ls
	in currentMetaData $ unionMetaData loggedmeta
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
	lastchangedval l = S.singleton $ toMetaValue $ encodeBS $ showts $ 
		case changed l of
			VectorClock t -> t
			Unknown -> 0
	showts = formatPOSIXTime "%F@%H-%M-%S"

getCurrentRemoteMetaData :: RemoteStateHandle -> Key -> Annex RemoteMetaData
getCurrentRemoteMetaData (RemoteStateHandle u) k = extractRemoteMetaData u <$>
	getCurrentMetaData' remoteMetaDataLogFile k

{- Adds in some metadata, which can override existing values, or unset
 - them, but otherwise leaves any existing metadata as-is. -}
addMetaData :: Key -> MetaData -> Annex ()
addMetaData = addMetaData' metaDataLogFile

addMetaData' :: (GitConfig -> Key -> RawFilePath) -> Key -> MetaData -> Annex ()
addMetaData' getlogfile k metadata = 
	addMetaDataClocked' getlogfile k metadata =<< liftIO currentVectorClock

{- Reusing the same VectorClock when making changes to the metadata
 - of multiple keys is a nice optimisation. The same metadata lines
 - will tend to be generated across the different log files, and so
 - git will be able to pack the data more efficiently. -}
addMetaDataClocked :: Key -> MetaData -> VectorClock -> Annex ()
addMetaDataClocked = addMetaDataClocked' metaDataLogFile

addMetaDataClocked' :: (GitConfig -> Key -> RawFilePath) -> Key -> MetaData -> VectorClock -> Annex ()
addMetaDataClocked' getlogfile k d@(MetaData m) c
	| d == emptyMetaData = noop
	| otherwise = do
		config <- Annex.getGitConfig
		Annex.Branch.change (getlogfile config k) $
			buildLog . simplifyLog 
				. S.insert (LogEntry c metadata)
				. parseLog
  where
	metadata = MetaData $ M.filterWithKey (\f _ -> not (isLastChangedField f)) m

addRemoteMetaData :: Key -> RemoteStateHandle -> MetaData -> Annex ()
addRemoteMetaData k (RemoteStateHandle u) m = 
	addMetaData' remoteMetaDataLogFile k $ fromRemoteMetaData $
		RemoteMetaData u m

getMetaDataLog :: Key -> Annex (Log MetaData)
getMetaDataLog key = do
	config <- Annex.getGitConfig
	readLog $ metaDataLogFile config key

{- Copies the metadata from the old key to the new key.
 -
 - The exact content of the metadata file is copied, so that the timestamps
 - remain the same, and because this is more space-efficient in the git
 - repository.
 - 
 - Any metadata already attached to the new key is not preserved.
 -
 - Returns True when metadata was copied.
 -}
copyMetaData :: Key -> Key -> Annex Bool
copyMetaData oldkey newkey
	| oldkey == newkey = return False
	| otherwise = do
		l <- getMetaDataLog oldkey
		if logToCurrentMetaData (S.toAscList l) == emptyMetaData
			then return False
			else do
				config <- Annex.getGitConfig
				Annex.Branch.change (metaDataLogFile config newkey) $
					const $ buildLog l
				return True

readLog :: RawFilePath -> Annex (Log MetaData)
readLog = parseLog <$$> Annex.Branch.get
