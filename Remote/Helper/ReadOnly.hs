{- Adds readonly support to remotes.
 -
 - Copyright 2013-2019 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Remote.Helper.ReadOnly
	( adjustReadOnly
	, readonlyStoreKey
	, readonlyStorer
	, readonlyRemoveKey
	) where

import Annex.Common
import Types.Remote
import Types.StoreRetrieve
import Types.Import
import Types.Export
import Utility.Metered

{- Adds support for read-only remotes, by replacing the
 - methods that write to a remote with dummies that fail.
 -
 - Note that disabling git pushes to remotes is not handled here.
 -}
adjustReadOnly :: Remote -> Remote
adjustReadOnly r
	| remoteAnnexReadOnly (gitconfig r) = r
		{ storeKey = readonlyStoreKey
		, removeKey = readonlyRemoveKey
		, repairRepo = Nothing
		, exportActions = (exportActions r)
			{ storeExport = readonlyStoreExport
			, removeExport = readonlyRemoveExport
			, removeExportDirectory = Just readonlyRemoveExportDirectory
			, renameExport = Nothing
			}
		, importActions = (importActions r)
			{ storeExportWithContentIdentifier = readonlyStoreExportWithContentIdentifier
			, removeExportWithContentIdentifier = readonlyRemoveExportWithContentIdentifier
			, removeExportDirectoryWhenEmpty = Just readonlyRemoveExportDirectory
			}
		}
	| otherwise = r

readonlyStoreKey :: Key -> AssociatedFile -> Maybe OsPath -> MeterUpdate -> Annex ()
readonlyStoreKey _ _ _ _ = readonlyFail

readonlyRemoveKey :: Maybe SafeDropProof -> Key -> Annex ()
readonlyRemoveKey _ _ = readonlyFail

readonlyStorer :: Storer
readonlyStorer _ _ _ = readonlyFail

readonlyStoreExport :: OsPath -> Key -> ExportLocation -> MeterUpdate -> Annex ()
readonlyStoreExport _ _ _ _ = readonlyFail

readonlyRemoveExport :: Key -> ExportLocation -> Annex ()
readonlyRemoveExport _ _ = readonlyFail

readonlyRemoveExportDirectory :: ExportDirectory -> Annex ()
readonlyRemoveExportDirectory _ = readonlyFail

readonlyStoreExportWithContentIdentifier :: OsPath -> Key -> ExportLocation -> [ContentIdentifier] -> MeterUpdate -> Annex ContentIdentifier
readonlyStoreExportWithContentIdentifier _ _ _ _ _ = readonlyFail

readonlyRemoveExportWithContentIdentifier :: Key -> ExportLocation -> [ContentIdentifier] -> Annex ()
readonlyRemoveExportWithContentIdentifier _ _ _ = readonlyFail

readonlyFail :: Annex a
readonlyFail = giveup readonlyWarning

readonlyWarning :: String
readonlyWarning = "this remote is readonly"
