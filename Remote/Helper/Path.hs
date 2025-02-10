{- Utilities for remotes located in a path in the filesystem.
 -
 - Copyright 2023 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Remote.Helper.Path where

import Annex.Common
import Types.Availability

checkPathAvailability :: Bool -> OsPath -> Annex Availability
checkPathAvailability islocal d
	| not islocal = return GloballyAvailable
	| otherwise = ifM (liftIO $ doesDirectoryExist d)
		( return LocallyAvailable
		, return Unavailable
		)
