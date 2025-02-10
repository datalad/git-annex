{- git-annex object content presence, low-level functions
 -
 - Copyright 2010-2021 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Annex.Content.Presence.LowLevel where

import Annex.Common
import Annex.Verify
import Annex.InodeSentinal
import Utility.InodeCache

isUnmodifiedLowLevel :: (Key -> [InodeCache] -> Annex ()) -> Key -> OsPath -> InodeCache -> [InodeCache] -> Annex Bool
isUnmodifiedLowLevel addinodecaches key f fc ic =
	isUnmodifiedCheapLowLevel fc ic <||> expensivecheck
  where
	expensivecheck = ifM (verifyKeyContent key f)
		( do
			-- The file could have been modified while it was
			-- being verified. Detect that.
			ifM (geti >>= maybe (return False) (compareInodeCaches fc))
				( do
					-- Update the InodeCache to avoid
					-- performing this expensive check again.
					addinodecaches key [fc]
					return True
				, return False
				)
		, return False
		)
	geti = withTSDelta (liftIO . genInodeCache f)

isUnmodifiedCheapLowLevel :: InodeCache -> [InodeCache] -> Annex Bool
isUnmodifiedCheapLowLevel fc ic = anyM (compareInodeCaches fc) ic
