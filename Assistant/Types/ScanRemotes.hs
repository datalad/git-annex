{- git-annex assistant remotes needing scanning
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.Types.ScanRemotes where

import Common.Annex

import Control.Concurrent.STM
import qualified Data.Map as M

data ScanInfo = ScanInfo
	{ scanPriority :: Float
	, fullScan :: Bool
	}

type ScanRemoteMap = TMVar (M.Map Remote ScanInfo)

{- The TMVar starts empty, and is left empty when there are no remotes
 - to scan. -}
newScanRemoteMap :: IO ScanRemoteMap
newScanRemoteMap = atomically newEmptyTMVar
