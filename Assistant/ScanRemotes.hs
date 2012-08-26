{- git-annex assistant remotes needing scanning
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.ScanRemotes where

import Common.Annex
import qualified Types.Remote as Remote

import Data.Function
import Control.Concurrent.STM
import qualified Data.Map as M

data ScanInfo = ScanInfo
	{ scanPriority :: Int
	, fullScan :: Bool
	}

type ScanRemoteMap = TMVar (M.Map Remote ScanInfo)

{- The TMVar starts empty, and is left empty when there are no remotes
 - to scan. -}
newScanRemoteMap :: IO ScanRemoteMap
newScanRemoteMap = atomically newEmptyTMVar

{- Blocks until there is a remote or remotes that need to be scanned.
 -
 - The list has higher priority remotes listed first. -}
getScanRemote :: ScanRemoteMap -> IO [(Remote, ScanInfo)]
getScanRemote v = atomically $
	reverse . sortBy (compare `on` scanPriority . snd) . M.toList
		<$> takeTMVar v

{- Adds new remotes that need scanning. -}
addScanRemotes :: ScanRemoteMap -> Bool -> [Remote] -> IO ()
addScanRemotes _ _ [] = noop
addScanRemotes v full rs = atomically $ do
	m <- fromMaybe M.empty <$> tryTakeTMVar v
	putTMVar v $ M.unionWith merge (M.fromList $ zip rs (map info rs)) m
	where
		info r = ScanInfo (-1 * Remote.cost r) full
		merge x y = ScanInfo
			{ scanPriority = max (scanPriority x) (scanPriority y)
			, fullScan = fullScan x || fullScan y 
			}
