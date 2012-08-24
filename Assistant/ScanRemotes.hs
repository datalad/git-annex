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

{- Blocks until there is a remote that needs to be scanned.
 - Processes higher priority remotes first. -}
getScanRemote :: ScanRemoteMap -> IO (Remote, ScanInfo)
getScanRemote v = atomically $ do
	m <- takeTMVar v
	let l = reverse $ sortBy (compare `on` scanPriority . snd) $ M.toList m
	case l of
		[] -> retry -- should never happen
		(ret@(r, _):_) -> do
			let m' = M.delete r m
			unless (M.null m') $
				putTMVar v m'
			return ret

{- Adds new remotes that need scanning to the map. -}
addScanRemotes :: ScanRemoteMap -> [Remote] -> Bool -> IO ()
addScanRemotes _ [] _ = noop
addScanRemotes v rs full = atomically $ do
	m <- fromMaybe M.empty <$> tryTakeTMVar v
	putTMVar v $ M.unionWith merge (M.fromList $ zip rs (map info rs)) m
	where
		info r = ScanInfo (Remote.cost r) full
		merge x y = ScanInfo
			{ scanPriority = max (scanPriority x) (scanPriority y)
			, fullScan = fullScan x || fullScan y 
			}
