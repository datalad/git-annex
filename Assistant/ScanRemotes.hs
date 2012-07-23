{- git-annex assistant remotes needing scanning
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.ScanRemotes where

import Common.Annex
import Data.Function

import Control.Concurrent.STM
import Data.Time.Clock
import qualified Data.Map as M

type ScanRemoteMap = TMVar (M.Map Remote UTCTime)

{- The TMVar starts empty, and is left empty when there are no remotes
 - to scan. -}
newScanRemoteMap :: IO ScanRemoteMap
newScanRemoteMap = atomically newEmptyTMVar

{- Blocks until there is a remote that needs to be scanned.
 - Processes remotes added most recently first. -}
getScanRemote :: ScanRemoteMap -> IO Remote
getScanRemote v = atomically $ do
	m <- takeTMVar v
	let newest = Prelude.head $ reverse $
		map fst $ sortBy (compare `on` snd) $ M.toList m
	putTMVar v $ M.delete newest m
	return newest

{- Adds new remotes that need scanning to the map. -}
addScanRemotes :: ScanRemoteMap -> [Remote] -> IO ()
addScanRemotes _ [] = return ()
addScanRemotes v rs = do
	now <- getCurrentTime
	atomically $ do
		m <- fromMaybe M.empty <$> tryTakeTMVar v
		putTMVar v $ foldr (`M.insert` now) m rs
