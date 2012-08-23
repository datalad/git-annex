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

type Priority = Int

type ScanRemoteMap = TMVar (M.Map Remote Priority)

{- The TMVar starts empty, and is left empty when there are no remotes
 - to scan. -}
newScanRemoteMap :: IO ScanRemoteMap
newScanRemoteMap = atomically newEmptyTMVar

{- Blocks until there is a remote that needs to be scanned.
 - Processes higher priority remotes first. -}
getScanRemote :: ScanRemoteMap -> IO Remote
getScanRemote v = atomically $ do
	m <- takeTMVar v
	let l = reverse $ map fst $ sortBy (compare `on` snd) $ M.toList m
	case l of
		[] -> retry -- should never happen
		(newest:_) -> do
			let m' = M.delete newest m
			unless (M.null m') $
				putTMVar v m'
			return newest

{- Adds new remotes that need scanning to the map. -}
addScanRemotes :: ScanRemoteMap -> [Remote] -> IO ()
addScanRemotes _ [] = noop
addScanRemotes v rs = atomically $ do
	m <- fromMaybe M.empty <$> tryTakeTMVar v
	putTMVar v $ M.union m $ M.fromList $ map (\r -> (r, Remote.cost r)) rs
