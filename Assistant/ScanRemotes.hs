{- git-annex assistant remotes needing scanning
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.ScanRemotes where

import Assistant.Common
import Assistant.Types.ScanRemotes
import qualified Types.Remote as Remote

import Data.Function
import Control.Concurrent.STM
import qualified Data.Map.Strict as M

{- Blocks until there is a remote or remotes that need to be scanned.
 -
 - The list has higher priority remotes listed first. -}
getScanRemote :: Assistant [(Remote, ScanInfo)]
getScanRemote = do
	v <- getAssistant scanRemoteMap
	liftIO $ atomically $
		reverse . sortBy (compare `on` scanPriority . snd) . M.toList
			<$> takeTMVar v

{- Adds new remotes that need scanning. -}
addScanRemotes :: Bool -> [Remote] -> Assistant ()
addScanRemotes _ [] = noop
addScanRemotes full rs = do
	v <- getAssistant scanRemoteMap
	liftIO $ atomically $ do
		m <- fromMaybe M.empty <$> tryTakeTMVar v
		putTMVar v $ M.unionWith merge (M.fromList $ zip rs (map info rs)) m
  where
	info r = ScanInfo (-1 * Remote.cost r) full
	merge x y = ScanInfo
		{ scanPriority = max (scanPriority x) (scanPriority y)
		, fullScan = fullScan x || fullScan y 
		}
