{- Waiting for changed git refs
 -
 - Copyright 2014-216 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Annex.ChangedRefs
	( ChangedRefs(..)
	, ChangedRefsHandle
	, waitChangedRefs
	, drainChangedRefs
	, stopWatchingChangedRefs
	, watchChangedRefs
	) where

import Annex.Common
import Utility.DirWatcher
import Utility.DirWatcher.Types
import qualified Git
import Git.Sha
import qualified Utility.SimpleProtocol as Proto

import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.STM.TBMChan

newtype ChangedRefs = ChangedRefs [Git.Ref]
	deriving (Show)

instance Proto.Serializable ChangedRefs where
	serialize (ChangedRefs l) = unwords $ map Git.fromRef l
	deserialize = Just . ChangedRefs . map Git.Ref . words

data ChangedRefsHandle = ChangedRefsHandle DirWatcherHandle (TBMChan Git.Sha)

-- | Wait for one or more git refs to change.
--
-- When possible, coalesce ref writes that occur closely together
-- in time. Delay up to 0.05 seconds to get more ref writes.
waitChangedRefs :: ChangedRefsHandle -> IO ChangedRefs
waitChangedRefs (ChangedRefsHandle _ chan) = do
	v <- atomically $ readTBMChan chan
	case v of
		Nothing -> return $ ChangedRefs []
		Just r -> do
			threadDelay 50000
			rs <- atomically $ loop []
			return $ ChangedRefs (r:rs)
  where
	loop rs = do
		v <- tryReadTBMChan chan
		case v of
			Just (Just r) -> loop (r:rs)
			_ -> return rs

-- | Remove any changes that might be buffered in the channel,
-- without waiting for any new changes.
drainChangedRefs :: ChangedRefsHandle -> IO ()
drainChangedRefs (ChangedRefsHandle _ chan) = atomically go
  where
	go = do
		v <- tryReadTBMChan chan
		case v of
			Just (Just _) -> go
			_ -> return ()

stopWatchingChangedRefs :: ChangedRefsHandle -> IO ()
stopWatchingChangedRefs h@(ChangedRefsHandle wh chan) = do
	stopWatchDir wh
	atomically $ closeTBMChan chan
	drainChangedRefs h

watchChangedRefs :: Annex (Maybe ChangedRefsHandle)
watchChangedRefs = do
	-- This channel is used to accumulate notifications,
	-- because the DirWatcher might have multiple threads that find
	-- changes at the same time. It is bounded to allow a watcher
	-- to be started once and reused, without too many changes being
	-- buffered in memory.
	chan <- liftIO $ newTBMChanIO 100
	
	g <- gitRepo
	let refdir = Git.localGitDir g </> "refs"
	liftIO $ createDirectoryIfMissing True refdir

	let notifyhook = Just $ notifyHook chan
	let hooks = mkWatchHooks
		{ addHook = notifyhook
		, modifyHook = notifyhook
		}

	if canWatch
		then do
			h <- liftIO $ watchDir refdir (const False) True hooks id
			return $ Just $ ChangedRefsHandle h chan
		else return Nothing

notifyHook :: TBMChan Git.Sha -> FilePath -> Maybe FileStatus -> IO ()
notifyHook chan reffile _
	| ".lock" `isSuffixOf` reffile = noop
	| otherwise = void $ do
		sha <- catchDefaultIO Nothing $
			extractSha <$> readFile reffile
		-- When the channel is full, there is probably no reader
		-- running, or ref changes have been occuring very fast,
		-- so it's ok to not write the change to it.
		maybe noop (void . atomically . tryWriteTBMChan chan) sha
