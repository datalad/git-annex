{- git-annex assistant thread to detect when git-annex binary is changed
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.Threads.UpgradeWatcher (
	upgradWatcherThread
) where

import Assistant.Common
import Utility.DirWatcher
import Utility.DirWatcher.Types
import Config.Files

import Control.Concurrent.MVar

data WatcherState = InStartupScan | Started | Upgrading
	deriving (Eq)

upgradWatcherThread :: NamedThread
upgradWatcherThread = namedThread "UpgradeWatcher" $ go =<< liftIO programPath
  where
	go Nothing = debug [ "cannot determine program path" ]
	go (Just program) = do
		mvar <- liftIO $ newMVar InStartupScan
		changed <- Just <$> asIO2 (changedFile mvar program)
		let hooks = mkWatchHooks
			{ addHook = changed
			, addSymlinkHook = changed
			, modifyHook = changed
			, delDirHook = changed
			}
		let dir = parentDir program
		let depth = length (splitPath dir) + 1
		let nosubdirs f = length (splitPath f) == depth
		void $ liftIO $ watchDir dir nosubdirs hooks (startup mvar)
  	-- Ignore bogus events generated during the startup scan.
  	startup mvar scanner = do
		r <- scanner
		void $ swapMVar mvar Started
		return r

changedFile :: MVar WatcherState -> FilePath -> FilePath -> Maybe FileStatus -> Assistant ()
changedFile mvar program file _status
	| program == file = do
		state <- liftIO $ readMVar mvar
		when (state == Started) $
			debug [ "saw change to", file ]
	| otherwise = noop
