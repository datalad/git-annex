{- git-annex assistant thread to detect when git-annex binary is changed
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Assistant.Threads.UpgradeWatcher (
	upgradWatcherThread
) where

import Assistant.Common
import Utility.DirWatcher
import Utility.DirWatcher.Types
import Config.Files
import qualified Utility.Lsof as Lsof
import Utility.ThreadScheduler
import Assistant.Types.UrlRenderer
import Assistant.Alert
import Assistant.DaemonStatus
#ifdef WITH_WEBAPP
import Assistant.WebApp.Types
#endif

import Control.Concurrent.MVar
import Data.Tuple.Utils
import qualified Data.Text as T

data WatcherState = InStartupScan | Started | Upgrading
	deriving (Eq)

upgradWatcherThread :: UrlRenderer -> NamedThread
upgradWatcherThread urlrenderer = namedThread "UpgradeWatcher" $ go =<< liftIO programPath
  where
	go Nothing = debug [ "cannot determine program path" ]
	go (Just program) = do
		mvar <- liftIO $ newMVar InStartupScan
		changed <- Just <$> asIO2 (changedFile urlrenderer mvar program)
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

changedFile :: UrlRenderer -> MVar WatcherState -> FilePath -> FilePath -> Maybe FileStatus -> Assistant ()
changedFile urlrenderer mvar program file _status
	| program /= file = noop
	| otherwise = do
		state <- liftIO $ readMVar mvar
		when (state == Started) $ do
			setstate Upgrading
			ifM (sanityCheck program)
				( handleUpgrade urlrenderer
				, do
					debug ["new version of", program, "failed sanity check; not using"]
					setstate Started
				)
  where
	setstate = void . liftIO . swapMVar mvar

{- The program's file has been changed. Before restarting,
 - it needs to not be open for write by anything, and should run
 - successfully when run with the parameter "version".
 -}
sanityCheck :: FilePath -> Assistant Bool
sanityCheck program = do
	whileM (liftIO haswriter) $ do
		debug [program, "is still being written; waiting"]
		liftIO $ threadDelaySeconds (Seconds 60)
	debug [program, "has changed, and seems to be ready to run"]
	liftIO $ boolSystem program [Param "version"]
  where
	haswriter = not . null
		. filter (`elem` [Lsof.OpenReadWrite, Lsof.OpenWriteOnly])
		. map snd3
		<$> Lsof.query [program]

handleUpgrade :: UrlRenderer -> Assistant ()
handleUpgrade urlrenderer = do
#ifdef WITH_WEBAPP
	button <- mkAlertButton True (T.pack "Finish Upgrade") urlrenderer ConfigFinishUpgradeR
	void $ addAlert (upgradeReadyAlert button)
#else
	noop
#endif
