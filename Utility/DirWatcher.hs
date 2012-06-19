{- generic directory watching interface
 -
 - Uses either inotify or kqueue to watch a directory (and subdirectories)
 - for changes, and runs hooks for different sorts of events as they occur.
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Utility.DirWatcher where

import Utility.Types.DirWatcher

#if WITH_INOTIFY
import qualified Utility.INotify as INotify
import qualified System.INotify as INotify
import Utility.ThreadScheduler
#endif
#if WITH_KQUEUE
import qualified Utility.Kqueue as Kqueue
#endif

type Pruner = FilePath -> Bool

canWatch :: Bool
#if (WITH_INOTIFY || WITH_KQUEUE)
canWatch = True
#else
#if defined linux_HOST_OS
#warning "Building without inotify support"
#endif
canWatch = False
#endif

/* With inotify, discrete events will be received when making multiple changes
 * to the same filename. For example, adding it, deleting it, and adding it
 * again will be three events.
 * 
 * OTOH, with kqueue, often only one event is received, indicating the most
 * recent state of the file.
 */
eventsCoalesce :: Bool
#if WITH_INOTIFY
eventsCoalesce = False
#else
#if WITH_KQUEUE
eventsCoalesce = True
#else
eventsCoalesce = undefined
#endif
#endif

#if WITH_INOTIFY
watchDir :: FilePath -> Pruner -> WatchHooks -> (IO () -> IO ()) -> IO ()
watchDir dir prune hooks runstartup = INotify.withINotify $ \i -> do
	runstartup $ INotify.watchDir i dir prune hooks
	waitForTermination -- Let the inotify thread run.
#else
#if WITH_KQUEUE
watchDir :: FilePath -> Pruner -> WatchHooks -> (IO Kqueue.Kqueue -> IO Kqueue.Kqueue) -> IO ()
watchDir dir ignored hooks runstartup = do
	kq <- runstartup $ Kqueue.initKqueue dir ignored
	Kqueue.runHooks kq hooks
#else
watchDir :: FilePath -> Pruner -> WatchHooks -> (IO () -> IO ()) -> IO ()
watchDir = undefined
#endif
#endif
