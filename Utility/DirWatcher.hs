{- generic directory watching interface
 -
 - Uses inotify, or kqueue, or fsevents, or win32-notify to watch a directory
 - (and subdirectories) for changes, and runs hooks for different
 - sorts of events as they occur.
 -
 - Copyright 2012-2013 Joey Hess <joey@kitenet.net>
 -
 - License: BSD-2-clause
 -}

{-# LANGUAGE CPP #-}

module Utility.DirWatcher where

import Utility.DirWatcher.Types

#if WITH_INOTIFY
import qualified Utility.DirWatcher.INotify as INotify
import qualified System.INotify as INotify
#endif
#if WITH_KQUEUE
import qualified Utility.DirWatcher.Kqueue as Kqueue
import Control.Concurrent
#endif
#if WITH_FSEVENTS
import qualified Utility.DirWatcher.FSEvents as FSEvents
import qualified System.OSX.FSEvents as FSEvents
#endif
#if WITH_WIN32NOTIFY
import qualified Utility.DirWatcher.Win32Notify as Win32Notify
import qualified System.Win32.Notify as Win32Notify
#endif

type Pruner = FilePath -> Bool

canWatch :: Bool
#if (WITH_INOTIFY || WITH_KQUEUE || WITH_FSEVENTS || WITH_WIN32NOTIFY)
canWatch = True
#else
#if defined linux_HOST_OS
#warning "Building without inotify support"
#endif
canWatch = False
#endif

{- With inotify, discrete events will be received when making multiple changes
 - to the same filename. For example, adding it, deleting it, and adding it
 - again will be three events.
 - 
 - OTOH, with kqueue, often only one event is received, indicating the most
 - recent state of the file. -}
eventsCoalesce :: Bool
#if (WITH_INOTIFY || WITH_WIN32NOTIFY)
eventsCoalesce = False
#else
#if (WITH_KQUEUE || WITH_FSEVENTS)
eventsCoalesce = True
#else
eventsCoalesce = undefined
#endif
#endif

{- With inotify, file closing is tracked to some extent, so an add event
 - will always be received for a file once its writer closes it, and
 - (typically) not before. This may mean multiple add events for the same file.
 - 
 - fsevents behaves similarly, although different event types are used for
 - creating and modification of the file.
 -
 - OTOH, with kqueue, add events will often be received while a file is
 - still being written to, and then no add event will be received once the
 - writer closes it. -}
closingTracked :: Bool
#if (WITH_INOTIFY || WITH_FSEVENTS || WITH_WIN32NOTIFY)
closingTracked = True
#else
#if WITH_KQUEUE
closingTracked = False
#else
closingTracked = undefined
#endif
#endif

{- With inotify, modifications to existing files can be tracked.
 - Kqueue does not support this.
 - Fsevents generates events when an existing file is reopened and rewritten,
 - but not necessarily when it's opened once and modified repeatedly. -}
modifyTracked :: Bool
#if (WITH_INOTIFY || WITH_FSEVENTS || WITH_WIN32NOTIFY)
modifyTracked = True
#else
#if WITH_KQUEUE
modifyTracked = False
#else
modifyTracked = undefined
#endif
#endif

{- Starts a watcher thread. The runstartup action is passed a scanner action
 - to run, that will return once the initial directory scan is complete.
 - Once runstartup returns, the watcher thread continues running,
 - and processing events. Returns a DirWatcherHandle that can be used
 - to shutdown later. -}
#if WITH_INOTIFY
type DirWatcherHandle = INotify.INotify
watchDir :: FilePath -> Pruner -> Bool -> WatchHooks -> (IO () -> IO ()) -> IO DirWatcherHandle
watchDir dir prune scanevents hooks runstartup = do
	i <- INotify.initINotify
	runstartup $ INotify.watchDir i dir prune scanevents hooks
	return i
#else
#if WITH_KQUEUE
type DirWatcherHandle = ThreadId
watchDir :: FilePath -> Pruner -> Bool -> WatchHooks -> (IO Kqueue.Kqueue -> IO Kqueue.Kqueue) -> IO DirWatcherHandle
watchDir dir prune _scanevents hooks runstartup = do
	kq <- runstartup $ Kqueue.initKqueue dir prune
	forkIO $ Kqueue.runHooks kq hooks
#else
#if WITH_FSEVENTS
type DirWatcherHandle = FSEvents.EventStream
watchDir :: FilePath -> Pruner -> Bool -> WatchHooks -> (IO FSEvents.EventStream -> IO FSEvents.EventStream) -> IO DirWatcherHandle
watchDir dir prune scanevents hooks runstartup =
	runstartup $ FSEvents.watchDir dir prune scanevents hooks
#else
#if WITH_WIN32NOTIFY
type DirWatcherHandle = Win32Notify.WatchManager
watchDir :: FilePath -> Pruner -> Bool -> WatchHooks -> (IO Win32Notify.WatchManager -> IO Win32Notify.WatchManager) -> IO DirWatcherHandle
watchDir dir prune scanevents hooks runstartup =
	runstartup $ Win32Notify.watchDir dir prune scanevents hooks
#else
type DirWatcherHandle = ()
watchDir :: FilePath -> Pruner -> Bool -> WatchHooks -> (IO () -> IO ()) -> IO DirWatcherHandle
watchDir = undefined
#endif
#endif
#endif
#endif

stopWatchDir :: DirWatcherHandle -> IO ()
#if WITH_INOTIFY
stopWatchDir = INotify.killINotify
#else
#if WITH_KQUEUE
stopWatchDir = killThread
#else
#if WITH_FSEVENTS
stopWatchDir = FSEvents.eventStreamDestroy
#else
#if WITH_WIN32NOTIFY
stopWatchDir = Win32Notify.killWatchManager
#else
stopWatchDir = undefined
#endif
#endif
#endif
#endif
