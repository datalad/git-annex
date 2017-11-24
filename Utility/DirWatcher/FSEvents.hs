{- FSEvents interface
 -
 - Copyright 2012 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

module Utility.DirWatcher.FSEvents where

import Common hiding (isDirectory)
import Utility.DirWatcher.Types

import System.OSX.FSEvents
import qualified System.Posix.Files as Files
import Data.Bits ((.&.))

watchDir :: FilePath -> (FilePath -> Bool) -> Bool -> WatchHooks -> IO EventStream
watchDir dir ignored scanevents hooks = do
	unlessM fileLevelEventsSupported $
		giveup "Need at least OSX 10.7.0 for file-level FSEvents"
	scan dir
	eventStreamCreate [dir] 1.0 True True True dispatch
  where
	dispatch evt
		| ignoredPath ignored (eventPath evt) = noop
		| otherwise = do
			{- More than one flag may be set, if events occurred
			 - close together. 
			 - 
			 - Order is important..
			 - If a file is added and then deleted, we'll see it's
			 - not present, and addHook won't run.
			 - OTOH, if a file is deleted and then re-added,
			 - the delHook will run first, followed by the addHook.
			 -}

			when (hasflag eventFlagItemRemoved) $
				if hasflag eventFlagItemIsDir
					then runhook delDirHook Nothing
					else runhook delHook Nothing
			when (hasflag eventFlagItemCreated) $
				maybe noop handleadd =<< getstatus (eventPath evt)
			{- When a file or dir is renamed, a rename event is
			 - received for both its old and its new name. -}
			when (hasflag eventFlagItemRenamed) $
				if hasflag eventFlagItemIsDir
					then ifM (doesDirectoryExist $ eventPath evt)
						( scan $ eventPath evt
						, runhook delDirHook Nothing
						)
					else maybe (runhook delHook Nothing) handleadd
						=<< getstatus (eventPath evt)
			{- Add hooks are run when a file is modified for 
			 - compatability with INotify, which calls the add
			 - hook when a file is closed, and so tends to call
			 - both add and modify for file modifications. -}
			when (hasflag eventFlagItemModified && not (hasflag eventFlagItemIsDir)) $ do
				ms <- getstatus $ eventPath evt
				maybe noop handleadd ms
				runhook modifyHook ms
	  where
		hasflag f = eventFlags evt .&. f /= 0
		runhook h s = maybe noop (\a -> a (eventPath evt) s) (h hooks)
		handleadd s
			| Files.isSymbolicLink s = runhook addSymlinkHook $ Just s
			| Files.isRegularFile s = runhook addHook $ Just s
			| otherwise = noop
	
	scan d = unless (ignoredPath ignored d) $
		-- Do not follow symlinks when scanning.
		-- This mirrors the inotify startup scan behavior.
		mapM_ go =<< dirContentsRecursiveSkipping (const False) False d
	  where		
		go f
			| ignoredPath ignored f = noop
			| otherwise = do
				ms <- getstatus f
				case ms of
					Nothing -> noop
					Just s
						| Files.isSymbolicLink s ->
							when scanevents $
								runhook addSymlinkHook ms
						| Files.isRegularFile s ->
							when scanevents $
								runhook addHook ms
						| otherwise ->
							noop
		  where
			runhook h s = maybe noop (\a -> a f s) (h hooks)
		
	getstatus = catchMaybeIO . getSymbolicLinkStatus

{- Check each component of the path to see if it's ignored. -}
ignoredPath :: (FilePath -> Bool) -> FilePath -> Bool
ignoredPath ignored = any ignored . map dropTrailingPathSeparator . splitPath
