{- FSEvents interface
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Utility.FSEvents where

import Common hiding (isDirectory)
import Utility.Types.DirWatcher

import System.OSX.FSEvents
import qualified System.Posix.Files as Files
import Data.Bits ((.&.))

watchDir :: FilePath -> (FilePath -> Bool) -> WatchHooks -> IO EventStream
watchDir dir ignored hooks = do
	unlessM fileLevelEventsSupported $
		error "Need at least OSX 10.7.0 for file-level FSEvents"
	eventStreamCreate [dir] 1.0 True False True handle
  where
	handle evt
		| not (hasflag eventFlagItemIsFile) = noop
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

			{- Deletion events are received for both directories
			 - and files, with no way to differentiate between
			 - them. Deleting a directory always first yields
			 - events deleting its contents though, so we
			 - just always call delHook, and never delDirHook. -}
			when (hasflag eventFlagItemRemoved) $
				runhook delHook Nothing
			{- TODO deal with moving whole directories -}
			when (hasflag eventFlagItemCreated || hasflag eventFlagItemRenamed) $ do
				ms <- getstatus $ eventPath evt
				case ms of
					Nothing -> noop
					Just s
						| Files.isSymbolicLink s -> 
							runhook addSymlinkHook ms
						| Files.isRegularFile s ->
							runhook addHook ms
						| otherwise -> noop
			when (hasflag eventFlagItemModified) $ do
				ms <- getstatus $ eventPath evt
				runhook modifyHook ms
	  where
		getstatus = catchMaybeIO . getSymbolicLinkStatus
		hasflag f = eventFlags evt .&. f /= 0
		runhook h s = maybe noop (\a -> a (eventPath evt) s) (h hooks)

{- Check each component of the path to see if it's ignored. -}
ignoredPath :: (FilePath -> Bool) -> FilePath -> Bool
ignoredPath ignored = any ignored . map dropTrailingPathSeparator . splitPath
