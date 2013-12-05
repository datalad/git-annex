{- Win32-notify interface
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Utility.DirWatcher.Win32Notify where

import Common hiding (isDirectory)
import Utility.DirWatcher.Types

import System.Win32.Notify
import qualified System.PosixCompat.Files as Files

watchDir :: FilePath -> (FilePath -> Bool) -> WatchHooks -> IO WatchManager
watchDir dir ignored hooks = do
	scan dir
	wm <- initWatchManager
	void $ watchDirectory wm dir True [Create, Delete, Modify, Move] handle
	return wm
  where
	handle evt
		| ignoredPath ignored (filePath evt) = noop
		| otherwise = case evt of
			(Deleted _ _)
				| isDirectory evt -> runhook delDirHook Nothing
				| otherwise -> runhook delHook Nothing
			(Created _ _)
				| isDirectory evt -> noop
				| otherwise -> runhook addHook Nothing
			(Modified _ _)
				| isDirectory evt -> noop
				{- Add hooks are run when a file is modified for 
				 - compatability with INotify, which calls the add
				 - hook when a file is closed, and so tends to call
				 - both add and modify for file modifications. -}
				| otherwise -> do
					runhook addHook Nothing
					runhook modifyHook Nothing
	  where
		runhook h s = maybe noop (\a -> a (filePath evt) s) (h hooks)

	scan d = unless (ignoredPath ignored d) $
		mapM_ go =<< dirContentsRecursive d
	  where		
		go f
			| ignoredPath ignored f = noop
			| otherwise = do
				ms <- getstatus f
				case ms of
					Nothing -> noop
					Just s
						| Files.isRegularFile s ->
							runhook addHook ms
						| otherwise ->
							noop
		  where
			runhook h s = maybe noop (\a -> a f s) (h hooks)
		
	getstatus = catchMaybeIO . getFileStatus

{- Check each component of the path to see if it's ignored. -}
ignoredPath :: (FilePath -> Bool) -> FilePath -> Bool
ignoredPath ignored = any ignored . map dropTrailingPathSeparator . splitPath
