{- higher-level inotify interface
 -
 - Copyright 2012 Joey Hess <id@joeyh.name>
 -
 - License: BSD-2-clause
 -}

module Utility.DirWatcher.INotify where

import Common hiding (isDirectory)
import Utility.ThreadLock
import Utility.DirWatcher.Types

import System.INotify
import qualified System.Posix.Files as Files
import System.IO.Error
import Control.Exception (throw)

{- Watches for changes to files in a directory, and all its subdirectories
 - that are not ignored, using inotify. This function returns after
 - its initial scan is complete, leaving a thread running. Callbacks are
 - made for different events.
 -
 - Inotify is weak at recursive directory watching; the whole directory
 - tree must be scanned and watches set explicitly for each subdirectory.
 -
 - To notice newly created subdirectories, inotify is used, and
 - watches are registered for those directories. There is a race there;
 - things can be added to a directory before the watch gets registered.
 -
 - To close the inotify race, each time a new directory is found, it also 
 - recursively scans it, assuming all files in it were just added,
 - and registering each subdirectory.
 -
 - Note: Due to the race amelioration, multiple add events may occur
 - for the same file.
 - 
 - Note: Moving a file will cause events deleting it from its old location
 - and adding it to the new location. 
 - 
 - Note: It's assumed that when a file that was open for write is closed, 
 - it's finished being written to, and can be added.
 -
 - Note: inotify has a limit to the number of watches allowed,
 - /proc/sys/fs/inotify/max_user_watches (default 8192).
 - So this will fail if there are too many subdirectories. The
 - errHook is called when this happens.
 -}
watchDir :: INotify -> FilePath -> (FilePath -> Bool) -> Bool -> WatchHooks -> IO ()
watchDir i dir ignored scanevents hooks
	| ignored dir = noop
	| otherwise = do
		-- Use a lock to make sure events generated during initial
		-- scan come before real inotify events.
		lock <- newLock
		let handler event = withLock lock (void $ go event)
		flip catchNonAsync failedwatch $ do
			void (addWatch i watchevents dir handler)
				`catchIO` failedaddwatch
			withLock lock $
				mapM_ scan =<< filter (not . dirCruft) <$>
					getDirectoryContents dir
  where
	recurse d = watchDir i d ignored scanevents hooks

	-- Select only inotify events required by the enabled
	-- hooks, but always include Create so new directories can
	-- be scanned.
	watchevents = Create : addevents ++ delevents ++ modifyevents
	addevents
		| hashook addHook || hashook addSymlinkHook = [MoveIn, CloseWrite]
		| otherwise = []
	delevents
		| hashook delHook || hashook delDirHook = [MoveOut, Delete]
		| otherwise = []
	modifyevents
		| hashook modifyHook = [Modify]
		| otherwise = []

	scan f = unless (ignored f) $ do
		ms <- getstatus f
		case ms of
			Nothing -> return ()
			Just s
				| Files.isDirectory s ->
					recurse $ indir f
				| Files.isSymbolicLink s ->
					when scanevents $
						runhook addSymlinkHook f ms
				| Files.isRegularFile s ->
					when scanevents $
						runhook addHook f ms
				| otherwise ->
					noop

	go (Created { isDirectory = isd, filePath = f })
		| isd = recurse $ indir f
		| otherwise = do
			ms <- getstatus f
			case ms of
				Just s
					| Files.isSymbolicLink s -> 
						when (hashook addSymlinkHook) $
							runhook addSymlinkHook f ms
					| Files.isRegularFile s ->
						when (hashook addHook) $
							runhook addHook f ms
				_ -> noop
	-- Closing a file is assumed to mean it's done being written,
	-- so a new add event is sent.
	go (Closed { isDirectory = False, maybeFilePath = Just f }) =
			checkfiletype Files.isRegularFile addHook f
	-- When a file or directory is moved in, scan it to add new
	-- stuff.
	go (MovedIn { filePath = f }) = scan f
	go (MovedOut { isDirectory = isd, filePath = f })
		| isd = runhook delDirHook f Nothing
		| otherwise = runhook delHook f Nothing
	-- Verify that the deleted item really doesn't exist,
	-- since there can be spurious deletion events for items
	-- in a directory that has been moved out, but is still
	-- being watched.
	go (Deleted { isDirectory = isd, filePath = f })
		| isd = guarded $ runhook delDirHook f Nothing
		| otherwise = guarded $ runhook delHook f Nothing
	  where
		guarded = unlessM (filetype (const True) f)
	go (Modified { isDirectory = isd, maybeFilePath = Just f })
		| isd = noop
		| otherwise = runhook modifyHook f Nothing
	go _ = noop

	hashook h = isJust $ h hooks

	runhook h f s
		| ignored f = noop
		| otherwise = maybe noop (\a -> a (indir f) s) (h hooks)

	indir f = dir </> f

	getstatus f = catchMaybeIO $ getSymbolicLinkStatus $ indir f
	checkfiletype check h f = do
		ms <- getstatus f
		case ms of
			Just s
				| check s -> runhook h f ms
			_ -> noop
	filetype t f = catchBoolIO $ t <$> getSymbolicLinkStatus (indir f)

	failedaddwatch e
		-- Inotify fails when there are too many watches with a
		-- disk full error.
		| isFullError e =
			case errHook hooks of
				Nothing -> giveup $ "failed to add inotify watch on directory " ++ dir ++ " (" ++ show e ++ ")"
				Just hook -> tooManyWatches hook dir
		-- The directory could have been deleted.
		| isDoesNotExistError e = return ()
		| otherwise = throw e

	failedwatch e = hPutStrLn stderr $ "failed to add watch on directory " ++ dir ++ " (" ++ show e ++ ")"

tooManyWatches :: (String -> Maybe FileStatus -> IO ()) -> FilePath -> IO ()
tooManyWatches hook dir = do
	sysctlval <- querySysctl [Param maxwatches] :: IO (Maybe Integer)
	hook (unlines $ basewarning : maybe withoutsysctl withsysctl sysctlval) Nothing
  where
	maxwatches = "fs.inotify.max_user_watches"
	basewarning = "Too many directories to watch! (Not watching " ++ dir ++")"
	withoutsysctl = ["Increase the value in /proc/sys/fs/inotify/max_user_watches"]
	withsysctl n = let new = n * 10 in
		[ "Increase the limit permanently by running:"
		, "  echo " ++ maxwatches ++ "=" ++ show new ++
		  " | sudo tee -a /etc/sysctl.conf; sudo sysctl -p"
		, "Or temporarily by running:"
		, "  sudo sysctl -w " ++ maxwatches ++ "=" ++ show new
		]

querySysctl :: Read a => [CommandParam] -> IO (Maybe a)
querySysctl ps = getM go ["sysctl", "/sbin/sysctl", "/usr/sbin/sysctl"]
  where
	go p = do
		v <- catchMaybeIO $ readProcess p (toCommand ps)
		case v of
			Nothing -> return Nothing
			Just s -> return $ parsesysctl s
	parsesysctl s = readish =<< lastMaybe (words s)
