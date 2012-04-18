{-# LANGUAGE CPP #-}

module Utility.Inotify where

import Common hiding (isDirectory)
import System.INotify
import qualified System.Posix.Files as Files
import System.Posix.Terminal
import Control.Concurrent.MVar
import System.Posix.Signals

demo :: IO ()
demo = withINotify $ \i -> do
	watchDir i (const True) (Just add) (Just del) "/home/joey/tmp/me"
	putStrLn "started"
	waitForTermination
	where
		add file = putStrLn $ "add " ++ file
		del file = putStrLn $ "del " ++ file

{- Watches for changes to files in a directory, and all its subdirectories
 - that match a test, using inotify. This function returns after its initial
 - setup is complete, leaving a thread running. Then callbacks are made for
 - adding and deleting files.
 -
 - Inotify is weak at recursive directory watching; the whole directory
 - tree must be walked and watches set explicitly for each subdirectory.
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
 - Note: Moving a file may involve deleting it from its old location and
 - adding it to the new location. 
 - 
 - Note: Modification of files is not detected, and it's assumed that when
 - a file that was open for write is closed, it's done being written
 - to, and can be added.
 -
 - Note: inotify has a limit to the number of watches allowed,
 - /proc/sys/fs/inotify/max_user_watches (default 8192).
 - So This will fail if there are too many subdirectories.
 -}
watchDir :: INotify -> (FilePath -> Bool) -> Maybe (FilePath -> IO ()) -> Maybe (FilePath -> IO ()) -> FilePath -> IO ()
watchDir i test add del dir = watchDir' False i test add del dir
watchDir' :: Bool -> INotify -> (FilePath -> Bool) -> Maybe (FilePath -> IO ()) -> Maybe (FilePath -> IO ()) -> FilePath -> IO ()
watchDir' scan i test add del dir = do
	if test dir
		then do
			_ <- addWatch i watchevents dir go
			_ <- mapM walk =<< dirContents dir
			return ()
		else return ()
	where
		watchevents
			| isJust add && isJust del =
				[Create, MoveIn, MoveOut, Delete, CloseWrite]
			| isJust add = [Create, MoveIn, CloseWrite]
			| isJust del = [Create, MoveOut, Delete]
			| otherwise = [Create]

		recurse = watchDir' scan i test add del
		walk f = ifM (catchBoolIO $ Files.isDirectory <$> getFileStatus f)
			( recurse f
			, if scan && isJust add then fromJust add f else return ()
			)

		go (Created { isDirectory = False }) = return ()
		go (Created { filePath = subdir }) = Just recurse <@> subdir
		go (Closed { maybeFilePath = Just f }) = add <@> f
		go (MovedIn { isDirectory = False, filePath = f }) = add <@> f
		go (MovedOut { isDirectory = False, filePath = f }) = del <@> f
		go (Deleted { isDirectory = False, filePath = f }) = del <@> f
		go _ = return ()
		
		Just a <@> f = a $ dir </> f
		Nothing <@> _ = return ()

{- Pauses the main thread, letting children run until program termination. -}
waitForTermination :: IO ()
waitForTermination = do
	mv <- newEmptyMVar
	check softwareTermination mv
	whenM (queryTerminal stdInput) $
		check keyboardSignal mv
	takeMVar mv
	where
		check sig mv = do
			_ <- installHandler sig (CatchOnce $ putMVar mv ()) Nothing
			return ()
