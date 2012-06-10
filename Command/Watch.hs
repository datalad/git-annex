{- git-annex command
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}

module Command.Watch where

import Common.Annex
import Command
import Utility.ThreadLock
import qualified Annex
import qualified Annex.Queue
import qualified Command.Add
import qualified Git.Command
import qualified Git.UpdateIndex
import qualified Backend
import Annex.Content

import Control.Concurrent
import Control.Concurrent.STM
import Data.Time.Clock

#if defined linux_HOST_OS
import Utility.Inotify
import System.INotify
#endif

type ChangeChan = TChan UTCTime

def :: [Command]
def = [command "watch" paramPaths seek "watch for changes"]

seek :: [CommandSeek]
seek = [withNothing start]

start :: CommandStart
start = notBareRepo $ do
	showStart "watch" "."
	watch
	stop

watch :: Annex ()
#if defined linux_HOST_OS
watch = do
	showAction "scanning"
	inRepo $ Git.Command.run "add" [Param "--update"]
	withStateMVar $ \st -> liftIO $ withINotify $ \i -> do
		changechan <- atomically newTChan
		_ <- forkIO $ commitThread st changechan
		let hook a = Just $ runHook st changechan a
		let hooks = WatchHooks
			{ addHook = hook onAdd
			, delHook = hook onDel
			, addSymlinkHook = hook onAddSymlink
			, delDirHook = hook onDelDir
			, errHook = hook onErr
			}
		watchDir i "." (ignored . takeFileName) hooks
		putStrLn "(started)"
		waitForTermination
#else
watch = error "watch mode is so far only available on Linux"
#endif

ignored :: FilePath -> Bool
ignored ".git" = True
ignored ".gitignore" = True
ignored ".gitattributes" = True
ignored _ = False

{- Stores the Annex state in a MVar, so that threaded actions can access
 - it.
 -
 - Once the action is finished, retrieves the state from the MVar.
 -}
withStateMVar :: (MVar Annex.AnnexState -> Annex a) -> Annex a
withStateMVar a = do
	state <- Annex.getState id
	mvar <- liftIO $ newMVar state
	r <- a mvar
	newstate <- liftIO $ takeMVar mvar
	Annex.changeState (const newstate)
	return r

{- Runs an Annex action, using the state from the MVar. -}
runStateMVar :: MVar Annex.AnnexState -> Annex () -> IO ()
runStateMVar mvar a = do
	startstate <- takeMVar mvar
	!newstate <- Annex.exec startstate a
	putMVar mvar newstate

{- Runs a hook, inside the Annex monad.
 -
 - Exceptions are ignored, otherwise a whole watcher thread could be crashed.
 -}
runHook :: MVar Annex.AnnexState -> ChangeChan -> (FilePath -> Annex ()) -> FilePath -> IO ()
runHook st changetimes a f = handle =<< tryIO (runStateMVar st go)
	where
		go = do
			a f
			signalChange changetimes
		handle (Right ()) = return ()
		handle (Left e) = putStrLn $ show e

{- Adding a file is tricky; the file has to be replaced with a symlink
 - but this is race prone, as the symlink could be changed immediately
 - after creation. To avoid that race, git add is not used to stage the
 - symlink. -}
onAdd :: FilePath -> Annex ()
onAdd file = do
	showStart "add" file
	Command.Add.ingest file >>= go
	where
		go Nothing = showEndFail
		go (Just key) = do
			link <- Command.Add.link file key True
			stageSymlink file link
			showEndOk

{- A symlink might be an arbitrary symlink, which is just added.
 - Or, if it is a git-annex symlink, ensure it points to the content
 - before adding it.
 -}
onAddSymlink :: FilePath -> Annex ()
onAddSymlink file = go =<< Backend.lookupFile file
	where
		go Nothing = addlink =<< liftIO (readSymbolicLink file)
		go (Just (key, _)) = do
			link <- calcGitLink file key
			ifM ((==) link <$> liftIO (readSymbolicLink file))
				( addlink link
				, do
					liftIO $ removeFile file
					liftIO $ createSymbolicLink link file
					addlink link
				)
		addlink link = stageSymlink file link

onDel :: FilePath -> Annex ()
onDel file = Annex.Queue.addUpdateIndex =<<
	inRepo (Git.UpdateIndex.unstageFile file)

{- A directory has been deleted, or moved, so tell git to remove anything
 - that was inside it from its cache. Since it could reappear at any time,
 - use --cached to only delete it from the index. 
 -
 - Note: This could use unstageFile, but would need to run another git
 - command to get the recursive list of files in the directory, so rm is
 - just as good. -}
onDelDir :: FilePath -> Annex ()
onDelDir dir = Annex.Queue.addCommand "rm"
	[Params "--quiet -r --cached --ignore-unmatch --"] [dir]

{- Called when there's an error with inotify. -}
onErr :: String -> Annex ()
onErr = warning

{- Adds a symlink to the index, without ever accessing the actual symlink
 - on disk. -}
stageSymlink :: FilePath -> String -> Annex ()
stageSymlink file linktext =
	Annex.Queue.addUpdateIndex =<<
		inRepo (Git.UpdateIndex.stageSymlink file linktext)

{- Signals that a change has been made, that needs to get committed. -}
signalChange :: ChangeChan -> Annex ()
signalChange chan = do
	liftIO $ (atomically . writeTChan chan) =<< getCurrentTime
	-- Just in case the commit thread is not flushing
	-- the queue fast enough.
	Annex.Queue.flushWhenFull

{- Gets the times of all unhandled changes.
 - Blocks until at least one change is made. -}
getChanges :: ChangeChan -> IO [UTCTime]
getChanges chan = atomically $ do
	c <- readTChan chan
	go [c]
	where
		go l = do
			v <- tryReadTChan chan
			case v of
				Nothing -> return l
				Just c -> go (c:l)

{- Puts unhandled changes back into the channel.
 - Note: Original order is not preserved. -}
refillChanges :: ChangeChan -> [UTCTime] -> IO ()
refillChanges chan cs = atomically $ mapM_ (writeTChan chan) cs

{- This thread makes git commits. -}
commitThread :: MVar Annex.AnnexState -> ChangeChan -> IO ()
commitThread st changechan = forever $ do
	-- First, a simple rate limiter.
	threadDelay oneSecond
	-- Next, wait until at least one change has been made.
	cs <- getChanges changechan
	-- Now see if now's a good time to commit.
	time <- getCurrentTime
	if shouldCommit time cs
		then commit
		else refillChanges changechan cs
	where
		commit = void $ tryIO $ runStateMVar st $ do
			Annex.Queue.flush
			inRepo $ Git.Command.run "commit"
				[ Param "--allow-empty-message"
				, Param "-m", Param ""
				-- Empty commits may be made if tree
				-- changes cancel each other out, etc
				, Param "--allow-empty"
				-- Avoid running the usual git-annex
				-- pre-commit hook; watch does the same
				-- symlink fixing, and we don't want to
				-- deal with unlocked files in these
				-- commits.
				, Param "--quiet"
				]
		oneSecond = 1000000 -- microseconds

{- Decide if now is a good time to make a commit.
 - Note that the list of change times has an undefined order.
 -
 - Current strategy: If there have been 10 commits within the past second,
 - a batch activity is taking place, so wait for later.
 -}
shouldCommit :: UTCTime -> [UTCTime] -> Bool
shouldCommit now changetimes
	| len == 0 = False
	| len > 4096 = True -- avoid bloating queue too much
	| length (filter thisSecond changetimes) < 10 = True
	| otherwise = False -- batch activity
	where
		len = length changetimes
		thisSecond t = now `diffUTCTime` t <= 1
