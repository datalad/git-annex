{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}

{- git-annex watch daemon
 -
 - Overview of threads and MVars, etc:
 -
 - Thread 1: Parent
 - 	The initial thread run, double forks to background, starts other
 - 	threads, and then stops, waiting for them to terminate.
 - Thread 2: inotify
 - 	Notices new files, and calls handlers for events, queuing changes.
 - Thread 3: inotify internal
 - 	Used by haskell inotify library to ensure inotify event buffer is
 - 	kept drained.
 - Thread 4: committer
 - 	Waits for changes to occur, and runs the git queue to update its
 - 	index, then commits.
 -
 - State MVar:
 - 	The Annex state is stored here, which allows recuscitating the
 - 	Annex monad in IO actions run by the inotify and committer
 - 	threads. Thus, a single state is shared amoung the threads, and
 - 	only one at a time can access it.
 - ChangeChan STM TChan:
 - 	Changes are indicated by writing to this channel. The committer
 - 	reads from it.
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Watch where

import Common.Annex
import Command
import Utility.Daemon
import Utility.LogFile
import Utility.ThreadLock
import qualified Annex
import qualified Annex.Queue
import qualified Command.Add
import qualified Git.Command
import qualified Git.UpdateIndex
import qualified Git.HashObject
import qualified Backend
import Annex.Content
import Annex.CatFile
import Git.Types
import Option

import Control.Concurrent
import Control.Concurrent.STM
import Data.Time.Clock
import Data.Bits.Utils
import qualified Data.ByteString.Lazy as L

#if defined linux_HOST_OS
import Utility.Inotify
import System.INotify
#endif

type ChangeChan = TChan Change

type Handler = FilePath -> Annex (Maybe Change)

data Change = Change
	{ changeTime :: UTCTime
	, changeFile :: FilePath
	, changeDesc :: String
	}
	deriving (Show)

def :: [Command]
def = [withOptions [foregroundOption, stopOption] $ 
	command "watch" paramPaths seek "watch for changes"]

seek :: [CommandSeek]
seek = [withFlag stopOption $ \stopdaemon -> 
	withFlag foregroundOption $ \foreground ->
	withNothing $ start foreground stopdaemon]

foregroundOption :: Option
foregroundOption = Option.flag [] "foreground" "do not daemonize"

stopOption :: Option
stopOption = Option.flag [] "stop" "stop daemon"

start :: Bool -> Bool -> CommandStart
start foreground stopdaemon = notBareRepo $ do
	if stopdaemon
		then liftIO . stopDaemon =<< fromRepo gitAnnexPidFile
		else withStateMVar $ startDaemon (not foreground)
	stop

startDaemon :: Bool -> MVar Annex.AnnexState -> Annex ()
startDaemon False st = do
	showStart "watch" "."
	liftIO $ watch st
startDaemon True st = do
	logfd <- liftIO . openLog =<< fromRepo gitAnnexLogFile
	pidfile <- fromRepo gitAnnexPidFile
	liftIO $ daemonize logfd (Just pidfile) False $ watch st

watch :: MVar Annex.AnnexState -> IO ()
#if defined linux_HOST_OS
watch st = withINotify $ \i -> do
	changechan <- runChangeChan newTChan
	let hook a = Just $ runHandler st changechan a
	let hooks = WatchHooks
		{ addHook = hook onAdd
		, delHook = hook onDel
		, addSymlinkHook = hook onAddSymlink
		, delDirHook = hook onDelDir
		, errHook = hook onErr
		}
	-- The commit thread is started early, so that the user
	-- can immediately begin adding files and having them
	-- committed, even while the inotify scan is taking place.
	_ <- forkIO $ commitThread st changechan
	-- This does not return until the inotify scan is done.
	-- That can take some time for large trees.
	watchDir i "." (ignored . takeFileName) hooks
	runStateMVar st $ showAction "scanning"
	-- Notice any files that were deleted before inotify
	-- was started.
	runStateMVar st $ do
		inRepo $ Git.Command.run "add" [Param "--update"]
		showAction "started"
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
runStateMVar :: MVar Annex.AnnexState -> Annex a -> IO a
runStateMVar mvar a = do
	liftIO $ putStrLn "takeMVar"
	startstate <- takeMVar mvar
	!(r, newstate) <- Annex.run startstate a
	liftIO $ putStrLn "putMVar"
	putMVar mvar newstate
	return r

runChangeChan :: STM a -> IO a
runChangeChan = atomically

{- Runs an action handler, inside the Annex monad, and if there was a
 - change, adds it to the ChangeChan.
 -
 - Exceptions are ignored, otherwise a whole watcher thread could be crashed.
 -}
runHandler :: MVar Annex.AnnexState -> ChangeChan -> Handler -> FilePath -> IO ()
runHandler st changechan handler file = void $ do
	r <- tryIO (runStateMVar st $ handler file)
	case r of
		Left e -> putStrLn $ show e
		Right Nothing -> noop
		Right (Just change) -> void $
			runChangeChan $ writeTChan changechan change

{- Handlers call this when they made a change that needs to get committed. -}
madeChange :: FilePath -> String -> Annex (Maybe Change)
madeChange file desc = do
	-- Just in case the commit thread is not flushing the queue fast enough.
	Annex.Queue.flushWhenFull
	liftIO $ Just <$> (Change <$> getCurrentTime <*> pure file <*> pure desc)

noChange :: Annex (Maybe Change)
noChange = return Nothing

{- Adding a file is tricky; the file has to be replaced with a symlink
 - but this is race prone, as the symlink could be changed immediately
 - after creation. To avoid that race, git add is not used to stage the
 - symlink.
 -
 - Inotify will notice the new symlink, so this Handler does not stage it
 - or return a Change, leaving that to onAddSymlink.
 -}
onAdd :: Handler
onAdd file = do
	showStart "add" file
	handle =<< Command.Add.ingest file
	noChange
	where
		handle Nothing = showEndFail
		handle (Just key) = do
			Command.Add.link file key True
			showEndOk

{- A symlink might be an arbitrary symlink, which is just added.
 - Or, if it is a git-annex symlink, ensure it points to the content
 - before adding it.
 -}
onAddSymlink :: Handler
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
		{- This is often called on symlinks that are already staged
		 - correctly, especially during the startup scan. A symlink
		 - may have been deleted and re-added, or added when
		 - the watcher was not running; so it always stages
		 - even symlinks that already exist.
		 -
		 - So for speed, tries to reuse the existing blob for
		 - the symlink target. -}
		addlink link = do
			v <- catObjectDetails $ Ref $ ":" ++ file
			case v of
				Just (currlink, sha)
					| s2w8 link == L.unpack currlink ->
						stageSymlink file sha
				_ -> do
					sha <- inRepo $
						Git.HashObject.hashObject BlobObject link
					stageSymlink file sha
			madeChange file "link"

onDel :: Handler
onDel file = do
	Annex.Queue.addUpdateIndex =<<
		inRepo (Git.UpdateIndex.unstageFile file)
	madeChange file "rm"

{- A directory has been deleted, or moved, so tell git to remove anything
 - that was inside it from its cache. Since it could reappear at any time,
 - use --cached to only delete it from the index. 
 -
 - Note: This could use unstageFile, but would need to run another git
 - command to get the recursive list of files in the directory, so rm is
 - just as good. -}
onDelDir :: Handler
onDelDir dir = do
	Annex.Queue.addCommand "rm"
		[Params "--quiet -r --cached --ignore-unmatch --"] [dir]
	madeChange dir "rmdir"

{- Called when there's an error with inotify. -}
onErr :: Handler
onErr msg = do
	warning msg
	return Nothing

{- Adds a symlink to the index, without ever accessing the actual symlink
 - on disk. -}
stageSymlink :: FilePath -> Sha -> Annex ()
stageSymlink file sha =
	Annex.Queue.addUpdateIndex =<<
		inRepo (Git.UpdateIndex.stageSymlink file sha)

{- Gets all unhandled changes.
 - Blocks until at least one change is made. -}
getChanges :: ChangeChan -> IO [Change]
getChanges chan = runChangeChan $ do
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
refillChanges :: ChangeChan -> [Change] -> IO ()
refillChanges chan cs = runChangeChan $ mapM_ (writeTChan chan) cs

{- This thread makes git commits at appropriate times. -}
commitThread :: MVar Annex.AnnexState -> ChangeChan -> IO ()
commitThread st changechan = forever $ do
	-- First, a simple rate limiter.
	threadDelay oneSecond
	-- Next, wait until at least one change has been made.
	cs <- getChanges changechan
	-- Now see if now's a good time to commit.
	time <- getCurrentTime
	if shouldCommit time cs
		then void $ tryIO $ runStateMVar st $ commitStaged
		else refillChanges changechan cs
	where
		oneSecond = 1000000 -- microseconds

commitStaged :: Annex ()
commitStaged = do
	Annex.Queue.flush
	inRepo $ Git.Command.run "commit"
		[ Param "--allow-empty-message"
		, Param "-m", Param ""
		-- Empty commits may be made if tree changes cancel
		-- each other out, etc
		, Param "--allow-empty"
		-- Avoid running the usual git-annex pre-commit hook;
		-- watch does the same symlink fixing, and we don't want
		-- to deal with unlocked files in these commits.
		, Param "--quiet"
		]

{- Decide if now is a good time to make a commit.
 - Note that the list of change times has an undefined order.
 -
 - Current strategy: If there have been 10 commits within the past second,
 - a batch activity is taking place, so wait for later.
 -}
shouldCommit :: UTCTime -> [Change] -> Bool
shouldCommit now changes
	| len == 0 = False
	| len > 4096 = True -- avoid bloating queue too much
	| length (filter thisSecond changes) < 10 = True
	| otherwise = False -- batch activity
	where
		len = length changes
		thisSecond c = now `diffUTCTime` changeTime c <= 1
