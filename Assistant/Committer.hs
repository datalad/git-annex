{- git-annex assistant change tracking and committing
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -}

module Assistant.Committer where

import Common.Annex
import Assistant.ThreadedMonad
import qualified Annex.Queue
import qualified Git.Command
import qualified Command.Add
import Utility.ThreadScheduler

import Control.Concurrent.STM
import Data.Time.Clock

data ChangeType = PendingAddChange | LinkChange | RmChange | RmDirChange
	deriving (Show, Eq)

type ChangeChan = TChan Change

data Change = Change
	{ changeTime :: UTCTime
	, changeFile :: FilePath
	, changeType :: ChangeType
	}
	deriving (Show)

runChangeChan :: STM a -> IO a
runChangeChan = atomically

newChangeChan :: IO ChangeChan
newChangeChan = atomically newTChan

{- Handlers call this when they made a change that needs to get committed. -}
madeChange :: FilePath -> ChangeType -> Annex (Maybe Change)
madeChange f t = do
	-- Just in case the commit thread is not flushing the queue fast enough.
	when (t /= PendingAddChange) $
		Annex.Queue.flushWhenFull
	liftIO $ Just <$> (Change <$> getCurrentTime <*> pure f <*> pure t)

noChange :: Annex (Maybe Change)
noChange = return Nothing

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
commitThread :: ThreadState -> ChangeChan -> IO ()
commitThread st changechan = runEvery (Seconds 1) $ do
	-- We already waited one second as a simple rate limiter.
	-- Next, wait until at least one change has been made.
	cs <- getChanges changechan
	-- Now see if now's a good time to commit.
	time <- getCurrentTime
	if shouldCommit time cs
		then do
			handleAdds st changechan cs
			void $ tryIO $ runThreadState st commitStaged
		else refillChanges changechan cs

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
 - Current strategy: If there have been 10 changes within the past second,
 - a batch activity is taking place, so wait for later.
 -}
shouldCommit :: UTCTime -> [Change] -> Bool
shouldCommit now changes
	| len == 0 = False
	| len > 10000 = True -- avoid bloating queue too much
	| length (filter thisSecond changes) < 10 = True
	| otherwise = False -- batch activity
	where
		len = length changes
		thisSecond c = now `diffUTCTime` changeTime c <= 1

{- If there are PendingAddChanges, the files have not yet actually been
 - added to the annex (probably), and that has to be done now, before
 - committing.
 -
 - Deferring the adds to this point causes batches to be bundled together,
 - which allows faster checking with lsof that the files are not still open
 - for write by some other process.
 -
 - When a file is added, Inotify will notice the new symlink. So this waits
 - for additional Changes to arrive, so that the symlink has hopefully been
 - staged before returning, and will be committed.
 -}
handleAdds :: ThreadState -> ChangeChan -> [Change] -> IO ()
handleAdds st changechan cs
	| null toadd = noop
	| otherwise = do
		added <- filter id <$> forM toadd go
		unless (null added) $
			handleAdds st changechan =<< getChanges changechan
	where
		toadd = map changeFile $ filter isPendingAdd cs

		isPendingAdd (Change { changeType = PendingAddChange }) = True
		isPendingAdd _ = False

		go file = do
			ms <- catchMaybeIO $ getSymbolicLinkStatus file
			case ms of
				Just s
					| isRegularFile s -> catchBoolIO $
						runThreadState st $ add file
				_ -> return False

		add file = do
			showStart "add" file
			handle file =<< Command.Add.ingest file

		handle _ Nothing = do
			showEndFail
			return False
		handle file (Just key) = do
			Command.Add.link file key True
			showEndOk
			return True
