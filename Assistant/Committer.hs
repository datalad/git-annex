{- git-annex assistant change tracking and committing
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -}

module Assistant.Committer where

import Common.Annex
import Assistant.ThreadedMonad
import qualified Annex.Queue
import qualified Git.Command
import Utility.ThreadScheduler

import Control.Concurrent.STM
import Data.Time.Clock

type ChangeChan = TChan Change

data Change = Change
	{ changeTime :: UTCTime
	, changeFile :: FilePath
	, changeDesc :: String
	}
	deriving (Show)

runChangeChan :: STM a -> IO a
runChangeChan = atomically

newChangeChan :: IO ChangeChan
newChangeChan = atomically newTChan

{- Handlers call this when they made a change that needs to get committed. -}
madeChange :: FilePath -> String -> Annex (Maybe Change)
madeChange file desc = do
	-- Just in case the commit thread is not flushing the queue fast enough.
	Annex.Queue.flushWhenFull
	liftIO $ Just <$> (Change <$> getCurrentTime <*> pure file <*> pure desc)

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
		then void $ tryIO $ runThreadState st commitStaged
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
 - Current strategy: If there have been 10 commits within the past second,
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
