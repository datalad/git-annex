{- git-annex assistant change tracking
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -}

module Assistant.Changes where

import Common.Annex
import qualified Annex.Queue

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
