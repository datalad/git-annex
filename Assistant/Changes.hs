{- git-annex assistant change tracking
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.Changes where

import Common.Annex
import Types.KeySource
import Utility.TSet

import Data.Time.Clock

data ChangeType = AddChange | LinkChange | RmChange | RmDirChange
	deriving (Show, Eq)

type ChangeChan = TSet Change

data Change
	= Change 
		{ changeTime :: UTCTime
		, changeFile :: FilePath
		, changeType :: ChangeType
		}
	| PendingAddChange
		{ changeTime ::UTCTime
		, changeFile :: FilePath
		}
	| InProcessAddChange
		{ changeTime ::UTCTime
		, keySource :: KeySource
		}
	deriving (Show)

newChangeChan :: IO ChangeChan
newChangeChan = newTSet

{- Handlers call this when they made a change that needs to get committed. -}
madeChange :: FilePath -> ChangeType -> IO (Maybe Change)
madeChange f t = Just <$> (Change <$> getCurrentTime <*> pure f <*> pure t)

noChange :: IO (Maybe Change)
noChange = return Nothing

{- Indicates an add needs to be done, but has not started yet. -}
pendingAddChange :: FilePath -> IO (Maybe Change)
pendingAddChange f = Just <$> (PendingAddChange <$> getCurrentTime <*> pure f)

isPendingAddChange :: Change -> Bool
isPendingAddChange (PendingAddChange {}) = True
isPendingAddChange _ = False

isInProcessAddChange :: Change -> Bool
isInProcessAddChange (InProcessAddChange {}) = True
isInProcessAddChange _ = False

finishedChange :: Change -> Change
finishedChange c@(InProcessAddChange { keySource = ks }) = Change
	{ changeTime = changeTime c
	, changeFile = keyFilename ks
	, changeType = AddChange
	}
finishedChange c = c

{- Gets all unhandled changes.
 - Blocks until at least one change is made. -}
getChanges :: ChangeChan -> IO [Change]
getChanges = getTSet

{- Puts unhandled changes back into the channel.
 - Note: Original order is not preserved. -}
refillChanges :: ChangeChan -> [Change] -> IO ()
refillChanges = putTSet

{- Records a change in the channel. -}
recordChange :: ChangeChan -> Change -> IO ()
recordChange = putTSet1
