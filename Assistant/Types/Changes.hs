{- git-annex assistant change tracking
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.Types.Changes where

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

