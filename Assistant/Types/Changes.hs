{- git-annex assistant change tracking
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.Types.Changes where

import Types.KeySource
import Types.Key
import Utility.TSet

import Data.Time.Clock

data ChangeInfo = AddChange Key | LinkChange (Maybe Key) | RmChange | RmDirChange
	deriving (Show, Eq)

changeInfoKey :: ChangeInfo -> Maybe Key
changeInfoKey (AddChange k) = Just k
changeInfoKey (LinkChange (Just k)) = Just k
changeInfoKey _ = Nothing

type ChangeChan = TSet Change

data Change
	= Change 
		{ changeTime :: UTCTime
		, changeFile :: FilePath
		, changeInfo :: ChangeInfo
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

finishedChange :: Change -> Key -> Change
finishedChange c@(InProcessAddChange { keySource = ks }) k = Change
	{ changeTime = changeTime c
	, changeFile = keyFilename ks
	, changeInfo = AddChange k
	}
finishedChange c _ = c
