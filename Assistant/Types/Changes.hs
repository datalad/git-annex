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
import Control.Concurrent.STM

data ChangeInfo = AddKeyChange Key | AddFileChange | LinkChange (Maybe Key) | RmChange
	deriving (Show, Eq)

changeInfoKey :: ChangeInfo -> Maybe Key
changeInfoKey (AddKeyChange k) = Just k
changeInfoKey (LinkChange (Just k)) = Just k
changeInfoKey _ = Nothing

type ChangeChan = TSet [Change]

newChangeChan :: IO ChangeChan
newChangeChan = atomically newTSet

data Change
	= Change 
		{ changeTime :: UTCTime
		, _changeFile :: FilePath
		, changeInfo :: ChangeInfo
		}
	| PendingAddChange
		{ changeTime ::UTCTime
		, _changeFile :: FilePath
		}
	| InProcessAddChange
		{ changeTime ::UTCTime
		, keySource :: KeySource
		}
	deriving (Show)

changeFile :: Change -> FilePath
changeFile (Change _ f _) = f
changeFile (PendingAddChange _ f) = f
changeFile (InProcessAddChange _ ks) = keyFilename ks

isPendingAddChange :: Change -> Bool
isPendingAddChange (PendingAddChange {}) = True
isPendingAddChange _ = False

isInProcessAddChange :: Change -> Bool
isInProcessAddChange (InProcessAddChange {}) = True
isInProcessAddChange _ = False

finishedChange :: Change -> Key -> Change
finishedChange c@(InProcessAddChange { keySource = ks }) k = Change
	{ changeTime = changeTime c
	, _changeFile = keyFilename ks
	, changeInfo = AddKeyChange k
	}
finishedChange c _ = c
