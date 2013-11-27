{- git-annex assistant change tracking
 -
 - Copyright 2012-2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.Types.Changes where

import Types.KeySource
import Types.Key
import Utility.TList

import Control.Concurrent.STM
import Data.Time.Clock

{- An un-ordered pool of Changes that have been noticed and should be
 - staged and committed. Changes will typically be in order, but ordering
 - may be lost. In any case, order should not matter, as any given Change
 - may later be reverted by a later Change (ie, a file is added and then
 - deleted). Code that processes the changes needs to deal with such
 - scenarios.
 -}
type ChangePool = TList Change

newChangePool :: IO ChangePool
newChangePool = atomically newTList

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

data ChangeInfo = AddKeyChange Key | AddFileChange | LinkChange (Maybe Key) | RmChange
	deriving (Show, Eq, Ord)

changeInfoKey :: ChangeInfo -> Maybe Key
changeInfoKey (AddKeyChange k) = Just k
changeInfoKey (LinkChange (Just k)) = Just k
changeInfoKey _ = Nothing

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

retryChange :: Change -> Change
retryChange (InProcessAddChange time ks) =
	PendingAddChange time (keyFilename ks)
retryChange c = c

finishedChange :: Change -> Key -> Change
finishedChange c@(InProcessAddChange { keySource = ks }) k = Change
	{ changeTime = changeTime c
	, _changeFile = keyFilename ks
	, changeInfo = AddKeyChange k
	}
finishedChange c _ = c
