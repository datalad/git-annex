{- git-annex numcopies types
 -
 - Copyright 2014 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Types.NumCopies (
	NumCopies(..),
	fromNumCopies,
	VerifiedCopy(..),
	checkVerifiedCopy,
	invalidateVerifiedCopy,
	strongestVerifiedCopy,
	deDupVerifiedCopies,
	mkVerifiedCopy,
	invalidatableVerifiedCopy,
) where

import Types.UUID

import qualified Data.Map as M
import Control.Concurrent.MVar

newtype NumCopies = NumCopies Int
	deriving (Ord, Eq)

fromNumCopies :: NumCopies -> Int
fromNumCopies (NumCopies n) = n

-- A verification that a copy of a key exists in a repository.
data VerifiedCopy
	{- Use when a repository cannot be accessed, but it's
	 - a trusted repository, which is on record as containing a key
	 - and is presumably not going to lose its copy. 
	 - This is the weakest level of verification. -}
	= TrustedCopy V
	{- Represents a recent verification that a copy of an
	 - object exists in a repository with the given UUID. -}
	| RecentlyVerifiedCopy V
 	{- The strongest proof of the existence of a copy.
	 - Until its associated action is called to unlock it,
	 - the copy is locked in the repository and is guaranteed
	 - not to be dropped by any git-annex process. -}
	| VerifiedCopyLock V
	deriving (Show)

instance ToUUID VerifiedCopy where
	toUUID = _getUUID . toV
	
toV :: VerifiedCopy -> V
toV (TrustedCopy v) = v
toV (RecentlyVerifiedCopy v) = v
toV (VerifiedCopyLock v) = v

-- Checks that it's still valid.
checkVerifiedCopy :: VerifiedCopy -> IO Bool
checkVerifiedCopy = _checkVerifiedCopy . toV

invalidateVerifiedCopy :: VerifiedCopy -> IO ()
invalidateVerifiedCopy = _invalidateVerifiedCopy . toV

data V = V
	{ _getUUID :: UUID
	, _checkVerifiedCopy :: IO Bool
	, _invalidateVerifiedCopy :: IO ()
	}

instance Show V where
	show v = show (_getUUID v)

strongestVerifiedCopy :: VerifiedCopy -> VerifiedCopy -> VerifiedCopy
strongestVerifiedCopy a@(VerifiedCopyLock _) _ = a
strongestVerifiedCopy _ b@(VerifiedCopyLock _) = b
strongestVerifiedCopy a@(RecentlyVerifiedCopy _) _ = a
strongestVerifiedCopy _ b@(RecentlyVerifiedCopy _) = b
strongestVerifiedCopy a@(TrustedCopy _) _  = a

-- Retains stronger verifications over weaker for the same uuid.
deDupVerifiedCopies :: [VerifiedCopy] -> [VerifiedCopy]
deDupVerifiedCopies l = M.elems $
	M.fromListWith strongestVerifiedCopy (zip (map toUUID l) l)

mkVerifiedCopy :: ToUUID u => (V -> VerifiedCopy) -> u -> VerifiedCopy
mkVerifiedCopy mk u = mk $ V (toUUID u) (return True) (return ())

invalidatableVerifiedCopy :: ToUUID u => (V -> VerifiedCopy) -> u -> IO VerifiedCopy
invalidatableVerifiedCopy mk u = do
	v <- newEmptyMVar
	let invalidate = do
		_ <- tryPutMVar v ()
		return ()
	let check = isEmptyMVar v
	return $ mk $ V (toUUID u) check invalidate
