{- git-annex numcopies types
 -
 - Copyright 2014 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Types.NumCopies where

import Types.UUID

import qualified Data.Map as M

newtype NumCopies = NumCopies Int
	deriving (Ord, Eq)

fromNumCopies :: NumCopies -> Int
fromNumCopies (NumCopies n) = n

data VerifiedCopy
	{- Use when a repository cannot be accessed, but it's
	 - a trusted repository, which is presumably not going to
	 - lose a copy. This is the weakest level of verification. -}
	= TrustedCopy UUID
	{- Represents a recent verification that a copy of an
	 - object exists in a repository with the given UUID. -}
	| VerifiedCopy UUID
 	{- The strongest proof of the existence of a copy.
	 - Until its associated action is called to unlock it,
	 - the copy is locked in the repository and is guaranteed
	 - not to be dropped by any git-annex process. -}
	| VerifiedCopyLock UUID (IO ())

instance ToUUID VerifiedCopy where
	toUUID (VerifiedCopy u) = u
	toUUID (VerifiedCopyLock u _) = u
	toUUID (TrustedCopy u) = u

instance Show VerifiedCopy where
	show (TrustedCopy u) = "TrustedCopy " ++ show u
	show (VerifiedCopy u) = "VerifiedCopy " ++ show u
	show (VerifiedCopyLock u _) = "VerifiedCopyLock " ++ show u

strongestVerifiedCopy :: VerifiedCopy -> VerifiedCopy -> VerifiedCopy
strongestVerifiedCopy a@(VerifiedCopyLock _ _) _ = a
strongestVerifiedCopy _ b@(VerifiedCopyLock _ _) = b
strongestVerifiedCopy a@(VerifiedCopy _) _ = a
strongestVerifiedCopy _ b@(VerifiedCopy _) = b
strongestVerifiedCopy a@(TrustedCopy _) _  = a

-- Retains stronger verifications over weaker for the same uuid.
deDupVerifiedCopies :: [VerifiedCopy] -> [VerifiedCopy]
deDupVerifiedCopies l = M.elems $
	M.fromListWith strongestVerifiedCopy (zip (map toUUID l) l)
