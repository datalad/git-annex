{- git-annex numcopies types
 -
 - Copyright 2014-2021 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Types.NumCopies (
	NumCopies(..),
	fromNumCopies,
	MinCopies(..),
	fromMinCopies,
	VerifiedCopy(..),
	checkVerifiedCopy,
	invalidateVerifiedCopy,
	strongestVerifiedCopy,
	deDupVerifiedCopies,
	mkVerifiedCopy,
	invalidatableVerifiedCopy,
	withVerifiedCopy,
	isSafeDrop,
	SafeDropProof,
	mkSafeDropProof,
	ContentRemovalLock(..),
) where

import Types.UUID
import Types.Key
import Utility.Exception (bracketIO)
import Utility.Monad

import qualified Data.Map as M
import Control.Concurrent.MVar
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad

newtype NumCopies = NumCopies Int
	deriving (Ord, Eq, Show)

fromNumCopies :: NumCopies -> Int
fromNumCopies (NumCopies n) = n

newtype MinCopies = MinCopies Int
	deriving (Ord, Eq, Show)

fromMinCopies :: MinCopies -> Int
fromMinCopies (MinCopies n) = n

-- Indicates that a key's content is exclusively
-- locked locally, pending removal.
newtype ContentRemovalLock = ContentRemovalLock Key
	deriving (Show)

-- A verification that a copy of a key exists in a repository.
data VerifiedCopy
	{- Represents a recent verification that a copy of an
	 - object exists in a repository with the given UUID. -}
	= RecentlyVerifiedCopy V
	{- Use when a repository cannot be accessed, but it's
	 - a trusted repository, which is on record as containing a key
	 - and is presumably not going to lose its copy. -}
	| TrustedCopy V
 	{- The strongest proof of the existence of a copy.
	 - Until its associated action is called to unlock it,
	 - the copy is locked in the repository and is guaranteed
	 - not to be removed by any git-annex process. -}
	| LockedCopy V
	deriving (Show)

data V = V
	{ _getUUID :: UUID
	, _checkVerifiedCopy :: IO Bool
	, _invalidateVerifiedCopy :: IO ()
	}

instance Show V where
	show v = show (_getUUID v)

instance ToUUID VerifiedCopy where
	toUUID = _getUUID . toV
	
toV :: VerifiedCopy -> V
toV (TrustedCopy v) = v
toV (RecentlyVerifiedCopy v) = v
toV (LockedCopy v) = v

-- Checks that it's still valid.
checkVerifiedCopy :: VerifiedCopy -> IO Bool
checkVerifiedCopy = _checkVerifiedCopy . toV

invalidateVerifiedCopy :: VerifiedCopy -> IO ()
invalidateVerifiedCopy = _invalidateVerifiedCopy . toV

strongestVerifiedCopy :: VerifiedCopy -> VerifiedCopy -> VerifiedCopy
strongestVerifiedCopy a@(LockedCopy _) _ = a
strongestVerifiedCopy _ b@(LockedCopy _) = b
strongestVerifiedCopy a@(TrustedCopy _) _ = a
strongestVerifiedCopy _ b@(TrustedCopy _) = b
strongestVerifiedCopy a@(RecentlyVerifiedCopy _) _ = a

-- Retains stronger verifications over weaker for the same uuid.
deDupVerifiedCopies :: [VerifiedCopy] -> [VerifiedCopy]
deDupVerifiedCopies l = M.elems $
	M.fromListWith strongestVerifiedCopy (zip (map toUUID l) l)

mkVerifiedCopy :: ToUUID u => (V -> VerifiedCopy) -> u -> VerifiedCopy
mkVerifiedCopy mk u = mk $ V (toUUID u) (return True) (return ())

invalidatableVerifiedCopy :: ToUUID u => (V -> VerifiedCopy) -> u -> IO Bool -> IO VerifiedCopy
invalidatableVerifiedCopy mk u check = do
	v <- newEmptyMVar
	let invalidate = do
		_ <- tryPutMVar v ()
		return ()
	let check' = isEmptyMVar v <&&> check
	return $ mk $ V (toUUID u) check' invalidate

-- Constructs a VerifiedCopy, and runs the action, ensuring that the
-- verified copy is invalidated when the action returns, or on error.
withVerifiedCopy 
	:: (MonadMask m, MonadIO m, ToUUID u)
	=> (V -> VerifiedCopy)
	-> u
	-> IO Bool
	-> (VerifiedCopy -> m a)
	-> m a
withVerifiedCopy mk u check = bracketIO setup cleanup
  where
	setup = invalidatableVerifiedCopy mk u check
	cleanup = invalidateVerifiedCopy

{- Check whether enough verification has been done of copies to allow
 - dropping content safely.
 -
 - This is carefully balanced to prevent data loss when there are races
 - between concurrent drops of the same content in different repos,
 - without requiring impractical amounts of locking.
 -
 - In particular, concurrent drop races may cause the number of copies
 - to fall below NumCopies, but it will never fall below MinCopies.
 -}
isSafeDrop :: NumCopies -> MinCopies -> [VerifiedCopy] -> Maybe ContentRemovalLock -> Bool
{- When a ContentRemovalLock is provided, the content is being
 - dropped from the local repo. That lock will prevent other git repos
 - that are concurrently dropping from using the local copy as a VerifiedCopy.
 - So, no additional locking is needed; all we need is verifications
 - of any kind of N other copies of the content. -}
isSafeDrop (NumCopies n) _ l (Just (ContentRemovalLock _)) = 
	length (deDupVerifiedCopies l) >= n
{- Dropping from a remote repo.
 -
 - To guarantee MinCopies is never violated, at least that many LockedCopy
 - or TrustedCopy are required. A LockedCopy prevents races between
 - concurrent drops from dropping the last copy, no matter what.
 -
 - The other copies required by NumCopies can be less strong verifications,
 - like RecentlyVerifiedCopy. While those are subject to concurrent drop
 - races, and so could be dropped all at once, causing NumCopies to be
 - violated, this is the best that can be done without requiring that 
 - all special remotes support locking.
 -}
isSafeDrop (NumCopies n) (MinCopies m) l Nothing
	| n == 0 && m == 0 = True
	| otherwise = and
		[ length (deDupVerifiedCopies l) >= n
		, length (filter fullVerification l) >= m
		]

fullVerification :: VerifiedCopy -> Bool
fullVerification (LockedCopy _) = True
fullVerification (TrustedCopy _) = True
fullVerification (RecentlyVerifiedCopy _) = False

-- A proof that it's currently safe to drop an object.
data SafeDropProof = SafeDropProof NumCopies MinCopies [VerifiedCopy] (Maybe ContentRemovalLock)
	deriving (Show)

-- Makes sure that none of the VerifiedCopies have become invalidated
-- before constructing proof.
mkSafeDropProof :: NumCopies -> MinCopies -> [VerifiedCopy] -> Maybe ContentRemovalLock -> IO (Either [VerifiedCopy] SafeDropProof)
mkSafeDropProof need mincopies have removallock = do
	stillhave <- filterM checkVerifiedCopy have
	return $ if isSafeDrop need mincopies stillhave removallock
		then Right (SafeDropProof need mincopies stillhave removallock)
		else Left stillhave
