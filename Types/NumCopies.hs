{- git-annex numcopies types
 -
 - Copyright 2014-2024 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Types.NumCopies (
	NumCopies,
	configuredNumCopies,
	fromNumCopies,
	MinCopies,
	configuredMinCopies,
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
	safeDropProofEndTime,
	mkSafeDropProof,
	ContentRemovalLock(..),
	p2pDefaultLockContentRetentionDuration,
) where

import Types.UUID
import Types.Key
import Utility.Exception (bracketIO)
import Utility.Monad
import Utility.HumanTime

import qualified Data.Map as M
import Data.Either
import Control.Concurrent.MVar
import Control.Monad.Catch (MonadMask)
import Control.Monad.IO.Class (MonadIO)
import Data.Time.Clock.POSIX (POSIXTime)

newtype NumCopies = NumCopies Int
	deriving (Ord, Eq, Show)

-- Smart constructor; prevent configuring numcopies to 0 which would
-- cause data loss.
configuredNumCopies :: Int -> NumCopies
configuredNumCopies n
	| n > 0 = NumCopies n
	| otherwise = NumCopies 1

fromNumCopies :: NumCopies -> Int
fromNumCopies (NumCopies n) = n

newtype MinCopies = MinCopies Int
	deriving (Ord, Eq, Show)

configuredMinCopies :: Int -> MinCopies
configuredMinCopies n
	| n > 0 = MinCopies n
	| otherwise = MinCopies 1

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
	 - or connection with a remote repository is lost,
	 - the copy is locked in the repository and is guaranteed
	 - not to be removed by any git-annex process. Use
	 - checkVerifiedCopy to detect loss of connection. -}
	| LockedCopy V
	deriving (Show)

data V = V
	{ _getUUID :: UUID
	, _checkVerifiedCopy :: IO (Either POSIXTime Bool)
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

-- Checks that the VerifiedCopy is still valid.
--
-- Invalidation of the VerifiedCopy will make this return False.
-- 
-- When the key is being kept locked by a connection to a remote
-- repository, a detected loss of connection will make this
-- return False. 
--
-- When the connection could possibly break without being detected
-- immediately, this will return a POSIXTime that is how long the
-- content is guaranteed to remain locked on the remote even if the
-- connection has broken.
checkVerifiedCopy :: VerifiedCopy -> IO (Either POSIXTime Bool)
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
mkVerifiedCopy mk u = mk $ V (toUUID u) (return (Right True)) (return ())

invalidatableVerifiedCopy :: ToUUID u => (V -> VerifiedCopy) -> u -> IO (Either POSIXTime Bool) -> IO VerifiedCopy
invalidatableVerifiedCopy mk u check = do
	v <- newEmptyMVar
	let invalidate = do
		_ <- tryPutMVar v ()
		return ()
	let check' = ifM (isEmptyMVar v)
		( check
		, pure (Right False)
		)
	return $ mk $ V (toUUID u) check' invalidate

-- Constructs a VerifiedCopy, and runs the action, ensuring that the
-- verified copy is invalidated when the action returns, or on error.
withVerifiedCopy 
	:: (MonadMask m, MonadIO m, ToUUID u)
	=> (V -> VerifiedCopy)
	-> u
	-> IO (Either POSIXTime Bool)
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
isSafeDrop n m l lck = case safeDropAnalysis n m l lck of
	UnsafeDrop -> False
	SafeDrop -> True
	SafeDropCheckTime -> True

data SafeDropAnalysis
	= UnsafeDrop
	| SafeDrop
	| SafeDropCheckTime

safeDropAnalysis :: NumCopies -> MinCopies -> [VerifiedCopy] -> Maybe ContentRemovalLock -> SafeDropAnalysis
{- When a ContentRemovalLock is provided, the content is being
 - dropped from the local repo. That lock will prevent other git repos
 - that are concurrently dropping from using the local copy as a VerifiedCopy.
 - So, no additional locking is needed; all we need is verifications
 - of any kind of enough other copies of the content. -}
safeDropAnalysis (NumCopies n) (MinCopies m) l (Just (ContentRemovalLock _)) =
	if length (deDupVerifiedCopies l) >= max n m
		then SafeDrop
		else UnsafeDrop
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
safeDropAnalysis (NumCopies n) (MinCopies m) l Nothing
	| n == 0 && m == 0 = SafeDrop
	| length (deDupVerifiedCopies l) >= n 
		&& length (filter fullVerification l) >= m =
			SafeDropCheckTime
	| otherwise = UnsafeDrop

fullVerification :: VerifiedCopy -> Bool
fullVerification (LockedCopy _) = True
fullVerification (TrustedCopy _) = True
fullVerification (RecentlyVerifiedCopy _) = False

-- Content locked using the P2P protocol defaults to being retained,
-- still locked, for 10 minutes after a connection loss.
-- 
-- This is only the case since git-annex 10.20240704, but currently
-- this is used even for older remotes, to avoid a disruptive behavior
-- change when used with remotes running an old version of git-annex.
p2pDefaultLockContentRetentionDuration :: Duration
p2pDefaultLockContentRetentionDuration = Duration (60*10)

-- A proof that it's safe to drop an object.
--
-- It may only be safe up until a given POSIXTime.
data SafeDropProof = SafeDropProof NumCopies MinCopies [VerifiedCopy] (Maybe POSIXTime) (Maybe ContentRemovalLock)
	deriving (Show)

safeDropProofEndTime :: SafeDropProof -> Maybe POSIXTime
safeDropProofEndTime (SafeDropProof _ _ _ t _) = t

-- Makes sure that none of the VerifiedCopies have become invalidated
-- before constructing the proof.
mkSafeDropProof :: NumCopies -> MinCopies -> [VerifiedCopy] -> Maybe ContentRemovalLock -> IO (Either [VerifiedCopy] SafeDropProof)
mkSafeDropProof need mincopies have removallock = do
	l <- mapM checkVerifiedCopy have
	let stillhave = map fst $ 
		filter (either (const True) id . snd) (zip have l)
	return $ case safeDropAnalysis need mincopies stillhave removallock of
		SafeDrop -> Right $
			SafeDropProof need mincopies stillhave Nothing removallock
		SafeDropCheckTime -> Right $
			let endtime = case lefts l of
				[] -> Nothing
				ts -> Just (minimum ts)
			in SafeDropProof need mincopies stillhave endtime removallock
		UnsafeDrop -> Left stillhave
