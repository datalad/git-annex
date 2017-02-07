{- git-annex remotes types
 -
 - Most things should not need this, using Types instead
 -
 - Copyright 2011-2014 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE RankNTypes #-}

module Types.Remote
	( RemoteConfigKey
	, RemoteConfig
	, RemoteTypeA(..)
	, RemoteA(..)
	, SetupStage(..)
	, Availability(..)
	, Verification(..)
	, unVerified
	)
	where

import Data.Map as M
import Data.Ord

import qualified Git
import Types.Key
import Types.UUID
import Types.GitConfig
import Types.Availability
import Types.Creds
import Types.UrlContents
import Types.NumCopies
import Config.Cost
import Utility.Metered
import Git.Types
import Utility.SafeCommand
import Utility.Url

type RemoteConfigKey = String

type RemoteConfig = M.Map RemoteConfigKey String

data SetupStage = Init | Enable
	deriving (Eq)

{- There are different types of remotes. -}
data RemoteTypeA a = RemoteType {
	-- human visible type name
	typename :: String,
	-- enumerates remotes of this type
	-- The Bool is True if automatic initialization of remotes is desired
	enumerate :: Bool -> a [Git.Repo],
	-- generates a remote of this type
	generate :: Git.Repo -> UUID -> RemoteConfig -> RemoteGitConfig -> a (Maybe (RemoteA a)),
	-- initializes or enables a remote
	setup :: SetupStage -> Maybe UUID -> Maybe CredPair -> RemoteConfig -> RemoteGitConfig -> a (RemoteConfig, UUID)
}

instance Eq (RemoteTypeA a) where
	x == y = typename x == typename y

{- An individual remote. -}
data RemoteA a = Remote {
	-- each Remote has a unique uuid
	uuid :: UUID,
	-- each Remote has a human visible name
	name :: RemoteName,
	-- Remotes have a use cost; higher is more expensive
	cost :: Cost,
	-- Transfers a key's contents from disk to the remote.
	-- The key should not appear to be present on the remote until
	-- all of its contents have been transferred.
	storeKey :: Key -> AssociatedFile -> MeterUpdate -> a Bool,
	-- Retrieves a key's contents to a file.
	-- (The MeterUpdate does not need to be used if it writes
	-- sequentially to the file.)
	retrieveKeyFile :: Key -> AssociatedFile -> FilePath -> MeterUpdate -> a (Bool, Verification),
	-- Retrieves a key's contents to a tmp file, if it can be done cheaply.
	-- It's ok to create a symlink or hardlink.
	retrieveKeyFileCheap :: Key -> AssociatedFile -> FilePath -> a Bool,
	-- Removes a key's contents (succeeds if the contents are not present)
	removeKey :: Key -> a Bool,
	-- Uses locking to prevent removal of a key's contents,
	-- thus producing a VerifiedCopy, which is passed to the callback.
	-- If unable to lock, does not run the callback, and throws an
	-- error.
	-- This is optional; remotes do not have to support locking.
	lockContent :: forall r. Maybe (Key -> (VerifiedCopy -> a r) -> a r),
	-- Checks if a key is present in the remote.
	-- Throws an exception if the remote cannot be accessed.
	checkPresent :: Key -> a Bool,
	-- Some remotes can checkPresent without an expensive network
	-- operation.
	checkPresentCheap :: Bool,
	-- Some remotes can provide additional details for whereis.
	whereisKey :: Maybe (Key -> a [String]),
	-- Some remotes can run a fsck operation on the remote,
	-- without transferring all the data to the local repo
	-- The parameters are passed to the fsck command on the remote.
	remoteFsck :: Maybe ([CommandParam] -> a (IO Bool)),
	-- Runs an action to repair the remote's git repository.
	repairRepo :: Maybe (a Bool -> a (IO Bool)),
	-- a Remote has a persistent configuration store
	config :: RemoteConfig,
	-- git repo for the Remote
	repo :: Git.Repo,
	-- a Remote's configuration from git
	gitconfig :: RemoteGitConfig,
	-- a Remote can be assocated with a specific local filesystem path
	localpath :: Maybe FilePath,
	-- a Remote can be known to be readonly
	readonly :: Bool,
	-- a Remote can be globally available. (Ie, "in the cloud".)
	availability :: Availability,
	-- the type of the remote
	remotetype :: RemoteTypeA a,
	-- For testing, makes a version of this remote that is not
	-- available for use. All its actions should fail.
	mkUnavailable :: a (Maybe (RemoteA a)),
	-- Information about the remote, for git annex info to display.
	getInfo :: a [(String, String)],
	-- Some remotes can download from an url (or uri).
	claimUrl :: Maybe (URLString -> a Bool),
	-- Checks that the url is accessible, and gets information about
	-- its contents, without downloading the full content.
	-- Throws an exception if the url is inaccessible.
	checkUrl :: Maybe (URLString -> a UrlContents)
}

instance Show (RemoteA a) where
	show remote = "Remote { name =\"" ++ name remote ++ "\" }"

-- two remotes are the same if they have the same uuid
instance Eq (RemoteA a) where
	x == y = uuid x == uuid y

instance Ord (RemoteA a) where
	compare = comparing uuid

instance ToUUID (RemoteA a) where
	toUUID = uuid

-- Use Verified when the content of a key is verified as part of a
-- transfer, and so a separate verification step is not needed.
data Verification = UnVerified | Verified

unVerified :: Monad m => m Bool -> m (Bool, Verification)
unVerified a = do
	ok <- a
	return (ok, UnVerified)
