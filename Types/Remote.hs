{- git-annex remotes types
 -
 - Most things should not need this, using Types instead
 -
 - Copyright 2011-2019 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE RankNTypes #-}

module Types.Remote
	( RemoteConfigField
	, RemoteConfig
	, RemoteTypeA(..)
	, RemoteA(..)
	, RemoteStateHandle
	, SetupStage(..)
	, Availability(..)
	, Verification(..)
	, unVerified
	, RetrievalSecurityPolicy(..)
	, isExportSupported
	, isImportSupported
	, ExportActions(..)
	, ImportActions(..)
	, ByteSize
	)
	where

import qualified Data.Map as M
import Data.Ord

import qualified Git
import Types.Key
import Types.UUID
import Types.GitConfig
import Types.Availability
import Types.Creds
import Types.RemoteState
import Types.UrlContents
import Types.NumCopies
import Types.Export
import Types.Import
import Types.ProposedAccepted
import Config.Cost
import Utility.Metered
import Git.Types (RemoteName)
import Utility.SafeCommand
import Utility.Url
import Utility.DataUnits

type RemoteConfigField = ProposedAccepted String

type RemoteConfig = M.Map RemoteConfigField (ProposedAccepted String)

data SetupStage = Init | Enable RemoteConfig

{- There are different types of remotes. -}
data RemoteTypeA a = RemoteType
	-- human visible type name
	{ typename :: String
	-- enumerates remotes of this type
	-- The Bool is True if automatic initialization of remotes is desired
	, enumerate :: Bool -> a [Git.Repo]
	-- generates a remote of this type
	, generate :: Git.Repo -> UUID -> RemoteConfig -> RemoteGitConfig -> RemoteStateHandle -> a (Maybe (RemoteA a))
	-- initializes or enables a remote
	, setup :: SetupStage -> Maybe UUID -> Maybe CredPair -> RemoteConfig -> RemoteGitConfig -> a (RemoteConfig, UUID)
	-- check if a remote of this type is able to support export
	, exportSupported :: RemoteConfig -> RemoteGitConfig -> a Bool
	-- check if a remote of this type is able to support import
	, importSupported :: RemoteConfig -> RemoteGitConfig -> a Bool
	}

instance Eq (RemoteTypeA a) where
	x == y = typename x == typename y

{- An individual remote. -}
data RemoteA a = Remote
	-- each Remote has a unique uuid
	{ uuid :: UUID
	-- each Remote has a human visible name
	, name :: RemoteName
	-- Remotes have a use cost; higher is more expensive
	, cost :: Cost

	-- Transfers a key's contents from disk to the remote.
	-- The key should not appear to be present on the remote until
	-- all of its contents have been transferred.
	, storeKey :: Key -> AssociatedFile -> MeterUpdate -> a Bool
	-- Retrieves a key's contents to a file.
	-- (The MeterUpdate does not need to be used if it writes
	-- sequentially to the file.)
	, retrieveKeyFile :: Key -> AssociatedFile -> FilePath -> MeterUpdate -> a (Bool, Verification)
	-- Retrieves a key's contents to a tmp file, if it can be done cheaply.
	-- It's ok to create a symlink or hardlink.
	, retrieveKeyFileCheap :: Key -> AssociatedFile -> FilePath -> a Bool
	-- Security policy for reteiving keys from this remote.
	, retrievalSecurityPolicy :: RetrievalSecurityPolicy
	-- Removes a key's contents (succeeds if the contents are not present)
	, removeKey :: Key -> a Bool
	-- Uses locking to prevent removal of a key's contents,
	-- thus producing a VerifiedCopy, which is passed to the callback.
	-- If unable to lock, does not run the callback, and throws an
	-- error.
	-- This is optional; remotes do not have to support locking.
	, lockContent :: forall r. Maybe (Key -> (VerifiedCopy -> a r) -> a r)
	-- Checks if a key is present in the remote.
	-- Throws an exception if the remote cannot be accessed.
	, checkPresent :: Key -> a Bool
	-- Some remotes can checkPresent without an expensive network
	-- operation.
	, checkPresentCheap :: Bool
	-- Some remotes support export of trees.
	, exportActions :: ExportActions a
	-- Some remotes support import of trees.
	, importActions :: ImportActions a
	-- Some remotes can provide additional details for whereis.
	, whereisKey :: Maybe (Key -> a [String])
	-- Some remotes can run a fsck operation on the remote,
	-- without transferring all the data to the local repo
	-- The parameters are passed to the fsck command on the remote.
	, remoteFsck :: Maybe ([CommandParam] -> a (IO Bool))
	-- Runs an action to repair the remote's git repository.
	, repairRepo :: Maybe (a Bool -> a (IO Bool))
	-- a Remote has a persistent configuration store
	, config :: RemoteConfig
	-- Get the git repo for the Remote.
	, getRepo :: a Git.Repo
	-- a Remote's configuration from git
	, gitconfig :: RemoteGitConfig
	-- a Remote can be assocated with a specific local filesystem path
	, localpath :: Maybe FilePath
	-- a Remote can be known to be readonly
	, readonly :: Bool
	-- a Remote can allow writes but not have a way to delete content
	-- from it. Note that an export remote that supports removeExport
	-- to remove a file from the exported tree, but still retains the
	-- content in accessible form should set this to True.
	, appendonly :: Bool
	-- a Remote can be globally available. (Ie, "in the cloud".)
	, availability :: Availability
	-- the type of the remote
	, remotetype :: RemoteTypeA a
	-- For testing, makes a version of this remote that is not
	-- available for use. All its actions should fail.
	, mkUnavailable :: a (Maybe (RemoteA a))
	-- Information about the remote, for git annex info to display.
	, getInfo :: a [(String, String)]
	-- Some remotes can download from an url (or uri).
	, claimUrl :: Maybe (URLString -> a Bool)
	-- Checks that the url is accessible, and gets information about
	-- its contents, without downloading the full content.
	-- Throws an exception if the url is inaccessible.
	, checkUrl :: Maybe (URLString -> a UrlContents)
	, remoteStateHandle :: RemoteStateHandle
	}

instance Show (RemoteA a) where
	show remote = "Remote { name =\"" ++ name remote ++ "\" }"

-- two remotes are the same if they have the same uuid
instance Eq (RemoteA a) where
	x == y = uuid x == uuid y

-- Order by cost since that is the important order of remotes
-- when deciding which to use. But since remotes often have the same cost
-- and Ord must be total, do a secondary ordering by uuid.
instance Ord (RemoteA a) where
	compare a b
		| cost a == cost b = comparing uuid a b
		| otherwise = comparing cost a b

instance ToUUID (RemoteA a) where
	toUUID = uuid

data Verification
	= UnVerified 
	-- ^ Content was not verified during transfer, but is probably
	-- ok, so if verification is disabled, don't verify it
	| Verified
	-- ^ Content was verified during transfer, so don't verify it
	-- again.
	| MustVerify
	-- ^ Content likely to have been altered during transfer,
	-- verify even if verification is normally disabled

unVerified :: Monad m => m Bool -> m (Bool, Verification)
unVerified a = do
	ok <- a
	return (ok, UnVerified)

-- Security policy indicating what keys can be safely retrieved from a
-- remote.
data RetrievalSecurityPolicy
	= RetrievalVerifiableKeysSecure
	-- ^ Transfer of keys whose content can be verified
	-- with a hash check is secure; transfer of unverifiable keys is
	-- not secure and should not be allowed.
	--
	-- This is used eg, when HTTP to a remote could be redirected to a
	-- local private web server or even a file:// url, causing private
	-- data from it that is not the intended content of a key to make
	-- its way into the git-annex repository.
	--
	-- It's also used when content is stored encrypted on a remote,
	-- which could replace it with a different encrypted file, and
	-- trick git-annex into decrypting it and leaking the decryption
	-- into the git-annex repository.
	--
	-- It's not (currently) used when the remote could alter the
	-- content stored on it, because git-annex does not provide
	-- strong guarantees about the content of keys that cannot be 
	-- verified with a hash check.
	-- (But annex.securehashesonly does provide such guarantees.)
	| RetrievalAllKeysSecure
	-- ^ Any key can be securely retrieved.

isExportSupported :: RemoteA a -> a Bool
isExportSupported r = exportSupported (remotetype r) (config r) (gitconfig r)

isImportSupported :: RemoteA a -> a Bool
isImportSupported r = importSupported (remotetype r) (config r) (gitconfig r)

data ExportActions a = ExportActions 
	-- Exports content to an ExportLocation.
	-- The exported file should not appear to be present on the remote
	-- until all of its contents have been transferred.
	{ storeExport :: FilePath -> Key -> ExportLocation -> MeterUpdate -> a Bool
	-- Retrieves exported content to a file.
	-- (The MeterUpdate does not need to be used if it writes
	-- sequentially to the file.)
	, retrieveExport :: Key -> ExportLocation -> FilePath -> MeterUpdate -> a Bool
	-- Removes an exported file (succeeds if the contents are not present)
	, removeExport :: Key -> ExportLocation -> a Bool
	-- Removes an exported directory. Typically the directory will be
	-- empty, but it could possibly contain files or other directories,
	-- and it's ok to delete those (but not required to). 
	-- If the remote does not use directories, or automatically cleans
	-- up empty directories, this can be Nothing.
	-- Should not fail if the directory was already removed.
	, removeExportDirectory :: Maybe (ExportDirectory -> a Bool)
	-- Checks if anything is exported to the remote at the specified
	-- ExportLocation.
	-- Throws an exception if the remote cannot be accessed.
	, checkPresentExport :: Key -> ExportLocation -> a Bool
	-- Renames an already exported file.
	-- This may fail with False, if the file doesn't exist.
	-- If the remote does not support renames, it can return Nothing.
	, renameExport :: Key -> ExportLocation -> ExportLocation -> a (Maybe Bool)
	}

data ImportActions a = ImportActions
	-- Finds the current set of files that are stored in the remote,
	-- along with their content identifiers and size.
	--
	-- May also find old versions of files that are still stored in the
	-- remote.
	{ listImportableContents :: a (Maybe (ImportableContents (ContentIdentifier, ByteSize)))
	-- Retrieves a file from the remote. Ensures that the file
	-- it retrieves has the requested ContentIdentifier.
	--
	-- This has to be used rather than retrieveExport
	-- when a special remote supports imports, since files on such a
	-- special remote can be changed at any time.
	, retrieveExportWithContentIdentifier 
		:: ExportLocation
		-> ContentIdentifier
		-> FilePath
		-- ^ file to write content to
		-> a (Maybe Key)
		-- ^ callback that generates a key from the downloaded content
		-> MeterUpdate
		-> a (Maybe Key)
	-- Exports content to an ExportLocation, and returns the
	-- ContentIdentifier corresponding to the content it stored.
	--
	-- This is used rather than storeExport when a special remote
	-- supports imports, since files on such a special remote can be
	-- changed at any time.
	--
	-- Since other things can modify the same file on the special
	-- remote, this must take care to not overwrite such modifications,
	-- and only overwrite a file that has one of the ContentIdentifiers
	-- passed to it, unless listContents can recover an overwritten file.
	--
	-- Also, since there can be concurrent writers, the implementation
	-- needs to make sure that the ContentIdentifier it returns
	-- corresponds to what it wrote, not to what some other writer
	-- wrote.
	, storeExportWithContentIdentifier
		:: FilePath
		-> Key
		-> ExportLocation
		-> [ContentIdentifier]
		-- ^ old content that it's safe to overwrite
		-> MeterUpdate
		-> a (Either String ContentIdentifier)
	-- This is used rather than removeExport when a special remote
	-- supports imports.
	--
	-- It should only remove a file from the remote when it has one
	-- of the ContentIdentifiers passed to it, unless listContents
	-- can recover an overwritten file.
	--
	-- It needs to handle races similar to storeExportWithContentIdentifier.
	, removeExportWithContentIdentifier
		:: Key
		-> ExportLocation
		-> [ContentIdentifier]
		-> a Bool
	-- Removes a directory from the export, but only when it's empty.
	-- Used instead of removeExportDirectory when a special remote
	-- supports imports.
	--
	-- If the directory is not empty, it should succeed.
	, removeExportDirectoryWhenEmpty :: Maybe (ExportDirectory -> a Bool)
	-- Checks if the specified ContentIdentifier is exported to the
	-- remote at the specified ExportLocation.
	-- Throws an exception if the remote cannot be accessed.
	, checkPresentExportWithContentIdentifier
		:: Key
		-> ExportLocation
		-> [ContentIdentifier]
		-> a Bool
	}
