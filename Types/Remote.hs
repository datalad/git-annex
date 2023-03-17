{- git-annex remotes types
 -
 - Most things should not need this, using Types instead
 -
 - Copyright 2011-2021 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE RankNTypes #-}

module Types.Remote
	( module Types.RemoteConfig
	, RemoteTypeA(..)
	, RemoteA(..)
	, RemoteStateHandle
	, SetupStage(..)
	, Availability(..)
	, VerifyConfigA(..)
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
import Types.RemoteConfig
import Utility.Hash (IncrementalVerifier)
import Config.Cost
import Utility.Metered
import Git.Types (RemoteName)
import Utility.SafeCommand
import Utility.Url
import Utility.DataUnits

data SetupStage = Init | Enable RemoteConfig | AutoEnable RemoteConfig

{- There are different types of remotes. -}
data RemoteTypeA a = RemoteType
	-- human visible type name
	{ typename :: String
	-- enumerates remotes of this type
	-- The Bool is True if automatic initialization of remotes is desired
	, enumerate :: Bool -> a [Git.Repo]
	-- generates a remote of this type
	, generate :: Git.Repo -> UUID -> RemoteConfig -> RemoteGitConfig -> RemoteStateHandle -> a (Maybe (RemoteA a))
	-- parse configs of remotes of this type
	, configParser :: RemoteConfig -> a RemoteConfigParser
	-- initializes or enables a remote
	, setup :: SetupStage -> Maybe UUID -> Maybe CredPair -> RemoteConfig -> RemoteGitConfig -> a (RemoteConfig, UUID)
	-- check if a remote of this type is able to support export
	, exportSupported :: ParsedRemoteConfig -> RemoteGitConfig -> a Bool
	-- check if a remote of this type is able to support import
	, importSupported :: ParsedRemoteConfig -> RemoteGitConfig -> a Bool
	-- is a remote of this type not a usual key/value store,
	-- or export/import of a tree of files, but instead a collection
	-- of files, populated by something outside git-annex, some of
	-- which may be annex objects?
	, thirdPartyPopulated :: Bool
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
	-- Throws exception on failure.
	, storeKey :: Key -> AssociatedFile -> MeterUpdate -> a ()
	-- Retrieves a key's contents to a file.
	-- (The MeterUpdate does not need to be used if it writes
	-- sequentially to the file.)
	-- Throws exception on failure.
	, retrieveKeyFile :: Key -> AssociatedFile -> FilePath -> MeterUpdate -> VerifyConfigA a -> a Verification
	-- Retrieves a key's contents to a tmp file, if it can be done cheaply.
	-- It's ok to create a symlink or hardlink.
	-- Throws exception on failure.
	, retrieveKeyFileCheap :: Maybe (Key -> AssociatedFile -> FilePath -> a ())
	-- Security policy for reteiving keys from this remote.
	, retrievalSecurityPolicy :: RetrievalSecurityPolicy
	-- Removes a key's contents (succeeds even the contents are not present)
	-- Can throw exception if unable to access remote, or if remote
	-- refuses to remove the content.
	, removeKey :: Key -> a ()
	-- Uses locking to prevent removal of a key's contents,
	-- thus producing a VerifiedCopy, which is passed to the callback.
	-- If unable to lock, does not run the callback, and throws an
	-- exception.
	-- This is optional; remotes do not have to support locking.
	, lockContent :: forall r. Maybe (Key -> (VerifiedCopy -> a r) -> a r)
	-- Checks if a key is present in the remote.
	-- Throws an exception if the remote cannot be accessed.
	, checkPresent :: Key -> a Bool
	-- Some remotes can checkPresent without an expensive network
	-- operation.
	, checkPresentCheap :: Bool
	-- Some remotes support export.
	, exportActions :: ExportActions a
	-- Some remotes support import.
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
	, config :: ParsedRemoteConfig
	-- Get the git repo for the Remote.
	, getRepo :: a Git.Repo
	-- a Remote's configuration from git
	, gitconfig :: RemoteGitConfig
	-- a Remote can be associated with a specific local filesystem path
	, localpath :: Maybe FilePath
	-- a Remote can be known to be readonly
	, readonly :: Bool
	-- a Remote can allow writes but not have a way to delete content
	-- from it.
	, appendonly :: Bool
	-- Set if a remote cannot be trusted to continue to contain the
	-- contents of files stored there. Notably, most export/import
	-- remotes are untrustworthy because they are not key/value stores.
	-- Since this prevents the user from adjusting a remote's trust
	-- level, it's often better not not set it and instead let the user
	-- decide.
	, untrustworthy :: Bool
	-- a Remote can be globally available. (Ie, "in the cloud".)
	, availability :: Availability
	-- the type of the remote
	, remotetype :: RemoteTypeA a
	-- For testing, makes a version of this remote that is not
	-- available for use. All its actions should fail.
	, mkUnavailable :: a (Maybe (RemoteA a))
	-- Information about the remote, for git annex info to display.
	, getInfo :: a [(String, String)]
	-- Some remotes can download from an url (or uri). This asks the
	-- remote if it can handle a particular url. The actual download
	-- will be done using retrieveKeyFile, and the remote can look up
	-- up the url to download for a key using Logs.Web.getUrls.
	, claimUrl :: Maybe (URLString -> a Bool)
	-- Checks that the url is accessible, and gets information about
	-- its contents, without downloading the full content.
	-- Throws an exception if the url is inaccessible.
	, checkUrl :: Maybe (URLString -> a UrlContents)
	, remoteStateHandle :: RemoteStateHandle
	}

instance RemoteNameable (RemoteA a) where
	getRemoteName = name

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

data VerifyConfigA a
	= AlwaysVerify
	| NoVerify
	| RemoteVerify (RemoteA a)
	| DefaultVerify

data Verification
	= UnVerified 
	-- ^ Content was not verified during transfer, but is probably
	-- ok, so if verification is disabled, don't verify it
	| Verified
	-- ^ Content was verified during transfer, so don't verify it
	-- again. The verification does not need to use a
	-- cryptographically secure hash, but the hash does need to
	-- have preimage resistance.
	| IncompleteVerify IncrementalVerifier
	-- ^ Content was partially verified during transfer, but
	-- the verification is not complete.
	| MustVerify
	-- ^ Content likely to have been altered during transfer,
	-- verify even if verification is normally disabled
	| MustFinishIncompleteVerify IncrementalVerifier
	-- ^ Content likely to have been altered during transfer,
	-- finish verification even if verification is normally disabled.

unVerified :: Monad m => m a -> m (a, Verification)
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
	-- Throws exception on failure.
	{ storeExport :: FilePath -> Key -> ExportLocation -> MeterUpdate -> a ()
	-- Retrieves exported content to a file.
	-- (The MeterUpdate does not need to be used if it writes
	-- sequentially to the file.)
	-- Throws exception on failure.
	, retrieveExport :: Key -> ExportLocation -> FilePath -> MeterUpdate -> a Verification
	-- Removes an exported file (succeeds if the contents are not present)
	-- Can throw exception if unable to access remote, or if remote
	-- refuses to remove the content.
	, removeExport :: Key -> ExportLocation -> a ()
	-- Set when the remote is versioned, so once a Key is stored
	-- to an ExportLocation, a subsequent deletion of that
	-- ExportLocation leaves the key still accessible to retrieveKeyFile
	-- and checkPresent.
	, versionedExport :: Bool
	-- Removes an exported directory. Typically the directory will be
	-- empty, but it could possibly contain files or other directories,
	-- and it's ok to delete those (but not required to). 
	-- If the remote does not use directories, or automatically cleans
	-- up empty directories, this can be Nothing.
	--
	-- Should not fail if the directory was already removed.
	--
	-- Throws exception if unable to contact the remote, or perhaps if
	-- the remote refuses to let the directory be removed.
	, removeExportDirectory :: Maybe (ExportDirectory -> a ())
	-- Checks if anything is exported to the remote at the specified
	-- ExportLocation. It may check the size or other characteristics
	-- of the Key, but does not need to guarantee that the content on
	-- the remote is the same as the Key's content.
	-- Throws an exception if the remote cannot be accessed.
	, checkPresentExport :: Key -> ExportLocation -> a Bool
	-- Renames an already exported file.
	--
	-- If the remote does not support the requested rename,
	-- it can return Nothing. It's ok if the remove deletes
	-- the file in such a situation too; it will be re-exported to
	-- recover.
	--
	-- Throws an exception if the remote cannot be accessed, or
	-- the file doesn't exist or cannot be renamed.
	, renameExport :: Key -> ExportLocation -> ExportLocation -> a (Maybe ())
	}

data ImportActions a = ImportActions
	-- Finds the current set of files that are stored in the remote,
	-- along with their content identifiers and size.
	--
	-- May also find old versions of files that are still stored in the
	-- remote.
	--
	-- Throws exception on failure to access the remote.
	-- May return Nothing when the remote is unchanged since last time.
	{ listImportableContents :: a (Maybe (ImportableContentsChunkable a (ContentIdentifier, ByteSize)))
	-- Generates a Key (of any type) for the file stored on the
	-- remote at the ImportLocation. Does not download the file
	-- from the remote.
	--
	-- May update the progress meter if it needs to perform an
	-- expensive operation, such as hashing a local file.
	--
	-- Ensures that the key corresponds to the ContentIdentifier,
	-- bearing in mind that the file on the remote may have changed
	-- since the ContentIdentifier was generated.
	--
	-- When it returns nothing, the file at the ImportLocation 
	-- will not be included in the imported tree.
	--
	-- When the remote is thirdPartyPopulated, this should check if the
	-- file stored on the remote is the content of an annex object,
	-- and return its Key, or Nothing if it is not.
	--
	-- Throws exception on failure to access the remote.
	, importKey :: Maybe (ImportLocation -> ContentIdentifier -> ByteSize -> MeterUpdate -> a (Maybe Key))
	-- Retrieves a file from the remote. Ensures that the file
	-- it retrieves has one of the requested ContentIdentifiers.
	--
	-- This has to be used rather than retrieveExport
	-- when a special remote supports imports, since files on such a
	-- special remote can be changed at any time.
	--
	-- Throws exception on failure.
	, retrieveExportWithContentIdentifier 
		:: ExportLocation
		-> [ContentIdentifier]
		-- file to write content to
		-> FilePath
		-- Either the key, or when it's not yet known, a callback
		-- that generates a key from the downloaded content.
		-> Either Key (a Key)
		-> MeterUpdate
		-> a (Key, Verification)
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
	--
	-- Throws exception on failure.
	, storeExportWithContentIdentifier
		:: FilePath
		-> Key
		-> ExportLocation
		-- old content that it's safe to overwrite
		-> [ContentIdentifier]
		-> MeterUpdate
		-> a ContentIdentifier
	-- This is used rather than removeExport when a special remote
	-- supports imports.
	--
	-- It should only remove a file from the remote when it has one
	-- of the ContentIdentifiers passed to it, unless listContents
	-- can recover an overwritten file.
	--
	-- It needs to handle races similar to storeExportWithContentIdentifier.
	--
	-- Throws an exception when unable to remove.
	, removeExportWithContentIdentifier
		:: Key
		-> ExportLocation
		-> [ContentIdentifier]
		-> a ()
	-- Removes a directory from the export, but only when it's empty.
	-- Used instead of removeExportDirectory when a special remote
	-- supports imports.
	--
	-- If the directory is not empty, it should succeed.
	--
	-- Throws exception if unable to contact the remote, or perhaps if
	-- the remote refuses to let the directory be removed.
	, removeExportDirectoryWhenEmpty :: Maybe (ExportDirectory -> a ())
	-- Checks if the specified ContentIdentifier is exported to the
	-- remote at the specified ExportLocation.
	-- Throws an exception if the remote cannot be accessed.
	, checkPresentExportWithContentIdentifier
		:: Key
		-> ExportLocation
		-> [ContentIdentifier]
		-> a Bool
	}

