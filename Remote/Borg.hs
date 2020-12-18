{- Using borg as a remote.
 -
 - Copyright 2020 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Remote.Borg (remote) where

import Annex.Common
import Types.Remote
import Types.Creds
import Types.Import
import qualified Git
import Config
import Config.Cost
import Annex.SpecialRemote.Config
import Remote.Helper.Special
import Remote.Helper.ExportImport
import Annex.UUID
import Types.ProposedAccepted
import Crypto (isEncKey)
import Utility.Metered

import qualified System.FilePath.ByteString as P
import qualified Data.Map as M

type BorgRepo = String

remote :: RemoteType
remote = RemoteType
	{ typename = "borg"
	, enumerate = const (findSpecialRemotes "borgrepo")
	, generate = gen
	, configParser = mkRemoteConfigParser
		[ optionalStringParser borgrepoField
			(FieldDesc "(required) borg repository to use")
		]
	, setup = borgSetup
	, exportSupported = exportUnsupported
	, importSupported = importIsSupported
	, thirdPartyPopulated = True
	}

borgrepoField :: RemoteConfigField
borgrepoField = Accepted "borgrepo"

gen :: Git.Repo -> UUID -> RemoteConfig -> RemoteGitConfig -> RemoteStateHandle -> Annex (Maybe Remote)
gen r u rc gc rs = do
	c <- parsedRemoteConfig remote rc
	cst <- remoteCost gc $
		if borgLocal borgrepo
			then nearlyCheapRemoteCost
			else expensiveRemoteCost
	return $ Just $ Remote
		{ uuid = u
		, cost = cst
		, name = Git.repoDescribe r
		, storeKey = storeKeyDummy
		, retrieveKeyFile = retrieveKeyFileDummy
		, retrieveKeyFileCheap = Nothing
		-- Borg cryptographically verifies content.
		, retrievalSecurityPolicy = RetrievalAllKeysSecure
		, removeKey = removeKeyDummy
		, lockContent = Nothing
		, checkPresent = checkPresentDummy
		, checkPresentCheap = borgLocal borgrepo
		, exportActions = exportUnsupported
		, importActions = ImportActions
			{ listImportableContents = listImportableContentsM borgrepo
			, importKey = Just importKeyM
			, retrieveExportWithContentIdentifier = retrieveExportWithContentIdentifierM borgrepo
			, checkPresentExportWithContentIdentifier = checkPresentExportWithContentIdentifierM borgrepo
			-- This remote is thirdPartyPopulated, so these
			-- actions will never be used.
			, storeExportWithContentIdentifier = storeExportWithContentIdentifier importUnsupported
			, removeExportDirectoryWhenEmpty = removeExportDirectoryWhenEmpty importUnsupported
			, removeExportWithContentIdentifier = removeExportWithContentIdentifier importUnsupported
			}
		, whereisKey = Nothing
		, remoteFsck = Nothing
		, repairRepo = Nothing
		, config = c
		, getRepo = return r
		, gitconfig = gc
		, localpath = if borgLocal borgrepo && not (null borgrepo)
			then Just borgrepo
			else Nothing
		, remotetype = remote
		, availability = if borgLocal borgrepo then LocallyAvailable else GloballyAvailable
		, readonly = False
		, appendonly = False
		, mkUnavailable = return Nothing
		, getInfo = return [("repo", borgrepo)]
		, claimUrl = Nothing
		, checkUrl = Nothing
		, remoteStateHandle = rs
		}
  where
	borgrepo = fromMaybe (giveup "missing borgrepo") $ remoteAnnexBorgRepo gc

borgSetup :: SetupStage -> Maybe UUID -> Maybe CredPair -> RemoteConfig -> RemoteGitConfig -> Annex (RemoteConfig, UUID)
borgSetup _ mu _ c _gc = do
	u <- maybe (liftIO genUUID) return mu

	-- verify configuration is sane
	let borgrepo = maybe (giveup "Specify borgrepo=") fromProposedAccepted $
		M.lookup borgrepoField c

	-- The borgrepo is stored in git config, as well as this repo's
	-- persistant state, so it can vary between hosts.
	gitConfigSpecialRemote u c [("borgrepo", borgrepo)]

	-- TODO: untrusted by default, but allow overriding that

	return (c, u)

borgLocal :: BorgRepo -> Bool
borgLocal = notElem ':'

listImportableContentsM :: BorgRepo -> Annex (Maybe (ImportableContents (ContentIdentifier, ByteSize)))
listImportableContentsM borgrepo = error "TODO"

-- Since this remote is thirdPartyPopulated, this needs to
-- find only those ImportLocations that are annex object files. All other
-- files in the borg backup are ignored.
importKeyM :: ImportLocation -> ContentIdentifier -> ByteSize -> MeterUpdate -> Annex (Maybe Key)
importKeyM loc cid sz _ = return $ case deserializeKey' f of
	Just k
		-- Annex objects always are in a subdirectory with the same
		-- name as the filename. If this is not the case for the file
		-- that was backed up, it is probably not a valid annex object.
		-- Eg, it could be something in annex/bad/, or annex/tmp/.
		-- Or it could be a file that only happens to have a name
		-- like an annex object.
		-- (This does unfortunately prevent recognizing files that are
		-- part of special remotes that don't use that layout. The most
		-- likely special remote to be in a backup, the directory
		-- special remote, does use that layout at least.)
		| lastMaybe (P.splitDirectories (P.dropFileName p)) /= Just f -> Nothing
		-- Chunked or encrypted keys used in special remotes are not
		-- supported.
		| isChunkKey k || isEncKey k -> Nothing
		-- Check that the size of the key is the same as the size of the
		-- file stored in borg. This is a cheap way to make sure it's
		-- probabably the actual content of the file. We don't fully
		-- verify the content here because that could be a very 
		-- expensive operation for a large repository; if the user
		-- wants to detect every possible data corruption problem
		-- (eg, wrong data read off disk during backup, or the object
		-- was corrupt in the git-annex repo and that bad object got
		-- backed up), they can fsck the remote.
		| otherwise -> case fromKey keySize k of
			Just sz'
				| sz' == sz -> Just k
				| otherwise -> Nothing
			Nothing -> Just k
	Nothing -> Nothing
  where
	p = fromImportLocation loc
	f = P.takeFileName p

retrieveExportWithContentIdentifierM :: BorgRepo -> ImportLocation -> ContentIdentifier -> FilePath -> Annex Key -> MeterUpdate -> Annex Key
retrieveExportWithContentIdentifierM borgrepo loc cid dest k p = error "TODO"

checkPresentExportWithContentIdentifierM :: BorgRepo -> Key -> ImportLocation -> [ContentIdentifier] -> Annex Bool
checkPresentExportWithContentIdentifierM borgrepo k loc cids = error "TODO"
