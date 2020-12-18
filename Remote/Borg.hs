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
import Utility.Metered
import qualified Remote.Helper.ThirdParty as ThirdParty

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
			, importKey = Just ThirdParty.importKey
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

retrieveExportWithContentIdentifierM :: BorgRepo -> ImportLocation -> ContentIdentifier -> FilePath -> Annex Key -> MeterUpdate -> Annex Key
retrieveExportWithContentIdentifierM borgrepo loc cid dest k p = error "TODO"

checkPresentExportWithContentIdentifierM :: BorgRepo -> Key -> ImportLocation -> [ContentIdentifier] -> Annex Bool
checkPresentExportWithContentIdentifierM borgrepo k loc cids = error "TODO"
