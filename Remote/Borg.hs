{- Using borg as a remote.
 -
 - Copyright 2020,2021 Joey Hess <id@joeyh.name>
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
import qualified Git.LsTree as LsTree
import Git.Types (toTreeItemType, TreeItemType(..))
import Git.FilePath
import Config
import Config.Cost
import Annex.Tmp
import Annex.SpecialRemote.Config
import Remote.Helper.Special
import Remote.Helper.ExportImport
import Annex.UUID
import Types.ProposedAccepted
import Utility.Metered
import Logs.Export
import qualified Remote.Helper.ThirdPartyPopulated as ThirdPartyPopulated
import Utility.Env

import Data.Either
import Text.Read
import Control.Exception (evaluate)
import Control.DeepSeq
import qualified Data.Map as M
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified System.FilePath.ByteString as P

type BorgRepo = String

type BorgArchiveName = S.ByteString

remote :: RemoteType
remote = RemoteType
	{ typename = "borg"
	, enumerate = const (findSpecialRemotes "borgrepo")
	, generate = gen
	, configParser = mkRemoteConfigParser
		[ optionalStringParser borgrepoField
			(FieldDesc "(required) borg repository to use")
		, optionalStringParser subdirField
			(FieldDesc "limit to a subdirectory of the borg repository")
		, yesNoParser appendonlyField (Just False)
			(FieldDesc "you will not use borg to delete from the repository")
		]
	, setup = borgSetup
	, exportSupported = exportUnsupported
	, importSupported = importIsSupported
	, thirdPartyPopulated = True
	}

borgrepoField :: RemoteConfigField
borgrepoField = Accepted "borgrepo"

subdirField :: RemoteConfigField
subdirField = Accepted "subdir"

appendonlyField :: RemoteConfigField
appendonlyField = Accepted "appendonly"

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
			{ listImportableContents = listImportableContentsM u borgrepo c
			, importKey = Just ThirdPartyPopulated.importKey
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
		-- When the user sets the appendonly field, they are
		-- promising not to delete content out from under git-annex
		-- using borg, so the remote is not untrustworthy.
		, untrustworthy = maybe True not $
			getRemoteConfigValue appendonlyField c
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

	return (c, u)

borgLocal :: BorgRepo -> Bool
borgLocal = notElem ':'

borgArchive :: BorgRepo -> BorgArchiveName -> String
borgArchive r n = r ++ "::" ++ decodeBS' n

listImportableContentsM :: UUID -> BorgRepo -> ParsedRemoteConfig -> Annex (Maybe (ImportableContents (ContentIdentifier, ByteSize)))
listImportableContentsM u borgrepo c = prompt $ do
	imported <- getImported u
	ls <- withborglist borgrepo Nothing formatarchivelist $ \as ->
		forM (filter (not . S.null) as) $ \archivename ->
			case M.lookup archivename imported of
				Just getfast -> return $ Left (archivename, getfast)
				Nothing -> Right <$>
					let archive = borgArchive borgrepo archivename
					in withborglist archive subdir formatfilelist $
						liftIO . evaluate . force . parsefilelist archivename
	if all isLeft ls && M.null (M.difference imported (M.fromList (lefts ls)))
		then return Nothing -- unchanged since last time, avoid work
		else Just . mkimportablecontents <$> mapM (either snd pure) ls
  where
	withborglist what addparam format a = do
		environ <- liftIO getEnvironment
		let p = proc "borg" $ toCommand $ catMaybes
			[ Just (Param "list")
			, Just (Param "--format")
			, Just (Param format)
			, Just (Param what)
			, addparam
			]
		(Nothing, Just h, Nothing, pid) <- liftIO $ createProcess $ p
			{ std_out = CreatePipe
			-- Run in C locale because the file list can
			-- include some possibly translatable text in the
			-- "extra" field.
			, env = Just (addEntry "LANG" "C" environ)
			}
		l <- liftIO $ map L.toStrict 
			. L.split 0 
			<$> L.hGetContents h
		let cleanup = liftIO $ do
			hClose h
			forceSuccessProcess p pid
		a l `finally` cleanup

	formatarchivelist = "{barchive}{NUL}"

	formatfilelist = "{size}{NUL}{path}{NUL}{extra}{NUL}"

	subdir = File <$> getRemoteConfigValue subdirField c

	parsefilelist archivename (bsz:f:extra:rest) = case readMaybe (fromRawFilePath bsz) of
		Nothing -> parsefilelist archivename rest
		Just sz ->
			let loc = genImportLocation archivename f
			-- borg list reports hard links as 0 byte files,
			-- with the extra field set to " link to ".
			-- When the annex object is a hard link to
			-- something else, we'll assume it has not been
			-- modified, since usually git-annex does prevent
			-- this. Since the 0 byte size is not the actual
			-- size,  report the key size instead, when available.
			    (reqsz, retsz) = case extra of
				" link to " -> (Nothing, fromMaybe sz . fromKey keySize)
				_ -> (Just sz, const sz)
			-- This does a little unncessary work to parse the 
			-- key, which is then thrown away. But, it lets the
			-- file list be shrank down to only the ones that are
			-- importable keys, so avoids needing to buffer all
			-- the rest of the files in memory.
			in case ThirdPartyPopulated.importKey' loc reqsz of
				Just k -> (loc, (borgContentIdentifier, retsz k))
					: parsefilelist archivename rest
				Nothing -> parsefilelist archivename rest
	parsefilelist _ _ = []

	-- importableHistory is not used for retrieval, so is not
	-- populated with old archives. Instead, a tree of archives
	-- is constructed, by genImportLocation including the archive
	-- name in the ImportLocation.
	mkimportablecontents l = ImportableContents
		{ importableContents = concat l
		, importableHistory = []
		}

-- We do not need a ContentIdentifier in order to retrieve a file from
-- borg; the ImportLocation contains all that's needed. So, this is left
-- empty.
borgContentIdentifier :: ContentIdentifier
borgContentIdentifier = ContentIdentifier mempty

-- Borg does not allow / in the name of an archive, so the archive
-- name will always be the first directory in the ImportLocation.
--
-- Paths in a borg archive are always relative, not absolute, so the use of
-- </> to combine the archive name with the path will always work.
genImportLocation :: BorgArchiveName -> RawFilePath -> ImportLocation
genImportLocation archivename p  = 
	ThirdPartyPopulated.mkThirdPartyImportLocation $
		archivename P.</> p

extractImportLocation :: ImportLocation -> (BorgArchiveName, RawFilePath)
extractImportLocation loc = go $ P.splitDirectories $
	ThirdPartyPopulated.fromThirdPartyImportLocation loc
  where
	go (archivename:rest) = (archivename, P.joinPath rest)
	go _ = giveup $ "Unable to parse import location " ++ fromRawFilePath (fromImportLocation loc)

-- Since the ImportLocation starts with the archive name, a list of all
-- archive names we've already imported can be found by just listing the
-- last imported tree. And the contents of those archives can be retrieved
-- by listing the subtree recursively, which will likely be quite a lot
-- faster than running borg.
getImported :: UUID -> Annex (M.Map BorgArchiveName (Annex [(ImportLocation, (ContentIdentifier, ByteSize))]))
getImported u = M.unions <$> (mapM go . exportedTreeishes =<< getExport u)
  where
	go t = M.fromList . mapMaybe mk
		<$> inRepo (LsTree.lsTreeStrict LsTree.LsTreeNonRecursive (LsTree.LsTreeLong False) t)
	
	mk ti
		| toTreeItemType (LsTree.mode ti) == Just TreeSubtree = Just
			( getTopFilePath (LsTree.file ti)
			, getcontents
				(getTopFilePath (LsTree.file ti))
				(LsTree.sha ti)
			)
		| otherwise = Nothing

	getcontents archivename t = mapMaybe (mkcontents archivename)
		<$> inRepo (LsTree.lsTreeStrict LsTree.LsTreeRecursive (LsTree.LsTreeLong False) t)
	
	mkcontents archivename ti = do
		let f = ThirdPartyPopulated.fromThirdPartyImportLocation $
			mkImportLocation $ getTopFilePath $ LsTree.file ti
		k <- fileKey (P.takeFileName f)
		return
			( genImportLocation archivename f
			,
				( borgContentIdentifier
				-- defaulting to 0 size is ok, this size
				-- only gets used by
				-- ThirdPartyPopulated.importKey,
				-- which ignores the size when the key
				-- does not have a size.
				, fromMaybe 0 (fromKey keySize k)
				)
			)

-- Check if the file is still there in the borg archive.
-- Does not check that the content is unchanged; we assume that 
-- the content of files in borg archives does not change, which is normally
-- the case. But archives may be deleted, and files may be deleted.
checkPresentExportWithContentIdentifierM :: BorgRepo -> Key -> ImportLocation -> [ContentIdentifier] -> Annex Bool
checkPresentExportWithContentIdentifierM borgrepo _ loc _ = prompt $ liftIO $ do
	let p = proc "borg" $ toCommand
		[ Param "list"
		, Param "--format"
		, Param "1"
		, Param (borgArchive borgrepo archivename)
		, File (fromRawFilePath archivefile)
		]
	-- borg list exits nonzero with an error message if an archive
	-- no longer exists. But, the user can delete archives at any
	-- time they want. So, hide errors, and if it exists nonzero,
	-- check if the borg repository still exists, and only throw an
	-- exception if not.
	(Nothing, Just h, Nothing, pid) <- withNullHandle $ \nullh ->
		createProcess $ p
			{ std_out = CreatePipe
			, std_err = UseHandle nullh
			}
	ok <- (== "1") <$> hGetContentsStrict h
	hClose h
	ifM (checkSuccessProcess pid)
		( return ok
		, checkrepoexists
		)
  where
	(archivename, archivefile) = extractImportLocation loc
	
	checkrepoexists = do
		let p = proc "borg" $ toCommand
			[ Param "list"
			, Param "--format"
			, Param "1"
			, Param borgrepo
			]
		(Nothing, Nothing, Nothing, pid) <- withNullHandle $ \nullh ->
			createProcess $ p
				{ std_out = UseHandle nullh }
		ifM (checkSuccessProcess pid)
			( return False -- repo exists, content not in it
			, giveup $ "Unable to access borg repository " ++ borgrepo
			)

retrieveExportWithContentIdentifierM :: BorgRepo -> ImportLocation -> ContentIdentifier -> FilePath -> Annex Key -> MeterUpdate -> Annex Key
retrieveExportWithContentIdentifierM borgrepo loc _ dest mkk _ = do
	showOutput
	prompt $ withOtherTmp $ \othertmp -> liftIO $ do
		-- borgrepo could be relative, and borg has to be run
		-- in the temp directory to get it to write there
		absborgrepo <- fromRawFilePath <$> absPath (toRawFilePath borgrepo)
		let p = proc "borg" $ toCommand
			[ Param "extract"
			, Param (borgArchive absborgrepo archivename)
			, File (fromRawFilePath archivefile)
			]
		(Nothing, Nothing, Nothing, pid) <- createProcess $ p
			{ cwd = Just (fromRawFilePath othertmp) }
		forceSuccessProcess p pid
		-- Filepaths in borg archives are relative, so it's ok to
		-- combine with </>
		moveFile (fromRawFilePath othertmp </> fromRawFilePath archivefile) dest
		removeDirectoryRecursive (fromRawFilePath othertmp)
	mkk
  where
	(archivename, archivefile) = extractImportLocation loc
