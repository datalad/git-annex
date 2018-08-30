{- A "remote" that is just a filesystem directory.
 -
 - Copyright 2011-2017 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Remote.Directory (
	remote,
	finalizeStoreGeneric,
	removeDirGeneric,
) where

import qualified Data.ByteString.Lazy as L
import qualified Data.Map as M
import Data.Default

import Annex.Common
import Types.Remote
import Types.Export
import Types.Creds
import qualified Git
import Config.Cost
import Config
import Utility.FileMode
import Remote.Helper.Special
import Remote.Helper.Export
import qualified Remote.Directory.LegacyChunked as Legacy
import Annex.Content
import Annex.UUID
import Utility.Metered
import Utility.Tmp

remote :: RemoteType
remote = RemoteType
	{ typename = "directory"
	, enumerate = const (findSpecialRemotes "directory")
	, generate = gen
	, setup = directorySetup
	, exportSupported = exportIsSupported
	}

gen :: Git.Repo -> UUID -> RemoteConfig -> RemoteGitConfig -> Annex (Maybe Remote)
gen r u c gc = do
	cst <- remoteCost gc cheapRemoteCost
	let chunkconfig = getChunkConfig c
	return $ Just $ specialRemote c
		(prepareStore dir chunkconfig)
		(retrieveKeyFileM dir chunkconfig)
		(simplyPrepare $ removeKeyM dir)
		(simplyPrepare $ checkPresentM dir chunkconfig)
		Remote
			{ uuid = u
			, cost = cst
			, name = Git.repoDescribe r
			, storeKey = storeKeyDummy
			, retrieveKeyFile = retreiveKeyFileDummy
			, retrieveKeyFileCheap = retrieveKeyFileCheapM dir chunkconfig
			, retrievalSecurityPolicy = RetrievalAllKeysSecure
			, removeKey = removeKeyDummy
			, lockContent = Nothing
			, checkPresent = checkPresentDummy
			, checkPresentCheap = True
			, exportActions = return $ ExportActions
				{ storeExport = storeExportM dir
				, retrieveExport = retrieveExportM dir
				, removeExport = removeExportM dir
				, checkPresentExport = checkPresentExportM dir
				-- Not needed because removeExportLocation
				-- auto-removes empty directories.
				, removeExportDirectory = Nothing
				, renameExport = renameExportM dir
				}
			, whereisKey = Nothing
			, remoteFsck = Nothing
			, repairRepo = Nothing
			, config = c
			, getRepo = return r
			, gitconfig = gc
			, localpath = Just dir
			, readonly = False
			, appendonly = False
			, availability = LocallyAvailable
			, remotetype = remote
			, mkUnavailable = gen r u c $
				gc { remoteAnnexDirectory = Just "/dev/null" }
			, getInfo = return [("directory", dir)]
			, claimUrl = Nothing
			, checkUrl = Nothing
			}
  where
	dir = fromMaybe (giveup "missing directory") $ remoteAnnexDirectory gc

directorySetup :: SetupStage -> Maybe UUID -> Maybe CredPair -> RemoteConfig -> RemoteGitConfig -> Annex (RemoteConfig, UUID)
directorySetup _ mu _ c gc = do
	u <- maybe (liftIO genUUID) return mu
	-- verify configuration is sane
	let dir = fromMaybe (giveup "Specify directory=") $
		M.lookup "directory" c
	absdir <- liftIO $ absPath dir
	liftIO $ unlessM (doesDirectoryExist absdir) $
		giveup $ "Directory does not exist: " ++ absdir
	(c', _encsetup) <- encryptionSetup c gc

	-- The directory is stored in git config, not in this remote's
	-- persistant state, so it can vary between hosts.
	gitConfigSpecialRemote u c' [("directory", absdir)]
	return (M.delete "directory" c', u)

{- Locations to try to access a given Key in the directory.
 - We try more than one since we used to write to different hash
 - directories. -}
locations :: FilePath -> Key -> [FilePath]
locations d k = map (d </>) (keyPaths k)

{- Returns the location off a Key in the directory. If the key is
 - present, returns the location that is actually used, otherwise
 - returns the first, default location. -}
getLocation :: FilePath -> Key -> IO FilePath
getLocation d k = do
	let locs = locations d k
	fromMaybe (Prelude.head locs) <$> firstM doesFileExist locs

{- Directory where the file(s) for a key are stored. -}
storeDir :: FilePath -> Key -> FilePath
storeDir d k = addTrailingPathSeparator $ d </> hashDirLower def k </> keyFile k

{- Check if there is enough free disk space in the remote's directory to
 - store the key. Note that the unencrypted key size is checked. -}
prepareStore :: FilePath -> ChunkConfig -> Preparer Storer
prepareStore d chunkconfig = checkPrepare (checkDiskSpaceDirectory d)
	(byteStorer $ store d chunkconfig)
  where

checkDiskSpaceDirectory :: FilePath -> Key -> Annex Bool
checkDiskSpaceDirectory d k = do
	annexdir <- fromRepo gitAnnexObjectDir
	samefilesystem <- liftIO $ catchDefaultIO False $ 
		(\a b -> deviceID a == deviceID b)
			<$> getFileStatus d
			<*> getFileStatus annexdir
	checkDiskSpace (Just d) k 0 samefilesystem

store :: FilePath -> ChunkConfig -> Key -> L.ByteString -> MeterUpdate -> Annex Bool
store d chunkconfig k b p = liftIO $ do
	void $ tryIO $ createDirectoryIfMissing True tmpdir
	case chunkconfig of
		LegacyChunks chunksize -> Legacy.store chunksize finalizeStoreGeneric k b p tmpdir destdir
		_ -> do
			let tmpf = tmpdir </> keyFile k
			meteredWriteFile p tmpf b
			finalizeStoreGeneric tmpdir destdir
			return True
  where
	tmpdir = addTrailingPathSeparator $ d </> "tmp" </> keyFile k
	destdir = storeDir d k

{- Passed a temp directory that contains the files that should be placed
 - in the dest directory, moves it into place. Anything already existing
 - in the dest directory will be deleted. File permissions will be locked
 - down. -}
finalizeStoreGeneric :: FilePath -> FilePath -> IO ()
finalizeStoreGeneric tmp dest = do
	void $ tryIO $ allowWrite dest -- may already exist
	void $ tryIO $ removeDirectoryRecursive dest -- or not exist
	createDirectoryIfMissing True (parentDir dest)
	renameDirectory tmp dest
	-- may fail on some filesystems
	void $ tryIO $ do
		mapM_ preventWrite =<< dirContents dest
		preventWrite dest

retrieveKeyFileM :: FilePath -> ChunkConfig -> Preparer Retriever
retrieveKeyFileM d (LegacyChunks _) = Legacy.retrieve locations d
retrieveKeyFileM d _ = simplyPrepare $ byteRetriever $ \k sink ->
	sink =<< liftIO (L.readFile =<< getLocation d k)

retrieveKeyFileCheapM :: FilePath -> ChunkConfig -> Key -> AssociatedFile -> FilePath -> Annex Bool
-- no cheap retrieval possible for chunks
retrieveKeyFileCheapM _ (UnpaddedChunks _) _ _ _ = return False
retrieveKeyFileCheapM _ (LegacyChunks _) _ _ _ = return False
#ifndef mingw32_HOST_OS
retrieveKeyFileCheapM d NoChunks k _af f = liftIO $ catchBoolIO $ do
	file <- absPath =<< getLocation d k
	ifM (doesFileExist file)
		( do
			createSymbolicLink file f
			return True
		, return False
		)
#else
retrieveKeyFileCheapM _ _ _ _ _ = return False
#endif

removeKeyM :: FilePath -> Remover
removeKeyM d k = liftIO $ removeDirGeneric d (storeDir d k)

{- Removes the directory, which must be located under the topdir.
 -
 - Succeeds even on directories and contents that do not have write
 - permission.
 -
 - If the directory does not exist, succeeds as long as the topdir does
 - exist. If the topdir does not exist, fails, because in this case the
 - remote is not currently accessible and probably still has the content
 - we were supposed to remove from it.
 -}
removeDirGeneric :: FilePath -> FilePath -> IO Bool
removeDirGeneric topdir dir = do
	void $ tryIO $ allowWrite dir
#ifdef mingw32_HOST_OS
	{- Windows needs the files inside the directory to be writable
	 - before it can delete them. -}
	void $ tryIO $ mapM_ allowWrite =<< dirContents dir
#endif
	ok <- catchBoolIO $ do
		removeDirectoryRecursive dir
		return True
	if ok
		then return ok
		else doesDirectoryExist topdir <&&> (not <$> doesDirectoryExist dir)

checkPresentM :: FilePath -> ChunkConfig -> CheckPresent
checkPresentM d (LegacyChunks _) k = Legacy.checkKey d locations k
checkPresentM d _ k = checkPresentGeneric d (locations d k)

checkPresentGeneric :: FilePath -> [FilePath] -> Annex Bool
checkPresentGeneric d ps = liftIO $
	ifM (anyM doesFileExist ps)
		( return True
		, ifM (doesDirectoryExist d)
			( return False
			, giveup $ "directory " ++ d ++ " is not accessible"
			)
		)

storeExportM :: FilePath -> FilePath -> Key -> ExportLocation -> MeterUpdate -> Annex Bool
storeExportM d src _k loc p = liftIO $ catchBoolIO $ do
	createDirectoryIfMissing True (takeDirectory dest)
	-- Write via temp file so that checkPresentGeneric will not
	-- see it until it's fully stored.
	viaTmp (\tmp () -> withMeteredFile src p (L.writeFile tmp)) dest ()
	return True
  where
	dest = exportPath d loc

retrieveExportM :: FilePath -> Key -> ExportLocation -> FilePath -> MeterUpdate -> Annex Bool
retrieveExportM d _k loc dest p = liftIO $ catchBoolIO $ do
	withMeteredFile src p (L.writeFile dest)
	return True
  where
	src = exportPath d loc

removeExportM :: FilePath -> Key -> ExportLocation -> Annex Bool
removeExportM d _k loc = liftIO $ do
	nukeFile src
	removeExportLocation d loc
	return True
  where
	src = exportPath d loc

checkPresentExportM :: FilePath -> Key -> ExportLocation -> Annex Bool
checkPresentExportM d _k loc =
	checkPresentGeneric d [exportPath d loc]

renameExportM :: FilePath -> Key -> ExportLocation -> ExportLocation -> Annex Bool
renameExportM d _k oldloc newloc = liftIO $ catchBoolIO $ do
	createDirectoryIfMissing True (takeDirectory dest)
	renameFile src dest
	removeExportLocation d oldloc
	return True
  where
	src = exportPath d oldloc
	dest = exportPath d newloc

exportPath :: FilePath -> ExportLocation -> FilePath
exportPath d loc = d </> fromExportLocation loc

{- Removes the ExportLocation's parent directory and its parents, so long as
 - they're empty, up to but not including the topdir. -}
removeExportLocation :: FilePath -> ExportLocation -> IO ()
removeExportLocation topdir loc = 
	go (Just $ takeDirectory $ fromExportLocation loc) (Right ())
  where
	go _ (Left _e) = return ()
	go Nothing _ = return ()
	go (Just loc') _ = go (upFrom loc')
		=<< tryIO (removeDirectory $ exportPath topdir (mkExportLocation loc'))
