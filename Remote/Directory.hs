{- A "remote" that is just a filesystem directory.
 -
 - Copyright 2011-2020 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
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
import Annex.SpecialRemote.Config
import Utility.FileMode
import Remote.Helper.Special
import Remote.Helper.ExportImport
import Types.Import
import qualified Remote.Directory.LegacyChunked as Legacy
import Annex.Content
import Annex.Perms
import Annex.UUID
import Backend
import Types.KeySource
import Utility.Metered
import Utility.Tmp
import Utility.InodeCache
import Types.ProposedAccepted

remote :: RemoteType
remote = specialRemoteType $ RemoteType
	{ typename = "directory"
	, enumerate = const (findSpecialRemotes "directory")
	, generate = gen
	, configParser = mkRemoteConfigParser
		[ optionalStringParser directoryField
			(FieldDesc "(required) where the special remote stores data")
		]
	, setup = directorySetup
	, exportSupported = exportIsSupported
	, importSupported = importIsSupported
	}

directoryField :: RemoteConfigField
directoryField = Accepted "directory"

gen :: Git.Repo -> UUID -> RemoteConfig -> RemoteGitConfig -> RemoteStateHandle -> Annex (Maybe Remote)
gen r u rc gc rs = do
	c <- parsedRemoteConfig remote rc
	cst <- remoteCost gc cheapRemoteCost
	let chunkconfig = getChunkConfig c
	return $ Just $ specialRemote c
		(storeKeyM dir chunkconfig)
		(retrieveKeyFileM dir chunkconfig)
		(removeKeyM dir)
		(checkPresentM dir chunkconfig)
		Remote
			{ uuid = u
			, cost = cst
			, name = Git.repoDescribe r
			, storeKey = storeKeyDummy
			, retrieveKeyFile = retrieveKeyFileDummy
			, retrieveKeyFileCheap = retrieveKeyFileCheapM dir chunkconfig
			, retrievalSecurityPolicy = RetrievalAllKeysSecure
			, removeKey = removeKeyDummy
			, lockContent = Nothing
			, checkPresent = checkPresentDummy
			, checkPresentCheap = True
			, exportActions = ExportActions
				{ storeExport = storeExportM dir
				, retrieveExport = retrieveExportM dir
				, removeExport = removeExportM dir
				, checkPresentExport = checkPresentExportM dir
				-- Not needed because removeExportLocation
				-- auto-removes empty directories.
				, removeExportDirectory = Nothing
				, renameExport = renameExportM dir
				}
			, importActions = ImportActions
				{ listImportableContents = listImportableContentsM dir
				, importKey = Just (importKeyM dir)
				, retrieveExportWithContentIdentifier = retrieveExportWithContentIdentifierM dir
				, storeExportWithContentIdentifier = storeExportWithContentIdentifierM dir
				, removeExportWithContentIdentifier = removeExportWithContentIdentifierM dir
				-- Not needed because removeExportWithContentIdentifier
				-- auto-removes empty directories.
				, removeExportDirectoryWhenEmpty = Nothing
				, checkPresentExportWithContentIdentifier = checkPresentExportWithContentIdentifierM dir
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
			, mkUnavailable = gen r u rc
				(gc { remoteAnnexDirectory = Just "/dev/null" }) rs
			, getInfo = return [("directory", dir)]
			, claimUrl = Nothing
			, checkUrl = Nothing
			, remoteStateHandle = rs
			}
  where
	dir = fromMaybe (giveup "missing directory") $ remoteAnnexDirectory gc

directorySetup :: SetupStage -> Maybe UUID -> Maybe CredPair -> RemoteConfig -> RemoteGitConfig -> Annex (RemoteConfig, UUID)
directorySetup _ mu _ c gc = do
	u <- maybe (liftIO genUUID) return mu
	-- verify configuration is sane
	let dir = maybe (giveup "Specify directory=") fromProposedAccepted $
		M.lookup directoryField c
	absdir <- liftIO $ absPath dir
	liftIO $ unlessM (doesDirectoryExist absdir) $
		giveup $ "Directory does not exist: " ++ absdir
	(c', _encsetup) <- encryptionSetup c gc

	-- The directory is stored in git config, not in this remote's
	-- persistant state, so it can vary between hosts.
	gitConfigSpecialRemote u c' [("directory", absdir)]
	return (M.delete directoryField c', u)

{- Locations to try to access a given Key in the directory.
 - We try more than one since we used to write to different hash
 - directories. -}
locations :: FilePath -> Key -> [FilePath]
locations d k = map (\f -> d </> fromRawFilePath f) (keyPaths k)

{- Returns the location off a Key in the directory. If the key is
 - present, returns the location that is actually used, otherwise
 - returns the first, default location. -}
getLocation :: FilePath -> Key -> IO FilePath
getLocation d k = do
	let locs = locations d k
	fromMaybe (Prelude.head locs) <$> firstM doesFileExist locs

{- Directory where the file(s) for a key are stored. -}
storeDir :: FilePath -> Key -> FilePath
storeDir d k = addTrailingPathSeparator $
	d </> fromRawFilePath (hashDirLower def k) </> fromRawFilePath (keyFile k)

{- Check if there is enough free disk space in the remote's directory to
 - store the key. Note that the unencrypted key size is checked. -}
storeKeyM :: FilePath -> ChunkConfig -> Storer
storeKeyM d chunkconfig k c m = 
	ifM (checkDiskSpaceDirectory d k)
		( byteStorer (store d chunkconfig) k c m
		, giveup "Not enough free disk space."
		)

checkDiskSpaceDirectory :: FilePath -> Key -> Annex Bool
checkDiskSpaceDirectory d k = do
	annexdir <- fromRepo gitAnnexObjectDir
	samefilesystem <- liftIO $ catchDefaultIO False $ 
		(\a b -> deviceID a == deviceID b)
			<$> getFileStatus d
			<*> getFileStatus annexdir
	checkDiskSpace (Just d) k 0 samefilesystem

store :: FilePath -> ChunkConfig -> Key -> L.ByteString -> MeterUpdate -> Annex ()
store d chunkconfig k b p = liftIO $ do
	void $ tryIO $ createDirectoryUnder d tmpdir
	case chunkconfig of
		LegacyChunks chunksize -> 
			Legacy.store d chunksize (finalizeStoreGeneric d) k b p tmpdir destdir
		_ -> do
			let tmpf = tmpdir </> kf
			meteredWriteFile p tmpf b
			finalizeStoreGeneric d tmpdir destdir
  where
	tmpdir = addTrailingPathSeparator $ d </> "tmp" </> kf
	kf = fromRawFilePath (keyFile k)
	destdir = storeDir d k

{- Passed a temp directory that contains the files that should be placed
 - in the dest directory, moves it into place. Anything already existing
 - in the dest directory will be deleted. File permissions will be locked
 - down. -}
finalizeStoreGeneric :: FilePath -> FilePath -> FilePath -> IO ()
finalizeStoreGeneric d tmp dest = do
	removeDirGeneric d dest
	createDirectoryUnder d (parentDir dest)
	renameDirectory tmp dest
	-- may fail on some filesystems
	void $ tryIO $ do
		mapM_ preventWrite =<< dirContents dest
		preventWrite dest

retrieveKeyFileM :: FilePath -> ChunkConfig -> Retriever
retrieveKeyFileM d (LegacyChunks _) = Legacy.retrieve locations d
retrieveKeyFileM d _ = byteRetriever $ \k sink ->
	sink =<< liftIO (L.readFile =<< getLocation d k)

retrieveKeyFileCheapM :: FilePath -> ChunkConfig -> Maybe (Key -> AssociatedFile -> FilePath -> Annex ())
-- no cheap retrieval possible for chunks
retrieveKeyFileCheapM _ (UnpaddedChunks _) = Nothing
retrieveKeyFileCheapM _ (LegacyChunks _) = Nothing
#ifndef mingw32_HOST_OS
retrieveKeyFileCheapM d NoChunks = Just $ \k _af f -> liftIO $ do
	file <- absPath =<< getLocation d k
	ifM (doesFileExist file)
		( createSymbolicLink file f
		, giveup "content file not present in remote"
		)
#else
retrieveKeyFileCheapM _ _ = Nothing
#endif

removeKeyM :: FilePath -> Remover
removeKeyM d k = liftIO $ removeDirGeneric d (storeDir d k)

{- Removes the directory, which must be located under the topdir.
 -
 - Succeeds even on directories and contents that do not have write
 - permission, if it's possible to turn the write bit on.
 -
 - If the directory does not exist, succeeds as long as the topdir does
 - exist. If the topdir does not exist, fails, because in this case the
 - remote is not currently accessible and probably still has the content
 - we were supposed to remove from it.
 -}
removeDirGeneric :: FilePath -> FilePath -> IO ()
removeDirGeneric topdir dir = do
	void $ tryIO $ allowWrite dir
#ifdef mingw32_HOST_OS
	{- Windows needs the files inside the directory to be writable
	 - before it can delete them. -}
	void $ tryIO $ mapM_ allowWrite =<< dirContents dir
#endif
	tryNonAsync (removeDirectoryRecursive dir) >>= \case
		Right () -> return ()
		Left e ->
			unlessM (doesDirectoryExist topdir <&&> (not <$> doesDirectoryExist dir)) $
				throwM e

checkPresentM :: FilePath -> ChunkConfig -> CheckPresent
checkPresentM d (LegacyChunks _) k = Legacy.checkKey d locations k
checkPresentM d _ k = checkPresentGeneric d (locations d k)

checkPresentGeneric :: FilePath -> [FilePath] -> Annex Bool
checkPresentGeneric d ps = checkPresentGeneric' d $
	liftIO $ anyM doesFileExist ps

checkPresentGeneric' :: FilePath -> Annex Bool -> Annex Bool
checkPresentGeneric' d check = ifM check
	( return True
	, ifM (liftIO $ doesDirectoryExist d)
		( return False
		, giveup $ "directory " ++ d ++ " is not accessible"
		)
	)

storeExportM :: FilePath -> FilePath -> Key -> ExportLocation -> MeterUpdate -> Annex ()
storeExportM d src _k loc p = liftIO $ do
	createDirectoryUnder d (takeDirectory dest)
	-- Write via temp file so that checkPresentGeneric will not
	-- see it until it's fully stored.
	viaTmp (\tmp () -> withMeteredFile src p (L.writeFile tmp)) dest ()
  where
	dest = exportPath d loc

retrieveExportM :: FilePath -> Key -> ExportLocation -> FilePath -> MeterUpdate -> Annex ()
retrieveExportM d _k loc dest p = 
	liftIO $ withMeteredFile src p (L.writeFile dest)
  where
	src = exportPath d loc

removeExportM :: FilePath -> Key -> ExportLocation -> Annex ()
removeExportM d _k loc = liftIO $ do
	removeWhenExistsWith removeLink src
	removeExportLocation d loc
  where
	src = exportPath d loc

checkPresentExportM :: FilePath -> Key -> ExportLocation -> Annex Bool
checkPresentExportM d _k loc =
	checkPresentGeneric d [exportPath d loc]

renameExportM :: FilePath -> Key -> ExportLocation -> ExportLocation -> Annex (Maybe ())
renameExportM d _k oldloc newloc = liftIO $ do
	createDirectoryUnder d (takeDirectory dest)
	renameFile src dest
	removeExportLocation d oldloc
	return (Just ())
  where
	src = exportPath d oldloc
	dest = exportPath d newloc

exportPath :: FilePath -> ExportLocation -> FilePath
exportPath d loc = d </> fromRawFilePath (fromExportLocation loc)

{- Removes the ExportLocation's parent directory and its parents, so long as
 - they're empty, up to but not including the topdir. -}
removeExportLocation :: FilePath -> ExportLocation -> IO ()
removeExportLocation topdir loc = 
	go (Just $ takeDirectory $ fromRawFilePath $ fromExportLocation loc) (Right ())
  where
	go _ (Left _e) = return ()
	go Nothing _ = return ()
	go (Just loc') _ = go (upFrom loc')
		=<< tryIO (removeDirectory $ exportPath topdir (mkExportLocation (toRawFilePath loc')))

listImportableContentsM :: FilePath -> Annex (Maybe (ImportableContents (ContentIdentifier, ByteSize)))
listImportableContentsM dir = catchMaybeIO $ liftIO $ do
	l <- dirContentsRecursive dir
	l' <- mapM go l
	return $ ImportableContents (catMaybes l') []
  where
	go f = do
		st <- getFileStatus f
		mkContentIdentifier f st >>= \case
			Nothing -> return Nothing
			Just cid -> do
				relf <- toRawFilePath <$> relPathDirToFile dir f
				sz <- getFileSize' f st
				return $ Just (mkImportLocation relf, (cid, sz))

-- Make a ContentIdentifier that contains an InodeCache.
--
-- The InodeCache is generated without checking a sentinal file.
-- So in a case when a remount etc causes all the inodes to change,
-- files may appear to be modified when they are not, which will only
-- result in extra work to re-import them.
--
-- If the file is not a regular file, this will return Nothing.
mkContentIdentifier :: FilePath -> FileStatus -> IO (Maybe ContentIdentifier)
mkContentIdentifier f st =
	fmap (ContentIdentifier . encodeBS . showInodeCache)
		<$> toInodeCache noTSDelta f st

guardSameContentIdentifiers :: a -> ContentIdentifier -> Maybe ContentIdentifier -> a
guardSameContentIdentifiers cont old new
	| new == Just old = cont
	| otherwise = giveup "file content has changed"

importKeyM :: FilePath -> ExportLocation -> ContentIdentifier -> MeterUpdate -> Annex Key
importKeyM dir loc cid p = do
	backend <- chooseBackend (fromRawFilePath f)
	k <- fst <$> genKey ks p backend
	currcid <- liftIO $ mkContentIdentifier absf =<< getFileStatus absf
	guardSameContentIdentifiers (return k) cid currcid
  where
	f = fromExportLocation loc
	absf = dir </> fromRawFilePath f
	ks  = KeySource
		{ keyFilename = f
		, contentLocation = toRawFilePath absf
		, inodeCache = Nothing
		}

retrieveExportWithContentIdentifierM :: FilePath -> ExportLocation -> ContentIdentifier -> FilePath -> Annex Key -> MeterUpdate -> Annex Key
retrieveExportWithContentIdentifierM dir loc cid dest mkkey p = 
	precheck $ docopy postcheck
  where
	f = exportPath dir loc

	docopy cont = do
#ifndef mingw32_HOST_OS
		let open = do
			-- Need a duplicate fd for the post check, since
			-- hGetContentsMetered closes its handle.
			fd <- openFd f ReadOnly Nothing defaultFileFlags
			dupfd <- dup fd
			h <- fdToHandle fd
			return (h, dupfd)
		let close (h, dupfd) = do
			hClose h
			closeFd dupfd
		bracketIO open close $ \(h, dupfd) -> do
#else
		let open = openBinaryFile f ReadMode
		let close = hClose
		bracketIO open close $ \h -> do
#endif
			liftIO $ hGetContentsMetered h p >>= L.writeFile dest
			k <- mkkey
#ifndef mingw32_HOST_OS
			cont dupfd (return k)
#else
			cont (return k)
#endif
	
	-- Check before copy, to avoid expensive copy of wrong file
	-- content.
	precheck cont = guardSameContentIdentifiers cont cid
		=<< liftIO . mkContentIdentifier f
		=<< liftIO (getFileStatus f)

	-- Check after copy, in case the file was changed while it was
	-- being copied.
	--
	-- When possible (not on Windows), check the same handle
	-- that the file was copied from. Avoids some race cases where
	-- the file is modified while it's copied but then gets restored
	-- to the original content afterwards.
	--
	-- This does not guard against every possible race, but neither
	-- can InodeCaches detect every possible modification to a file.
	-- It's probably as good or better than git's handling of similar
	-- situations with files being modified while it's updating the
	-- working tree for a merge.
#ifndef mingw32_HOST_OS
	postcheck fd cont = do
#else
	postcheck cont = do
#endif
		currcid <- liftIO $ mkContentIdentifier f
#ifndef mingw32_HOST_OS
			=<< getFdStatus fd
#else
			=<< getFileStatus f
#endif
		guardSameContentIdentifiers cont cid currcid

storeExportWithContentIdentifierM :: FilePath -> FilePath -> Key -> ExportLocation -> [ContentIdentifier] -> MeterUpdate -> Annex ContentIdentifier
storeExportWithContentIdentifierM dir src _k loc overwritablecids p = do
	liftIO $ createDirectoryUnder dir destdir
	withTmpFileIn destdir template $ \tmpf tmph -> do
		liftIO $ withMeteredFile src p (L.hPut tmph)
		liftIO $ hFlush tmph
		liftIO $ hClose tmph
		resetAnnexFilePerm tmpf
		liftIO (getFileStatus tmpf) >>= liftIO . mkContentIdentifier tmpf >>= \case
			Nothing -> giveup "unable to generate content identifier"
			Just newcid -> do
				checkExportContent dir loc
					(newcid:overwritablecids)
					(giveup "unsafe to overwrite file")
					(const $ liftIO $ rename tmpf dest)
				return newcid
  where
	dest = exportPath dir loc
	(destdir, base) = splitFileName dest
	template = relatedTemplate (base ++ ".tmp")

removeExportWithContentIdentifierM :: FilePath -> Key -> ExportLocation -> [ContentIdentifier] -> Annex ()
removeExportWithContentIdentifierM dir k loc removeablecids =
	checkExportContent dir loc removeablecids (giveup "unsafe to remove modified file") $ \case
		DoesNotExist -> return ()
		KnownContentIdentifier -> removeExportM dir k loc

checkPresentExportWithContentIdentifierM :: FilePath -> Key -> ExportLocation -> [ContentIdentifier] -> Annex Bool
checkPresentExportWithContentIdentifierM dir _k loc knowncids =
	checkPresentGeneric' dir $
		checkExportContent dir loc knowncids (return False) $ \case
			DoesNotExist -> return False
			KnownContentIdentifier -> return True

data CheckResult = DoesNotExist | KnownContentIdentifier

-- Checks if the content at an ExportLocation is in the knowncids,
-- and only runs the callback that modifies it if it's safe to do so.
--
-- This should avoid races to the extent possible. However,
-- if something has the file open for write, it could write to the handle
-- after the callback has overwritten or deleted it, and its write would
-- be lost, and we don't need to detect that.
-- (In similar situations, git doesn't either!) 
--
-- It follows that if something is written to the destination file
-- shortly before, it's acceptable to run the callback anyway, as that's
-- nearly indistinguishable from the above case.
--
-- So, it suffices to check if the destination file's current
-- content is known, and immediately run the callback.
checkExportContent :: FilePath -> ExportLocation -> [ContentIdentifier] -> Annex a -> (CheckResult -> Annex a) -> Annex a
checkExportContent dir loc knowncids unsafe callback = 
	tryWhenExists (liftIO $ getFileStatus dest) >>= \case
		Just destst
			| not (isRegularFile destst) -> unsafe
			| otherwise -> catchDefaultIO Nothing (liftIO $ mkContentIdentifier dest destst) >>= \case
				Just destcid
					| destcid `elem` knowncids -> callback KnownContentIdentifier
					-- dest exists with other content
					| otherwise -> unsafe
				-- should never happen
				Nothing -> unsafe
		-- dest does not exist
		Nothing -> callback DoesNotExist
  where
	dest = exportPath dir loc
