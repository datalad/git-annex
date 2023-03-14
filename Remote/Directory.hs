{- A "remote" that is just a filesystem directory.
 -
 - Copyright 2011-2022 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE CPP #-}

module Remote.Directory (
	remote,
	finalizeStoreGeneric,
	removeDirGeneric,
) where

import qualified Data.ByteString.Lazy as L
import qualified Data.Map as M
import qualified System.FilePath.ByteString as P
import Data.Default
import System.PosixCompat.Files (isRegularFile, getFdStatus, deviceID)

import Annex.Common
import Types.Remote
import Types.Export
import Types.Creds
import qualified Git
import Config.Cost
import Config
import Annex.SpecialRemote.Config
import Remote.Helper.Special
import Remote.Helper.ExportImport
import Types.Import
import qualified Remote.Directory.LegacyChunked as Legacy
import Annex.CopyFile
import Annex.Content
import Annex.Perms
import Annex.UUID
import Annex.Verify
import Backend
import Types.KeySource
import Types.ProposedAccepted
import Utility.Metered
import Utility.Tmp
import Utility.InodeCache
import Utility.FileMode
import Utility.Directory.Create
import qualified Utility.RawFilePath as R

remote :: RemoteType
remote = specialRemoteType $ RemoteType
	{ typename = "directory"
	, enumerate = const (findSpecialRemotes "directory")
	, generate = gen
	, configParser = mkRemoteConfigParser
		[ optionalStringParser directoryField
			(FieldDesc "(required) where the special remote stores data")
		, yesNoParser ignoreinodesField (Just False)
			(FieldDesc "ignore inodes when importing/exporting")
		]
	, setup = directorySetup
	, exportSupported = exportIsSupported
	, importSupported = importIsSupported
	, thirdPartyPopulated = False
	}

directoryField :: RemoteConfigField
directoryField = Accepted "directory"

ignoreinodesField :: RemoteConfigField
ignoreinodesField = Accepted "ignoreinodes"

gen :: Git.Repo -> UUID -> RemoteConfig -> RemoteGitConfig -> RemoteStateHandle -> Annex (Maybe Remote)
gen r u rc gc rs = do
	c <- parsedRemoteConfig remote rc
	cst <- remoteCost gc c cheapRemoteCost
	let chunkconfig = getChunkConfig c
	cow <- liftIO newCopyCoWTried
	let ii = IgnoreInodes $ fromMaybe True $
		getRemoteConfigValue ignoreinodesField c
	return $ Just $ specialRemote c
		(storeKeyM dir chunkconfig cow)
		(retrieveKeyFileM dir chunkconfig cow)
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
				{ storeExport = storeExportM dir cow
				, retrieveExport = retrieveExportM dir cow
				, removeExport = removeExportM dir
				, versionedExport = False
				, checkPresentExport = checkPresentExportM dir
				-- Not needed because removeExportLocation
				-- auto-removes empty directories.
				, removeExportDirectory = Nothing
				, renameExport = renameExportM dir
				}
			, importActions = ImportActions
				{ listImportableContents = listImportableContentsM ii dir
				, importKey = Just (importKeyM ii dir)
				, retrieveExportWithContentIdentifier = retrieveExportWithContentIdentifierM ii dir cow
				, storeExportWithContentIdentifier = storeExportWithContentIdentifierM ii dir cow
				, removeExportWithContentIdentifier = removeExportWithContentIdentifierM ii dir
				-- Not needed because removeExportWithContentIdentifier
				-- auto-removes empty directories.
				, removeExportDirectoryWhenEmpty = Nothing
				, checkPresentExportWithContentIdentifier = checkPresentExportWithContentIdentifierM ii dir
				}
			, whereisKey = Nothing
			, remoteFsck = Nothing
			, repairRepo = Nothing
			, config = c
			, getRepo = return r
			, gitconfig = gc
			, localpath = Just dir'
			, readonly = False
			, appendonly = False
			, untrustworthy = False
			, availability = LocallyAvailable
			, remotetype = remote
			, mkUnavailable = gen r u rc
				(gc { remoteAnnexDirectory = Just "/dev/null" }) rs
			, getInfo = return [("directory", dir')]
			, claimUrl = Nothing
			, checkUrl = Nothing
			, remoteStateHandle = rs
			}
  where
	dir = toRawFilePath dir'
	dir' = fromMaybe (giveup "missing directory") (remoteAnnexDirectory gc)

directorySetup :: SetupStage -> Maybe UUID -> Maybe CredPair -> RemoteConfig -> RemoteGitConfig -> Annex (RemoteConfig, UUID)
directorySetup _ mu _ c gc = do
	u <- maybe (liftIO genUUID) return mu
	-- verify configuration is sane
	let dir = maybe (giveup "Specify directory=") fromProposedAccepted $
		M.lookup directoryField c
	absdir <- liftIO $ fromRawFilePath <$> absPath (toRawFilePath dir)
	liftIO $ unlessM (doesDirectoryExist absdir) $
		giveup $ "Directory does not exist: " ++ absdir
	(c', _encsetup) <- encryptionSetup c gc

	-- The directory is stored in git config, not in this remote's
	-- persistent state, so it can vary between hosts.
	gitConfigSpecialRemote u c' [("directory", absdir)]
	return (M.delete directoryField c', u)

{- Locations to try to access a given Key in the directory.
 - We try more than one since we used to write to different hash
 - directories. -}
locations :: RawFilePath -> Key -> [RawFilePath]
locations d k = map (d P.</>) (keyPaths k)

{- Returns the location off a Key in the directory. If the key is
 - present, returns the location that is actually used, otherwise
 - returns the first, default location. -}
getLocation :: RawFilePath -> Key -> IO RawFilePath
getLocation d k = do
	let locs = locations d k
	fromMaybe (Prelude.head locs)
		<$> firstM (doesFileExist . fromRawFilePath) locs

{- Directory where the file(s) for a key are stored. -}
storeDir :: RawFilePath -> Key -> RawFilePath
storeDir d k = P.addTrailingPathSeparator $
	d P.</> hashDirLower def k P.</> keyFile k

{- Check if there is enough free disk space in the remote's directory to
 - store the key. Note that the unencrypted key size is checked. -}
storeKeyM :: RawFilePath -> ChunkConfig -> CopyCoWTried -> Storer
storeKeyM d chunkconfig cow k c m = 
	ifM (checkDiskSpaceDirectory d k)
		( do
			void $ liftIO $ tryIO $ createDirectoryUnder [d] tmpdir
			store
		, giveup "Not enough free disk space."
		)
  where
	store = case chunkconfig of
		LegacyChunks chunksize -> 
			let go _k b p = liftIO $ Legacy.store
				(fromRawFilePath d)
				chunksize
				(finalizeStoreGeneric d)
				k b p
				(fromRawFilePath tmpdir)
				(fromRawFilePath destdir)
			in byteStorer go k c m
		NoChunks ->
			let go _k src p = liftIO $ do
				void $ fileCopier cow src tmpf p Nothing
				finalizeStoreGeneric d tmpdir destdir
			in fileStorer go k c m
		_ -> 
			let go _k b p = liftIO $ do
				meteredWriteFile p tmpf b
				finalizeStoreGeneric d tmpdir destdir
			in byteStorer go k c m
	
	tmpdir = P.addTrailingPathSeparator $ d P.</> "tmp" P.</> kf
	tmpf = fromRawFilePath tmpdir </> fromRawFilePath kf
	kf = keyFile k
	destdir = storeDir d k

checkDiskSpaceDirectory :: RawFilePath -> Key -> Annex Bool
checkDiskSpaceDirectory d k = do
	annexdir <- fromRepo gitAnnexObjectDir
	samefilesystem <- liftIO $ catchDefaultIO False $ 
		(\a b -> deviceID a == deviceID b)
			<$> R.getSymbolicLinkStatus d
			<*> R.getSymbolicLinkStatus annexdir
	checkDiskSpace (Just d) k 0 samefilesystem

{- Passed a temp directory that contains the files that should be placed
 - in the dest directory, moves it into place. Anything already existing
 - in the dest directory will be deleted. File permissions will be locked
 - down. -}
finalizeStoreGeneric :: RawFilePath -> RawFilePath -> RawFilePath -> IO ()
finalizeStoreGeneric d tmp dest = do
	removeDirGeneric (fromRawFilePath d) dest'
	createDirectoryUnder [d] (parentDir dest)
	renameDirectory (fromRawFilePath tmp) dest'
	-- may fail on some filesystems
	void $ tryIO $ do
		mapM_ (preventWrite . toRawFilePath) =<< dirContents dest'
		preventWrite dest
  where
	dest' = fromRawFilePath dest

retrieveKeyFileM :: RawFilePath -> ChunkConfig -> CopyCoWTried -> Retriever
retrieveKeyFileM d (LegacyChunks _) _ = Legacy.retrieve locations d
retrieveKeyFileM d NoChunks cow = fileRetriever' $ \dest k p iv -> do
	src <- liftIO $ fromRawFilePath <$> getLocation d k
	void $ liftIO $ fileCopier cow src (fromRawFilePath dest) p iv
retrieveKeyFileM d _ _ = byteRetriever $ \k sink ->
	sink =<< liftIO (L.readFile . fromRawFilePath =<< getLocation d k)

retrieveKeyFileCheapM :: RawFilePath -> ChunkConfig -> Maybe (Key -> AssociatedFile -> FilePath -> Annex ())
-- no cheap retrieval possible for chunks
retrieveKeyFileCheapM _ (UnpaddedChunks _) = Nothing
retrieveKeyFileCheapM _ (LegacyChunks _) = Nothing
#ifndef mingw32_HOST_OS
retrieveKeyFileCheapM d NoChunks = Just $ \k _af f -> liftIO $ do
	file <- fromRawFilePath <$> (absPath =<< getLocation d k)
	ifM (doesFileExist file)
		( R.createSymbolicLink (toRawFilePath file) (toRawFilePath f)
		, giveup "content file not present in remote"
		)
#else
retrieveKeyFileCheapM _ _ = Nothing
#endif

removeKeyM :: RawFilePath -> Remover
removeKeyM d k = liftIO $ removeDirGeneric
	(fromRawFilePath d)
	(fromRawFilePath (storeDir d k))

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
	void $ tryIO $ allowWrite (toRawFilePath dir)
#ifdef mingw32_HOST_OS
	{- Windows needs the files inside the directory to be writable
	 - before it can delete them. -}
	void $ tryIO $ mapM_ (allowWrite . toRawFilePath) =<< dirContents dir
#endif
	tryNonAsync (removeDirectoryRecursive dir) >>= \case
		Right () -> return ()
		Left e ->
			unlessM (doesDirectoryExist topdir <&&> (not <$> doesDirectoryExist dir)) $
				throwM e

checkPresentM :: RawFilePath -> ChunkConfig -> CheckPresent
checkPresentM d (LegacyChunks _) k = Legacy.checkKey d locations k
checkPresentM d _ k = checkPresentGeneric d (locations d k)

checkPresentGeneric :: RawFilePath -> [RawFilePath] -> Annex Bool
checkPresentGeneric d ps = checkPresentGeneric' d $
	liftIO $ anyM (doesFileExist . fromRawFilePath) ps

checkPresentGeneric' :: RawFilePath -> Annex Bool -> Annex Bool
checkPresentGeneric' d check = ifM check
	( return True
	, ifM (liftIO $ doesDirectoryExist (fromRawFilePath d))
		( return False
		, giveup $ "directory " ++ fromRawFilePath d ++ " is not accessible"
		)
	)

storeExportM :: RawFilePath -> CopyCoWTried -> FilePath -> Key -> ExportLocation -> MeterUpdate -> Annex ()
storeExportM d cow src _k loc p = do
	liftIO $ createDirectoryUnder [d] (P.takeDirectory dest)
	-- Write via temp file so that checkPresentGeneric will not
	-- see it until it's fully stored.
	viaTmp go (fromRawFilePath dest) ()
  where
	dest = exportPath d loc
	go tmp () = void $ liftIO $ fileCopier cow src tmp p Nothing

retrieveExportM :: RawFilePath -> CopyCoWTried -> Key -> ExportLocation -> FilePath -> MeterUpdate -> Annex Verification
retrieveExportM d cow k loc dest p = 
	verifyKeyContentIncrementally AlwaysVerify k $ \iv -> 
		void $ liftIO $ fileCopier cow src dest p iv
  where
	src = fromRawFilePath $ exportPath d loc

removeExportM :: RawFilePath -> Key -> ExportLocation -> Annex ()
removeExportM d _k loc = liftIO $ do
	removeWhenExistsWith R.removeLink src
	removeExportLocation d loc
  where
	src = exportPath d loc

checkPresentExportM :: RawFilePath -> Key -> ExportLocation -> Annex Bool
checkPresentExportM d _k loc =
	checkPresentGeneric d [exportPath d loc]

renameExportM :: RawFilePath -> Key -> ExportLocation -> ExportLocation -> Annex (Maybe ())
renameExportM d _k oldloc newloc = liftIO $ do
	createDirectoryUnder [d] (P.takeDirectory dest)
	renameFile (fromRawFilePath src) (fromRawFilePath dest)
	removeExportLocation d oldloc
	return (Just ())
  where
	src = exportPath d oldloc
	dest = exportPath d newloc

exportPath :: RawFilePath -> ExportLocation -> RawFilePath
exportPath d loc = d P.</> fromExportLocation loc

{- Removes the ExportLocation's parent directory and its parents, so long as
 - they're empty, up to but not including the topdir. -}
removeExportLocation :: RawFilePath -> ExportLocation -> IO ()
removeExportLocation topdir loc = 
	go (Just $ P.takeDirectory $ fromExportLocation loc) (Right ())
  where
	go _ (Left _e) = return ()
	go Nothing _ = return ()
	go (Just loc') _ = 
		let p = fromRawFilePath $ exportPath topdir $
			mkExportLocation loc'
		in go (upFrom loc') =<< tryIO (removeDirectory p)

listImportableContentsM :: IgnoreInodes -> RawFilePath -> Annex (Maybe (ImportableContentsChunkable Annex (ContentIdentifier, ByteSize)))
listImportableContentsM ii dir = liftIO $ do
	l <- dirContentsRecursiveSkipping (const False) False (fromRawFilePath dir)
	l' <- mapM (go . toRawFilePath) l
	return $ Just $ ImportableContentsComplete $
		ImportableContents (catMaybes l') []
  where
	go f = do
		st <- R.getSymbolicLinkStatus f
		mkContentIdentifier ii f st >>= \case
			Nothing -> return Nothing
			Just cid -> do
				relf <- relPathDirToFile dir f
				sz <- getFileSize' f st
				return $ Just (mkImportLocation relf, (cid, sz))

newtype IgnoreInodes = IgnoreInodes Bool

-- Make a ContentIdentifier that contains the size and mtime of the file,
-- and also normally the inode, unless ignoreinodes=yes.
--
-- If the file is not a regular file, this will return Nothing.
mkContentIdentifier :: IgnoreInodes -> RawFilePath -> FileStatus -> IO (Maybe ContentIdentifier)
mkContentIdentifier (IgnoreInodes ii) f st =
	liftIO $ fmap (ContentIdentifier . encodeBS . showInodeCache)
		<$> if ii
			then toInodeCache' noTSDelta f st 0
			else toInodeCache noTSDelta f st

-- Since ignoreinodes can be changed by enableremote, and since previous
-- versions of git-annex ignored inodes by default, treat two content
-- idenfiers as the same if they differ only by one having the inode
-- ignored.
guardSameContentIdentifiers :: a -> [ContentIdentifier] -> Maybe ContentIdentifier -> a
guardSameContentIdentifiers _ _ Nothing = giveup "file not found"
guardSameContentIdentifiers cont olds (Just new)
	| any (new ==) olds = cont
	| any (ignoreinode new ==) olds = cont
	| any (\old -> new == ignoreinode old) olds = cont
	| otherwise = giveup "file content has changed"
  where
	ignoreinode cid@(ContentIdentifier b) = 
		case readInodeCache (decodeBS b) of
			Nothing -> cid
			Just ic -> 
				let ic' = replaceInode 0 ic
				in ContentIdentifier (encodeBS (showInodeCache ic'))

importKeyM :: IgnoreInodes -> RawFilePath -> ExportLocation -> ContentIdentifier -> ByteSize -> MeterUpdate -> Annex (Maybe Key)
importKeyM ii dir loc cid sz p = do
	backend <- chooseBackend f
	unsizedk <- fst <$> genKey ks p backend
	let k = alterKey unsizedk $ \kd -> kd
		{ keySize = keySize kd <|> Just sz }
	currcid <- liftIO $ mkContentIdentifier ii absf
		=<< R.getSymbolicLinkStatus absf
	guardSameContentIdentifiers (return (Just k)) [cid] currcid
  where
	f = fromExportLocation loc
	absf = dir P.</> f
	ks  = KeySource
		{ keyFilename = f
		, contentLocation = absf
		, inodeCache = Nothing
		}

retrieveExportWithContentIdentifierM :: IgnoreInodes -> RawFilePath -> CopyCoWTried -> ExportLocation -> [ContentIdentifier] -> FilePath -> Either Key (Annex Key) -> MeterUpdate -> Annex (Key, Verification)
retrieveExportWithContentIdentifierM ii dir cow loc cids dest gk p =
	case gk of
		Right mkkey -> do
			go Nothing
			k <- mkkey
			return (k, UnVerified)
		Left k -> do
			v <- verifyKeyContentIncrementally DefaultVerify k go
			return (k, v)
  where
	f = exportPath dir loc
	f' = fromRawFilePath f
	
	go iv = precheck (docopy iv)

	docopy iv = ifM (liftIO $ tryCopyCoW cow f' dest p)
		( postcheckcow (liftIO $ maybe noop unableIncrementalVerifier iv)
		, docopynoncow iv
		)

	docopynoncow iv = do
#ifndef mingw32_HOST_OS
		let open = do
			-- Need a duplicate fd for the post check.
			fd <- openFd f' ReadOnly Nothing defaultFileFlags
			dupfd <- dup fd
			h <- fdToHandle fd
			return (h, dupfd)
		let close (h, dupfd) = do
			hClose h
			closeFd dupfd
		bracketIO open close $ \(h, dupfd) -> do
#else
		let open = openBinaryFile f' ReadMode
		let close = hClose
		bracketIO open close $ \h -> do
#endif
			liftIO $ fileContentCopier h dest p iv
#ifndef mingw32_HOST_OS
			postchecknoncow dupfd (return ())
#else
			postchecknoncow (return ())
#endif
	
	-- Check before copy, to avoid expensive copy of wrong file
	-- content.
	precheck cont = guardSameContentIdentifiers cont cids
		=<< liftIO . mkContentIdentifier ii f
		=<< liftIO (R.getSymbolicLinkStatus f)

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
	postchecknoncow fd cont = do
#else
	postchecknoncow cont = do
#endif
		currcid <- liftIO $ mkContentIdentifier ii f
#ifndef mingw32_HOST_OS
			=<< getFdStatus fd
#else
			=<< R.getSymbolicLinkStatus f
#endif
		guardSameContentIdentifiers cont cids currcid

	-- When copy-on-write was done, cannot check the handle that was
	-- copied from, but such a copy should run very fast, so
	-- it's very unlikely that the file changed after precheck,
	-- the modified version was copied CoW, and then the file was
	-- restored to the original content before this check.
	postcheckcow cont = do
		currcid <- liftIO $ mkContentIdentifier ii f
			=<< R.getSymbolicLinkStatus f
		guardSameContentIdentifiers cont cids currcid

storeExportWithContentIdentifierM :: IgnoreInodes -> RawFilePath -> CopyCoWTried -> FilePath -> Key -> ExportLocation -> [ContentIdentifier] -> MeterUpdate -> Annex ContentIdentifier
storeExportWithContentIdentifierM ii dir cow src _k loc overwritablecids p = do
	liftIO $ createDirectoryUnder [dir] (toRawFilePath destdir)
	withTmpFileIn destdir template $ \tmpf tmph -> do
		liftIO $ hClose tmph
		void $ liftIO $ fileCopier cow src tmpf p Nothing
		let tmpf' = toRawFilePath tmpf
		resetAnnexFilePerm tmpf'
		liftIO (R.getSymbolicLinkStatus tmpf') >>= liftIO . mkContentIdentifier ii tmpf' >>= \case
			Nothing -> giveup "unable to generate content identifier"
			Just newcid -> do
				checkExportContent ii dir loc
					overwritablecids
					(giveup "unsafe to overwrite file")
					(const $ liftIO $ R.rename tmpf' dest)
				return newcid
  where
	dest = exportPath dir loc
	(destdir, base) = splitFileName (fromRawFilePath dest)
	template = relatedTemplate (base ++ ".tmp")

removeExportWithContentIdentifierM :: IgnoreInodes -> RawFilePath -> Key -> ExportLocation -> [ContentIdentifier] -> Annex ()
removeExportWithContentIdentifierM ii dir k loc removeablecids =
	checkExportContent ii dir loc removeablecids (giveup "unsafe to remove modified file") $ \case
		DoesNotExist -> return ()
		KnownContentIdentifier -> removeExportM dir k loc

checkPresentExportWithContentIdentifierM :: IgnoreInodes -> RawFilePath -> Key -> ExportLocation -> [ContentIdentifier] -> Annex Bool
checkPresentExportWithContentIdentifierM ii dir _k loc knowncids =
	checkPresentGeneric' dir $
		checkExportContent ii dir loc knowncids (return False) $ \case
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
checkExportContent :: IgnoreInodes -> RawFilePath -> ExportLocation -> [ContentIdentifier] -> Annex a -> (CheckResult -> Annex a) -> Annex a
checkExportContent ii dir loc knowncids unsafe callback = 
	tryWhenExists (liftIO $ R.getSymbolicLinkStatus dest) >>= \case
		Just destst
			| not (isRegularFile destst) -> unsafe
			| otherwise -> catchDefaultIO Nothing (liftIO $ mkContentIdentifier ii dest destst) >>= \case
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
