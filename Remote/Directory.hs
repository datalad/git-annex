{- A "remote" that is just a filesystem directory.
 -
 - Copyright 2011-2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Remote.Directory (remote) where

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as S
import qualified Data.Map as M

import Common.Annex
import Types.Remote
import Types.Creds
import qualified Git
import Config.Cost
import Config
import Utility.FileMode
import Remote.Helper.Special
import Remote.Helper.Encryptable
import Remote.Helper.Chunked
import qualified Remote.Helper.Chunked.Legacy as Legacy
import Crypto
import Annex.Content
import Annex.UUID
import Utility.Metered

remote :: RemoteType
remote = RemoteType {
	typename = "directory",
	enumerate = findSpecialRemotes "directory",
	generate = gen,
	setup = directorySetup
}

gen :: Git.Repo -> UUID -> RemoteConfig -> RemoteGitConfig -> Annex (Maybe Remote)
gen r u c gc = do
	cst <- remoteCost gc cheapRemoteCost
	let chunkconfig = chunkConfig c
	return $ Just $ encryptableRemote c
		(storeEncrypted dir (getGpgEncParams (c,gc)) chunkconfig)
		(retrieveEncrypted u dir chunkconfig)
		Remote {
			uuid = u,
			cost = cst,
			name = Git.repoDescribe r,
			storeKey = store u dir chunkconfig,
			retrieveKeyFile = retrieve u dir chunkconfig,
			retrieveKeyFileCheap = retrieveCheap u dir chunkconfig,
			removeKey = remove dir,
			hasKey = checkPresent u dir chunkconfig,
			hasKeyCheap = True,
			whereisKey = Nothing,
			remoteFsck = Nothing,
			repairRepo = Nothing,
			config = c,
			repo = r,
			gitconfig = gc,
			localpath = Just dir,
			readonly = False,
			availability = LocallyAvailable,
			remotetype = remote
		}
  where
	dir = fromMaybe (error "missing directory") $ remoteAnnexDirectory gc

directorySetup :: Maybe UUID -> Maybe CredPair -> RemoteConfig -> Annex (RemoteConfig, UUID)
directorySetup mu _ c = do
	u <- maybe (liftIO genUUID) return mu
	-- verify configuration is sane
	let dir = fromMaybe (error "Specify directory=") $
		M.lookup "directory" c
	absdir <- liftIO $ absPath dir
	liftIO $ unlessM (doesDirectoryExist absdir) $
		error $ "Directory does not exist: " ++ absdir
	c' <- encryptionSetup c

	-- The directory is stored in git config, not in this remote's
	-- persistant state, so it can vary between hosts.
	gitConfigSpecialRemote u c' "directory" absdir
	return (M.delete "directory" c', u)

{- Locations to try to access a given Key in the Directory.
 - We try more than since we used to write to different hash directories. -}
locations :: FilePath -> Key -> [FilePath]
locations d k = map (d </>) (keyPaths k)

{- Directory where the file(s) for a key are stored. -}
storeDir :: FilePath -> Key -> FilePath
storeDir d k = addTrailingPathSeparator $ d </> hashDirLower k </> keyFile k

{- Where we store temporary data for a key as it's being uploaded. -}
tmpDir :: FilePath -> Key -> FilePath
tmpDir d k = addTrailingPathSeparator $ d </> "tmp" </> keyFile k

withCheckedFiles :: (FilePath -> IO Bool) -> UUID -> ChunkConfig -> FilePath -> Key -> ([FilePath] -> IO Bool) -> IO Bool
withCheckedFiles _ _ _ [] _ _ = return False
withCheckedFiles check _ (LegacyChunks _) d k a = go $ locations d k
  where
	go [] = return False
	go (f:fs) = do
		let chunkcount = f ++ Legacy.chunkCount
		ifM (check chunkcount)
			( do
				chunks <- Legacy.listChunks f <$> readFile chunkcount
				ifM (allM check chunks)
					( a chunks , return False )
			, do
				chunks <- Legacy.probeChunks f check
				if null chunks
					then go fs
					else a chunks
			)
withCheckedFiles check u chunkconfig d k a =
	go $ locations d k
  where
	go [] = return False
	go (f:fs) = ifM (check f) ( a [f] , go fs )

withStoredFiles :: UUID -> ChunkConfig -> FilePath -> Key -> ([FilePath] -> IO Bool) -> IO Bool
withStoredFiles = withCheckedFiles doesFileExist

store :: UUID -> FilePath -> ChunkConfig -> Key -> AssociatedFile -> MeterUpdate -> Annex Bool
store u d chunkconfig k _f p = whenDiskAvail d k $
	sendAnnex k (void $ remove d k) $ \src ->
		storeChunks u chunkconfig k src p $ \k' b meterupdate ->
			storeHelper d chunkconfig k' $ \dests ->
				case chunkconfig of
					LegacyChunks chunksize ->
						storeLegacyChunked meterupdate chunksize dests b
					_ -> do
						let dest = Prelude.head dests
						meteredWriteFile meterupdate dest b
						return [dest]

storeEncrypted :: FilePath -> [CommandParam] -> ChunkConfig -> (Cipher, Key) -> Key -> MeterUpdate -> Annex Bool
storeEncrypted d gpgOpts chunkconfig (cipher, enck) k p = whenDiskAvail d k $
	sendAnnex k (void $ remove d enck) $ \src ->
		metered (Just p) k $ \meterupdate ->
			storeHelper d chunkconfig enck $ \dests ->
				encrypt gpgOpts cipher (feedFile src) $ readBytes $ \b ->
					case chunkconfig of
						LegacyChunks chunksize ->
							storeLegacyChunked meterupdate chunksize dests b
						_ -> do
							let dest = Prelude.head dests
							meteredWriteFile meterupdate dest b
							return [dest]

{- Splits a ByteString into chunks and writes to dests, obeying configured
 - chunk size (not to be confused with the L.ByteString chunk size). -}
storeLegacyChunked :: MeterUpdate -> ChunkSize -> [FilePath] -> L.ByteString -> IO [FilePath]
storeLegacyChunked _ _ [] _ = error "bad storeLegacyChunked call"
storeLegacyChunked meterupdate chunksize alldests@(firstdest:_) b
	| L.null b = do
		-- always write at least one file, even for empty
		L.writeFile firstdest b
		return [firstdest]
	| otherwise = storeLegacyChunked' meterupdate chunksize alldests (L.toChunks b) []
storeLegacyChunked' :: MeterUpdate -> ChunkSize -> [FilePath] -> [S.ByteString] -> [FilePath] -> IO [FilePath]
storeLegacyChunked' _ _ [] _ _ = error "ran out of dests"
storeLegacyChunked' _ _  _ [] c = return $ reverse c
storeLegacyChunked' meterupdate chunksize (d:dests) bs c = do
	bs' <- withFile d WriteMode $
		feed zeroBytesProcessed chunksize bs
	storeLegacyChunked' meterupdate chunksize dests bs' (d:c)
  where
	feed _ _ [] _ = return []
	feed bytes sz (l:ls) h = do
		let len = S.length l
		let s = fromIntegral len
		if s <= sz || sz == chunksize
			then do
				S.hPut h l
				let bytes' = addBytesProcessed bytes len
				meterupdate bytes'
				feed bytes' (sz - s) ls h
			else return (l:ls)

{- An encrypted key does not have a known size, so the unencrypted
 - key should always be passed. -}
whenDiskAvail :: FilePath -> Key -> Annex Bool -> Annex Bool
whenDiskAvail d k a = checkDiskSpace (Just d) k 0 <&&> a

storeHelper :: FilePath -> ChunkConfig -> Key -> ([FilePath] -> IO [FilePath]) -> Annex Bool
storeHelper d chunkconfig key storer = liftIO $ do
	void $ liftIO $ tryIO $ createDirectoryIfMissing True tmpdir
	case chunkconfig of
		LegacyChunks _ -> Legacy.storeChunks key tmpdir destdir storer recorder finalizer
		_ -> flip catchNonAsync (\e -> print e >> return False) $ do
			let tmpf = tmpdir </> keyFile key
			void $ storer [tmpf]
			finalizer tmpdir destdir
			return True
  where
	tmpdir = tmpDir d key
	destdir = storeDir d key
	
	finalizer tmp dest = do
		void $ tryIO $ allowWrite dest -- may already exist
		void $ tryIO $ removeDirectoryRecursive dest -- or not exist
		createDirectoryIfMissing True (parentDir dest)
		renameDirectory tmp dest
		-- may fail on some filesystems
		void $ tryIO $ do
			mapM_ preventWrite =<< dirContents dest
			preventWrite dest
	
	recorder f s = do
		void $ tryIO $ allowWrite f
		writeFile f s
		void $ tryIO $ preventWrite f

retrieve :: UUID -> FilePath -> ChunkConfig -> Key -> AssociatedFile -> FilePath -> MeterUpdate -> Annex Bool
retrieve u d chunkconfig k _ f p = metered (Just p) k $ \meterupdate ->
	liftIO $ withStoredFiles u chunkconfig d k $ \files ->
		catchBoolIO $ do
			meteredWriteFileChunks meterupdate f files L.readFile
			return True

retrieveEncrypted :: UUID -> FilePath -> ChunkConfig -> (Cipher, Key) -> Key -> FilePath -> MeterUpdate -> Annex Bool
retrieveEncrypted u d chunkconfig (cipher, enck) k f p = metered (Just p) k $ \meterupdate ->
	liftIO $ withStoredFiles u chunkconfig d enck $ \files ->
		catchBoolIO $ do
			decrypt cipher (feeder files) $
				readBytes $ meteredWriteFile meterupdate f
			return True
  where
	feeder files h = forM_ files $ L.hPut h <=< L.readFile

retrieveCheap :: UUID -> FilePath -> ChunkConfig -> Key -> FilePath -> Annex Bool
-- no cheap retrieval for chunks
retrieveCheap _ _ (UnpaddedChunks _) _ _ = return False
retrieveCheap _ _ (LegacyChunks _) _ _ = return False
#ifndef mingw32_HOST_OS
retrieveCheap u d ck k f = liftIO $ withStoredFiles u ck d k go
  where
	go [file] = catchBoolIO $
		createSymbolicLink file f >> return True
	go _files = return False
#else
retrieveCheap _ _ _ _ _ = return False
#endif

remove :: FilePath -> Key -> Annex Bool
remove d k = liftIO $ do
	void $ tryIO $ allowWrite dir
#ifdef mingw32_HOST_OS
	{- Windows needs the files inside the directory to be writable
	 - before it can delete them. -}
	void $ tryIO $ mapM_ allowWrite =<< dirContents dir
#endif
	catchBoolIO $ do
		removeDirectoryRecursive dir
		return True
  where
	dir = storeDir d k

checkPresent :: UUID -> FilePath -> ChunkConfig -> Key -> Annex (Either String Bool)
checkPresent u d chunkconfig k = liftIO $ catchMsgIO $ withStoredFiles u chunkconfig d k $
	const $ return True -- withStoredFiles checked that it exists
