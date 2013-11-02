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
import Data.Int

import Common.Annex
import Types.Remote
import qualified Git
import Config.Cost
import Config
import Utility.FileMode
import Remote.Helper.Special
import Remote.Helper.Encryptable
import Remote.Helper.Chunked
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
	let chunksize = chunkSize c
	return $ Just $ encryptableRemote c
		(storeEncrypted dir (getGpgEncParams (c,gc)) chunksize)
		(retrieveEncrypted dir chunksize)
		Remote {
			uuid = u,
			cost = cst,
			name = Git.repoDescribe r,
			storeKey = store dir chunksize,
			retrieveKeyFile = retrieve dir chunksize,
			retrieveKeyFileCheap = retrieveCheap dir chunksize,
			removeKey = remove dir,
			hasKey = checkPresent dir chunksize,
			hasKeyCheap = True,
			whereisKey = Nothing,
			remoteFsck = Nothing,
			repairRepo = Nothing,
			config = c,
			repo = r,
			gitconfig = gc,
			localpath = Just dir,
			readonly = False,
			globallyAvailable = False,
			remotetype = remote
		}
  where
	dir = fromMaybe (error "missing directory") $ remoteAnnexDirectory gc

directorySetup :: Maybe UUID -> RemoteConfig -> Annex (RemoteConfig, UUID)
directorySetup mu c = do
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

withCheckedFiles :: (FilePath -> IO Bool) -> ChunkSize -> FilePath -> Key -> ([FilePath] -> IO Bool) -> IO Bool
withCheckedFiles _ _ [] _ _ = return False
withCheckedFiles check Nothing d k a = go $ locations d k
  where
	go [] = return False
	go (f:fs) = ifM (check f) ( a [f] , go fs )
withCheckedFiles check (Just _) d k a = go $ locations d k
  where
	go [] = return False
	go (f:fs) = do
		let chunkcount = f ++ chunkCount
		ifM (check chunkcount)
			( do
				chunks <- listChunks f <$> readFile chunkcount
				ifM (allM check chunks)
					( a chunks , return False )
			, do
				chunks <- probeChunks f check
				if null chunks
					then go fs
					else a chunks
			)

withStoredFiles :: ChunkSize -> FilePath -> Key -> ([FilePath] -> IO Bool) -> IO Bool
withStoredFiles = withCheckedFiles doesFileExist

store :: FilePath -> ChunkSize -> Key -> AssociatedFile -> MeterUpdate -> Annex Bool
store d chunksize k _f p = sendAnnex k (void $ remove d k) $ \src ->
	metered (Just p) k $ \meterupdate -> 
		storeHelper d chunksize k k $ \dests ->
			case chunksize of
				Nothing -> do
					let dest = Prelude.head dests
					meteredWriteFile meterupdate dest
						=<< L.readFile src
					return [dest]
				Just _ ->
					storeSplit meterupdate chunksize dests
						=<< L.readFile src

storeEncrypted :: FilePath -> [CommandParam] -> ChunkSize -> (Cipher, Key) -> Key -> MeterUpdate -> Annex Bool
storeEncrypted d gpgOpts chunksize (cipher, enck) k p = sendAnnex k (void $ remove d enck) $ \src ->
	metered (Just p) k $ \meterupdate ->
		storeHelper d chunksize enck k $ \dests ->
			encrypt gpgOpts cipher (feedFile src) $ readBytes $ \b ->
				case chunksize of
					Nothing -> do
						let dest = Prelude.head dests
						meteredWriteFile meterupdate dest b
						return [dest]
					Just _ -> storeSplit meterupdate chunksize dests b

{- Splits a ByteString into chunks and writes to dests, obeying configured
 - chunk size (not to be confused with the L.ByteString chunk size).
 - Note: Must always write at least one file, even for empty ByteString. -}
storeSplit :: MeterUpdate -> ChunkSize -> [FilePath] -> L.ByteString -> IO [FilePath]
storeSplit _ Nothing _ _ = error "bad storeSplit call"
storeSplit _ _ [] _ = error "bad storeSplit call"
storeSplit meterupdate (Just chunksize) alldests@(firstdest:_) b
	| L.null b = do
		-- must always write at least one file, even for empty
		L.writeFile firstdest b
		return [firstdest]
	| otherwise = storeSplit' meterupdate chunksize alldests (L.toChunks b) []
storeSplit' :: MeterUpdate -> Int64 -> [FilePath] -> [S.ByteString] -> [FilePath] -> IO [FilePath]
storeSplit' _ _ [] _ _ = error "ran out of dests"
storeSplit' _ _  _ [] c = return $ reverse c
storeSplit' meterupdate chunksize (d:dests) bs c = do
	bs' <- withFile d WriteMode $
		feed zeroBytesProcessed chunksize bs
	storeSplit' meterupdate chunksize dests bs' (d:c)
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

storeHelper :: FilePath -> ChunkSize -> Key -> Key -> ([FilePath] -> IO [FilePath]) -> Annex Bool
storeHelper d chunksize key origkey storer = check <&&> go
  where
	tmpdir = tmpDir d key
	destdir = storeDir d key
	{- An encrypted key does not have a known size,
	 - so check that the size of the original key is available as free
	 - space. -}
	check = do
		liftIO $ createDirectoryIfMissing True tmpdir
		checkDiskSpace (Just tmpdir) origkey 0
	go = liftIO $ catchBoolIO $
		storeChunks key tmpdir destdir chunksize storer recorder finalizer
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

retrieve :: FilePath -> ChunkSize -> Key -> AssociatedFile -> FilePath -> MeterUpdate -> Annex Bool
retrieve d chunksize k _ f p = metered (Just p) k $ \meterupdate ->
	liftIO $ withStoredFiles chunksize d k $ \files ->
		catchBoolIO $ do
			meteredWriteFileChunks meterupdate f files L.readFile
			return True

retrieveEncrypted :: FilePath -> ChunkSize -> (Cipher, Key) -> Key -> FilePath -> MeterUpdate -> Annex Bool
retrieveEncrypted d chunksize (cipher, enck) k f p = metered (Just p) k $ \meterupdate ->
	liftIO $ withStoredFiles chunksize d enck $ \files ->
		catchBoolIO $ do
			decrypt cipher (feeder files) $
				readBytes $ meteredWriteFile meterupdate f
			return True
  where
	feeder files h = forM_ files $ L.hPut h <=< L.readFile

retrieveCheap :: FilePath -> ChunkSize -> Key -> FilePath -> Annex Bool
retrieveCheap _ (Just _) _ _ = return False -- no cheap retrieval for chunks
#ifndef mingw32_HOST_OS
retrieveCheap d _ k f = liftIO $ withStoredFiles Nothing d k go
  where
	go [file] = catchBoolIO $ createSymbolicLink file f >> return True
	go _files = return False
#else
retrieveCheap _ _ _ _ = return False
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

checkPresent :: FilePath -> ChunkSize -> Key -> Annex (Either String Bool)
checkPresent d chunksize k = liftIO $ catchMsgIO $ withStoredFiles chunksize d k $
	const $ return True -- withStoredFiles checked that it exists
