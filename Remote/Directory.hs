{- A "remote" that is just a filesystem directory.
 -
 - Copyright 2011-2014 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Remote.Directory (remote) where

import qualified Data.ByteString.Lazy as L
import qualified Data.Map as M

import Common.Annex
import Types.Remote
import Types.Creds
import qualified Git
import Config.Cost
import Config
import Utility.FileMode
import Remote.Helper.Special
import Remote.Helper.ChunkedEncryptable
import qualified Remote.Directory.LegacyChunked as Legacy
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
	return $ Just $ chunkedEncryptableRemote c
		(prepareStore dir chunkconfig)
		(retrieve dir chunkconfig)
		Remote {
			uuid = u,
			cost = cst,
			name = Git.repoDescribe r,
			storeKey = storeKeyDummy,
			retrieveKeyFile = retreiveKeyFileDummy,
			retrieveKeyFileCheap = retrieveCheap dir chunkconfig,
			removeKey = remove dir,
			hasKey = checkPresent dir chunkconfig,
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
storeDir d k = addTrailingPathSeparator $ d </> hashDirLower k </> keyFile k

{- Where we store temporary data for a key, in the directory, as it's being
 - written. -}
tmpDir :: FilePath -> Key -> FilePath
tmpDir d k = addTrailingPathSeparator $ d </> "tmp" </> keyFile k

{- Check if there is enough free disk space in the remote's directory to
 - store the key. Note that the unencrypted key size is checked. -}
prepareStore :: FilePath -> ChunkConfig -> PrepareStorer
prepareStore d chunkconfig k = ifM (checkDiskSpace (Just d) k 0)
	( return $ Just (store d chunkconfig)
	, return Nothing
	)

store :: FilePath -> ChunkConfig -> Storer
store d chunkconfig k b p = do
	void $ tryIO $ createDirectoryIfMissing True tmpdir
	case chunkconfig of
		LegacyChunks chunksize -> Legacy.store chunksize finalizer k b p tmpdir destdir
		_ -> flip catchNonAsync (\e -> warningIO (show e) >> return False) $ do
			let tmpf = tmpdir </> keyFile k
			meteredWriteFile p tmpf b
			finalizer tmpdir destdir
			return True
  where
	tmpdir = tmpDir d k
	destdir = storeDir d k
	finalizer tmp dest = do
		void $ tryIO $ allowWrite dest -- may already exist
		void $ tryIO $ removeDirectoryRecursive dest -- or not exist
		createDirectoryIfMissing True (parentDir dest)
		renameDirectory tmp dest
		-- may fail on some filesystems
		void $ tryIO $ do
			mapM_ preventWrite =<< dirContents dest
			preventWrite dest

retrieve :: FilePath -> ChunkConfig -> PrepareRetriever
retrieve d (LegacyChunks _) basek = Legacy.retrieve locations d basek
retrieve d _ _ = return $ Just $ \k -> L.readFile =<< getLocation d k

retrieveCheap :: FilePath -> ChunkConfig -> Key -> FilePath -> Annex Bool
-- no cheap retrieval possible for chunks
retrieveCheap _ (UnpaddedChunks _) _ _ = return False
retrieveCheap _ (LegacyChunks _) _ _ = return False
#ifndef mingw32_HOST_OS
retrieveCheap d NoChunks k f = liftIO $ catchBoolIO $ do
	file <- getLocation d k
	createSymbolicLink file f
	return True
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

checkPresent :: FilePath -> ChunkConfig -> Key -> Annex (Either String Bool)
checkPresent d (LegacyChunks _) k = Legacy.checkPresent d locations k
checkPresent d _ k = liftIO $ do
	v <- catchMsgIO $ anyM doesFileExist (locations d k)
	case v of
		Right False -> ifM (doesDirectoryExist d)
			( return v
			, return $ Left $ "directory " ++ d ++ " is not accessible"
			)
		_ -> return v
