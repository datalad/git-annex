{- A "remote" that is just a filesystem directory.
 -
 - Copyright 2011-2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Remote.Directory (remote) where

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as S
import qualified Data.Map as M
import qualified Control.Exception as E

import Common.Annex
import Types.Remote
import qualified Git
import Config
import Utility.FileMode
import Remote.Helper.Special
import Remote.Helper.Encryptable
import Remote.Helper.Chunked
import Crypto
import Data.Int
import Annex.Content

remote :: RemoteType
remote = RemoteType {
	typename = "directory",
	enumerate = findSpecialRemotes "directory",
	generate = gen,
	setup = directorySetup
}

gen :: Git.Repo -> UUID -> Maybe RemoteConfig -> Annex Remote
gen r u c = do
	dir <- getRemoteConfig r "directory" (error "missing directory")
	cst <- remoteCost r cheapRemoteCost
	let chunksize = chunkSize c
	return $ encryptableRemote c
		(storeEncrypted dir chunksize)
		(retrieveEncrypted dir chunksize)
		Remote {
			uuid = u,
			cost = cst,
			name = Git.repoDescribe r,
 			storeKey = store dir chunksize,
			retrieveKeyFile = retrieve dir chunksize,
			retrieveKeyFileCheap = retrieveCheap dir chunksize,
			removeKey = remove dir chunksize,
			hasKey = checkPresent dir chunksize,
			hasKeyCheap = True,
			whereisKey = Nothing,
			config = Nothing,
			repo = r,
			localpath = Just dir,
			readonly = False,
			remotetype = remote
		}

directorySetup :: UUID -> RemoteConfig -> Annex RemoteConfig
directorySetup u c = do
	-- verify configuration is sane
	let dir = fromMaybe (error "Specify directory=") $
		M.lookup "directory" c
	liftIO $ unlessM (doesDirectoryExist dir) $
		error $ "Directory does not exist: " ++ dir
	c' <- encryptionSetup c

	-- The directory is stored in git config, not in this remote's
	-- persistant state, so it can vary between hosts.
	gitConfigSpecialRemote u c' "directory" dir
	return $ M.delete "directory" c'

{- Locations to try to access a given Key in the Directory. -}
locations :: FilePath -> Key -> [FilePath]
locations d k = map (d </>) (keyPaths k)

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
				chunks <- getChunks f <$> readFile chunkcount
				ifM (all id <$> mapM check chunks)
					( a chunks , return False )
			, go fs
			)

withStoredFiles :: ChunkSize -> FilePath -> Key -> ([FilePath] -> IO Bool) -> IO Bool
withStoredFiles = withCheckedFiles doesFileExist

store :: FilePath -> ChunkSize -> Key -> AssociatedFile -> MeterUpdate -> Annex Bool
store d chunksize k _f p = do
	src <- inRepo $ gitAnnexLocation k
	metered (Just p) k $ \meterupdate -> 
		storeHelper d chunksize k $ \dests ->
			case chunksize of
				Nothing -> do
					let dest = Prelude.head dests
					meteredWriteFile meterupdate dest
						=<< L.readFile src
					return [dest]
				Just _ ->
					storeSplit meterupdate chunksize dests
						=<< L.readFile src

storeEncrypted :: FilePath -> ChunkSize -> (Cipher, Key) -> Key -> MeterUpdate -> Annex Bool
storeEncrypted d chunksize (cipher, enck) k p = do
	src <- inRepo $ gitAnnexLocation k
	metered (Just p) k $ \meterupdate ->
		storeHelper d chunksize enck $ \dests ->
			withEncryptedContent cipher (L.readFile src) $ \s ->
				case chunksize of
					Nothing -> do
						let dest = Prelude.head dests
						meteredWriteFile meterupdate dest s
						return [dest]
					Just _ -> storeSplit meterupdate chunksize dests s

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
	bs' <- E.bracket (openFile d WriteMode) hClose (feed chunksize bs)
	storeSplit' meterupdate chunksize dests bs' (d:c)
  where
	feed _ [] _ = return []
	feed sz (l:ls) h = do
		let s = fromIntegral $ S.length l
		if s <= sz
			then do
				S.hPut h l
				meterupdate $ toInteger s
				feed (sz - s) ls h
			else return (l:ls)

{- Write a L.ByteString to a file, updating a progress meter
 - after each chunk of the L.ByteString, typically every 64 kb or so. -}
meteredWriteFile :: MeterUpdate -> FilePath -> L.ByteString -> IO ()
meteredWriteFile meterupdate dest b =
	meteredWriteFile' meterupdate dest (L.toChunks b) feeder
  where
	feeder chunks = return ([], chunks)

{- Writes a series of S.ByteString chunks to a file, updating a progress
 - meter after each chunk. The feeder is called to get more chunks. -}
meteredWriteFile' :: MeterUpdate -> FilePath -> s -> (s -> IO (s, [S.ByteString])) -> IO ()
meteredWriteFile' meterupdate dest startstate feeder =
	E.bracket (openFile dest WriteMode) hClose (feed startstate [])
  where
	feed state [] h = do
		(state', cs) <- feeder state
		unless (null cs) $
			feed state' cs h
	feed state (c:cs) h = do
		S.hPut h c
		meterupdate $ toInteger $ S.length c
		feed state cs h

storeHelper :: FilePath -> ChunkSize -> Key -> ([FilePath] -> IO [FilePath]) -> Annex Bool
storeHelper d chunksize key storer = check <&&> go
  where
	basedest = Prelude.head $ locations d key
	dir = parentDir basedest
	{- The size is not exactly known when encrypting the key;
	 - this assumes that at least the size of the key is
	 - needed as free space. -}
	check = checkDiskSpace (Just dir) key 0
	go = liftIO $ catchBoolIO $ do
		createDirectoryIfMissing True dir
		allowWrite dir
		preventWrite dir `after` storeChunks basedest chunksize storer recorder finalizer
	finalizer f dest = do
		renameFile f dest
		preventWrite dest
	recorder f s = do
		void $ tryIO $ allowWrite f
		writeFile f s
		preventWrite f

retrieve :: FilePath -> ChunkSize -> Key -> AssociatedFile -> FilePath -> Annex Bool
retrieve d chunksize k _ f = metered Nothing k $ \meterupdate ->
	liftIO $ withStoredFiles chunksize d k $ \files ->
		catchBoolIO $ do
			meteredWriteFile' meterupdate f files feeder
			return True
  where
	feeder [] = return ([], [])
	feeder (x:xs) = do
		chunks <- L.toChunks <$> L.readFile x
		return (xs, chunks)

retrieveEncrypted :: FilePath -> ChunkSize -> (Cipher, Key) -> Key -> FilePath -> Annex Bool
retrieveEncrypted d chunksize (cipher, enck) k f = metered Nothing k $ \meterupdate ->
	liftIO $ withStoredFiles chunksize d enck $ \files ->
		catchBoolIO $ do
			withDecryptedContent cipher (L.concat <$> mapM L.readFile files) $
				meteredWriteFile meterupdate f
			return True

retrieveCheap :: FilePath -> ChunkSize -> Key -> FilePath -> Annex Bool
retrieveCheap _ (Just _) _ _ = return False -- no cheap retrieval for chunks
retrieveCheap d _ k f = liftIO $ withStoredFiles Nothing d k go
  where
	go [file] = catchBoolIO $ createSymbolicLink file f >> return True
	go _files = return False

remove :: FilePath -> ChunkSize -> Key -> Annex Bool
remove d chunksize k = liftIO $ withStoredFiles chunksize d k go
  where
	go = all id <$$> mapM removefile
	removefile file = catchBoolIO $ do
		let dir = parentDir file
		allowWrite dir
		removeFile file
		_ <- tryIO $ removeDirectory dir
		return True

checkPresent :: FilePath -> ChunkSize -> Key -> Annex (Either String Bool)
checkPresent d chunksize k = liftIO $ catchMsgIO $ withStoredFiles chunksize d k $
	const $ return True -- withStoredFiles checked that it exists
