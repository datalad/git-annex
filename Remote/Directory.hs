{- A "remote" that is just a filesystem directory.
 -
 - Copyright 2011-2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Remote.Directory (remote) where

import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Map as M

import Common.Annex
import Utility.CopyFile
import Types.Remote
import qualified Git
import Config
import Utility.FileMode
import Remote.Helper.Special
import Remote.Helper.Encryptable
import Crypto
import Utility.DataUnits
import Data.Int

remote :: RemoteType
remote = RemoteType {
	typename = "directory",
	enumerate = findSpecialRemotes "directory",
	generate = gen,
	setup = directorySetup
}

gen :: Git.Repo -> UUID -> Maybe RemoteConfig -> Annex Remote
gen r u c = do
	dir <- getConfig r "directory" (error "missing directory")
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
			remotetype = remote
		}
	where

type ChunkSize = Maybe Int64

chunkSize :: Maybe RemoteConfig -> ChunkSize
chunkSize Nothing = Nothing
chunkSize (Just m) =
	case M.lookup "chunksize" m of
		Nothing -> Nothing
		Just v -> case readSize dataUnits v of
			Nothing -> error "bad chunksize"
			Just size
				| size <= 0 -> error "bad chunksize"
				| otherwise -> Just $ fromInteger size

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

{- An infinite stream of chunks to use for a given file. -}
chunkStream :: FilePath -> [FilePath]
chunkStream f = map (\n -> f ++ ".chunk" ++ show n) [1 :: Integer ..]

{- A file that records the number of chunks used. -}
chunkCount :: FilePath -> FilePath
chunkCount f = f ++ ".chunkcount"

withCheckedFiles :: (FilePath -> IO Bool) -> ChunkSize -> FilePath -> Key -> ([FilePath] -> IO Bool) -> IO Bool
withCheckedFiles _ _ [] _ _ = return False
withCheckedFiles check Nothing d k a = go $ locations d k
	where
		go [] = return False
		go (f:fs) = do
			use <- check f
			if use
				then a [f]
				else go fs
withCheckedFiles check (Just _) d k a = go $ locations d k
	where
		go [] = return False
		go (f:fs) = do
			let chunkcount = chunkCount f
			use <- check chunkcount
			if use
				then do
					count <- readcount chunkcount
					let chunks = take count $ chunkStream f
					ok <- all id <$> mapM check chunks
					if ok
						then a chunks
						else return False
				else go fs
		readcount f = fromMaybe (error $ "cannot parse " ++ f)
			. (readish :: String -> Maybe Int)
			<$> readFile f

withStoredFiles :: ChunkSize -> FilePath -> Key -> ([FilePath] -> IO Bool) -> IO Bool
withStoredFiles = withCheckedFiles doesFileExist

store :: FilePath -> ChunkSize -> Key -> Annex Bool
store d chunksize k = do
	src <- inRepo $ gitAnnexLocation k
	liftIO $ catchBoolIO $ storeHelper d chunksize k $ \dests ->
		case chunksize of
			Nothing -> do
				let dest = Prelude.head dests
				ok <- copyFileExternal src dest
				return $ if ok then [dest] else []
			Just _ -> storeSplit chunksize dests =<< L.readFile src

storeEncrypted :: FilePath -> ChunkSize -> (Cipher, Key) -> Key -> Annex Bool
storeEncrypted d chunksize (cipher, enck) k = do
	src <- inRepo $ gitAnnexLocation k
	liftIO $ catchBoolIO $ storeHelper d chunksize enck $ encrypt src
	where
		encrypt src dests = withEncryptedContent cipher (L.readFile src) $ \s ->
			case chunksize of
				Nothing -> do
					let dest = Prelude.head dests
					L.writeFile dest s
					return [dest]
				Just _ -> storeSplit chunksize dests s

{- Splits a ByteString into chunks and writes to dests.
 - Note: Must always write at least one file, even for empty ByteString. -}
storeSplit :: ChunkSize -> [FilePath] -> L.ByteString -> IO [FilePath]
storeSplit Nothing _ _ = error "bad storeSplit call"
storeSplit _ [] _ = error "bad storeSplit call"
storeSplit (Just chunksize) alldests@(firstdest:_) s
	| L.null s = do
		-- must always write at least one file, even for empty
		L.writeFile firstdest s
		return [firstdest]
	| otherwise = storeSplit' chunksize alldests s []
storeSplit' :: Int64 -> [FilePath] -> L.ByteString -> [FilePath] -> IO [FilePath]
storeSplit' _ [] _ _ = error "expected an infinite list"
storeSplit' chunksize (d:dests) s c
	| L.null s = return $ reverse c
	| otherwise = do
		let (chunk, rest) = L.splitAt chunksize s
		L.writeFile d chunk
		storeSplit' chunksize dests rest (d:c)

{- Generates a list of destinations to write to in order to store a key.
 - When chunksize is specified, this list will be a list of chunks.
 - The action should store the file, and return a list of the destinations
 - it stored it to, or [] on error.
 - The stored files are only put into their final place once storage is
 - complete.
 -}
storeHelper :: FilePath -> ChunkSize -> Key -> ([FilePath] -> IO [FilePath]) -> IO Bool
storeHelper d chunksize key a = do
	let dir = parentDir desttemplate
	createDirectoryIfMissing True dir
	allowWrite dir
	stored <- a tmpdests
	forM_ stored $ \f -> do
		let dest = detmpprefix f
		renameFile f dest
		preventWrite dest
	when (chunksize /= Nothing) $ do
		let chunkcount = chunkCount desttemplate
		_ <- tryIO $ allowWrite chunkcount
		writeFile chunkcount (show $ length stored)
		preventWrite chunkcount
	preventWrite dir
	return (not $ null stored)
	where
		desttemplate = Prelude.head $ locations d key
		tmpdests = case chunksize of
			Nothing -> [desttemplate ++ tmpprefix]
			Just _ -> map (++ tmpprefix) (chunkStream desttemplate)
		tmpprefix = ".tmp"
		detmpprefix f = take (length f - tmpprefixlen) f
		tmpprefixlen = length tmpprefix

retrieve :: FilePath -> ChunkSize -> Key -> FilePath -> Annex Bool
retrieve d chunksize k f = liftIO $ withStoredFiles chunksize d k go
	where
		go [file] = copyFileExternal file f
		go files = catchBoolIO $ do
			L.writeFile f =<< (L.concat <$> mapM L.readFile files)
			return True

retrieveEncrypted :: FilePath -> ChunkSize -> (Cipher, Key) -> FilePath -> Annex Bool
retrieveEncrypted d chunksize (cipher, enck) f =
	liftIO $ withStoredFiles chunksize d enck $ \files -> catchBoolIO $ do
		withDecryptedContent cipher (L.concat <$> mapM L.readFile files) $
			L.writeFile f
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
		go files = all id <$> mapM removefile files
		removefile file = catchBoolIO $ do
			let dir = parentDir file
			allowWrite dir
			removeFile file
			_ <- tryIO $ removeDirectory dir
			return True

checkPresent :: FilePath -> ChunkSize -> Key -> Annex (Either String Bool)
checkPresent d chunksize k = liftIO $ catchMsgIO $ withStoredFiles chunksize d k $
	const $ return True -- withStoredFiles checked that it exists
