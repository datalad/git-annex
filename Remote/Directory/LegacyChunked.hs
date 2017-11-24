{- Legacy chunksize support for directory special remote.
 -
 - Can be removed eventually.
 -
 - Copyright 2011-2012 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Remote.Directory.LegacyChunked where

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as S

import Annex.Common
import Utility.FileMode
import Remote.Helper.Special
import qualified Remote.Helper.Chunked.Legacy as Legacy
import Annex.Perms
import Utility.Metered

withCheckedFiles :: (FilePath -> IO Bool) -> FilePath -> (FilePath -> Key -> [FilePath]) -> Key -> ([FilePath] -> IO Bool) -> IO Bool
withCheckedFiles _ [] _locations _ _ = return False
withCheckedFiles check d locations k a = go $ locations d k
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
withStoredFiles :: FilePath -> (FilePath -> Key -> [FilePath]) -> Key -> ([FilePath] -> IO Bool) -> IO Bool
withStoredFiles = withCheckedFiles doesFileExist

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

storeHelper :: (FilePath -> FilePath -> IO ()) -> Key -> ([FilePath] -> IO [FilePath]) -> FilePath -> FilePath -> IO Bool
storeHelper finalizer key storer tmpdir destdir = do
	void $ liftIO $ tryIO $ createDirectoryIfMissing True tmpdir
	Legacy.storeChunks key tmpdir destdir storer recorder finalizer
  where
	recorder f s = do
		void $ tryIO $ allowWrite f
		writeFile f s
		void $ tryIO $ preventWrite f

store :: ChunkSize -> (FilePath -> FilePath -> IO ()) -> Key -> L.ByteString -> MeterUpdate -> FilePath -> FilePath -> IO Bool
store chunksize finalizer k b p = storeHelper finalizer k $ \dests ->
	storeLegacyChunked p chunksize dests b

{- Need to get a single ByteString containing every chunk.
 - Done very innefficiently, by writing to a temp file.
 - :/ This is legacy code..
 -}
retrieve :: (FilePath -> Key -> [FilePath]) -> FilePath -> Preparer Retriever
retrieve locations d basek a = do
	showLongNote "This remote uses the deprecated chunksize setting. So this will be quite slow."
	tmpdir <- fromRepo $ gitAnnexTmpMiscDir
	createAnnexDirectory tmpdir
	let tmp = tmpdir </> keyFile basek ++ ".directorylegacy.tmp"
	a $ Just $ byteRetriever $ \k sink -> do
		liftIO $ void $ withStoredFiles d locations k $ \fs -> do
			forM_ fs $
				S.appendFile tmp <=< S.readFile
			return True
		b <- liftIO $ L.readFile tmp
		liftIO $ nukeFile tmp
		sink b

checkKey :: FilePath -> (FilePath -> Key -> [FilePath]) -> Key -> Annex Bool
checkKey d locations k = liftIO $ withStoredFiles d locations k $
	-- withStoredFiles checked that it exists
	const $ return True
