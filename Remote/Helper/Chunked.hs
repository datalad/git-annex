{- git-annex chunked remotes
 -
 - Copyright 2012-2014 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Remote.Helper.Chunked
	( ChunkSize
	, ChunkConfig(..)
	, chunkConfig
	, storeChunks
	, chunkKeys
	, meteredWriteFileChunks
	) where

import Common.Annex
import Utility.DataUnits
import Types.Remote
import Types.Key
import Logs.Chunk.Pure (ChunkSize, ChunkCount)
import Logs.Chunk
import Utility.Metered

import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as S
import qualified Data.Map as M

data ChunkConfig
	= NoChunks
	| UnpaddedChunks ChunkSize
	| LegacyChunks ChunkSize

chunkConfig :: RemoteConfig -> ChunkConfig
chunkConfig m =
	case M.lookup "chunksize" m of
		Nothing -> case M.lookup "chunk" m of
			Nothing -> NoChunks
			Just v -> UnpaddedChunks $ readsz v "chunk"
		Just v -> LegacyChunks $ readsz v "chunksize"
  where
	readsz v f = case readSize dataUnits v of
		Just size | size > 0 -> fromInteger size
		_ -> error ("bad " ++ f)

-- An infinite stream of chunk keys, starting from chunk 1.
newtype ChunkKeyStream = ChunkKeyStream [Key]

chunkKeyStream :: Key -> ChunkSize -> ChunkKeyStream
chunkKeyStream basek chunksize = ChunkKeyStream $ map mk [1..]
  where
	mk chunknum = sizedk { keyChunkNum = Just chunknum }
	sizedk = basek { keyChunkSize = Just (toInteger chunksize) }

nextChunkKeyStream :: ChunkKeyStream -> (Key, ChunkKeyStream)
nextChunkKeyStream (ChunkKeyStream (k:l)) = (k, ChunkKeyStream l)
nextChunkKeyStream (ChunkKeyStream []) = undefined -- stream is infinite!

takeChunkKeyStream :: ChunkCount -> ChunkKeyStream -> [Key]
takeChunkKeyStream n (ChunkKeyStream l) = genericTake n l

-- Number of chunks already consumed from the stream.
numChunks :: ChunkKeyStream -> Integer
numChunks = pred . fromJust . keyChunkNum . fst . nextChunkKeyStream

{- Slits up the key's content into chunks, passing each chunk to
 - the storer action, along with a unique chunk key.
 -
 - Note that the storer action is responsible for catching any
 - exceptions it may encounter.
 -
 - A progress meter display is set up, and the storer action
 - is passed a callback to update it.
 -
 - Once all chunks are successfully stored, updates the chunk log.
 -}
storeChunks :: UUID -> ChunkConfig -> Key -> FilePath -> MeterUpdate -> (Key -> L.ByteString -> MeterUpdate -> Annex Bool) -> Annex Bool
storeChunks u chunkconfig k f p storer = metered (Just p) k $ \meterupdate ->
	either (\e -> liftIO (print e) >> return False) (go meterupdate)
		=<< (liftIO $ tryIO $ L.readFile f)
  where
	go meterupdate b = case chunkconfig of
		(UnpaddedChunks chunksize) | not (isChunkKey k) ->
			gochunks meterupdate chunksize b (chunkKeyStream k chunksize)
		_ -> storer k b meterupdate

	gochunks :: MeterUpdate -> ChunkSize -> L.ByteString -> ChunkKeyStream -> Annex Bool
	gochunks meterupdate chunksize lb =
		loop zeroBytesProcessed chunksize (L.toChunks lb) []
	  where
		loop bytesprocessed sz [] c chunkkeys
			-- Always store at least one chunk,
			-- even for empty content.
			| not (null c) || numchunks == 0 = 
				storechunk bytesprocessed sz [] c chunkkeys
			| otherwise = do
				chunksStored u k chunksize numchunks
				return True
		  where
		  	numchunks = numChunks chunkkeys
		loop bytesprocessed sz (b:bs) c chunkkeys
			| s <= sz || sz == chunksize =
				loop bytesprocessed sz' bs (b:c) chunkkeys
			| otherwise =
				storechunk bytesprocessed sz' bs (b:c) chunkkeys
		  where
			s = fromIntegral (S.length b)
			sz' = sz - s

		storechunk bytesprocessed sz bs c chunkkeys = do
			let (chunkkey, chunkkeys') = nextChunkKeyStream chunkkeys
			ifM (storer chunkkey (L.fromChunks $ reverse c) meterupdate')
				( do
					let bytesprocessed' = addBytesProcessed bytesprocessed (chunksize - sz)
					loop bytesprocessed' chunksize bs [] chunkkeys'
				, return False
				)
		  where
 			{- The MeterUpdate that is passed to the action
			 - storing a chunk is offset, so that it reflects
			 - the total bytes that have already been stored
			 - in previous chunks. -}
			meterupdate' = offsetMeterUpdate meterupdate bytesprocessed

-- retrieveChunks :: UUID -> ChunkConfig -> Key -> Annex 

{- A key can be stored in a remote unchunked, or as a list of chunked keys.
 - It's even possible  for a remote to have the same key stored multiple
 - times with different chunk sizes. This finds all possible lists of keys
 - that might be on the remote that can be combined to get back the
 - requested key.
 -}
chunkKeys :: UUID -> ChunkConfig -> Key -> Annex [[Key]]
chunkKeys u (UnpaddedChunks _) k = do
	chunklists <- map (toChunkList k) <$> getCurrentChunks u k
	return ([k]:chunklists)
chunkKeys _ _ k = pure [[k]]

toChunkList :: Key -> (ChunkSize, ChunkCount) -> [Key]
toChunkList k (chunksize, chunkcount) = takeChunkKeyStream chunkcount $
	chunkKeyStream k chunksize

{- Writes a series of chunks to a file. The feeder is called to get
 - each chunk. -}
meteredWriteFileChunks :: MeterUpdate -> FilePath -> [v] -> (v -> IO L.ByteString) -> IO ()
meteredWriteFileChunks meterupdate dest chunks feeder =
	withBinaryFile dest WriteMode $ \h ->
		forM_ chunks $
			meteredWrite meterupdate h <=< feeder
