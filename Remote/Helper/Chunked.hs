{- git-annex chunked remotes
 -
 - Copyright 2012-2014 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Remote.Helper.Chunked (
	ChunkSize,
	ChunkConfig(..),
	chunkConfig,
	storeChunks,
	chunkKeys,
	removeChunks,
	retrieveChunks,
	hasKeyChunks,
) where

import Common.Annex
import Utility.DataUnits
import Types.Remote
import Types.Key
import Logs.Chunk.Pure (ChunkSize, ChunkCount)
import Logs.Chunk
import Utility.Metered
import Crypto (EncKey)

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

{- Splits up the key's content into chunks, passing each chunk to
 - the storer action, along with a corresponding chunk key and a
 - progress meter update callback.
 -
 - Note that the storer action is responsible for catching any
 - exceptions it may encounter.
 -
 - This action may be called on a chunked key. It will simply store it.
 -}
storeChunks :: UUID -> ChunkConfig -> Key -> FilePath -> MeterUpdate -> (Key -> L.ByteString -> MeterUpdate -> IO Bool) -> Annex Bool
storeChunks u chunkconfig k f p storer = metered (Just p) k $ \meterupdate ->
	either (\e -> liftIO (print e) >> return False) (go meterupdate)
		=<< (liftIO $ tryIO $ L.readFile f)
  where
	go meterupdate b = case chunkconfig of
		(UnpaddedChunks chunksize) | not (isChunkKey k) ->
			gochunks meterupdate chunksize b (chunkKeyStream k chunksize)
		_ -> liftIO $ storer k b meterupdate

	gochunks :: MeterUpdate -> ChunkSize -> L.ByteString -> ChunkKeyStream -> Annex Bool
	gochunks meterupdate chunksize lb =
		loop zeroBytesProcessed chunksize (L.toChunks lb) []
	  where
		loop bytesprocessed sz [] c chunkkeys
			-- Always store at least one chunk,
			-- even for empty content.
			| not (null c) || numchunks == 0 = 
				storechunk bytesprocessed sz [] c chunkkeys
 			-- Once all chunks are successfully stored,
			-- update the chunk log.
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
			ifM (liftIO $ storer chunkkey (L.fromChunks $ reverse c) meterupdate')
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
chunkKeys u (UnpaddedChunks _) k | not (isChunkKey k) = do
	chunklists <- map (toChunkList k) <$> getCurrentChunks u k
	-- Probably using the chunklists, but the unchunked
	-- key could be present.
	return (chunklists ++ [[k]])
chunkKeys _ _ k = pure [[k]]

toChunkList :: Key -> (ChunkSize, ChunkCount) -> [Key]
toChunkList k (chunksize, chunkcount) = takeChunkKeyStream chunkcount $
	chunkKeyStream k chunksize

{- Removes all chunks of a key from a remote, by calling a remover
 - action on each.
 -
 - The remover action should succeed even if asked to
 - remove a key that is not present on the remote.
 -
 - This action may be called on a chunked key. It will simply remove it.
 -}
removeChunks :: (Key -> Annex Bool) -> UUID -> ChunkConfig -> EncKey -> Key -> Annex Bool
removeChunks remover u chunkconfig encryptor k = do
	ls <- chunkKeys u chunkconfig k
	ok <- allM (remover . encryptor) (concat ls)
	when ok $
		case chunkconfig of
			(UnpaddedChunks _) | not (isChunkKey k) -> do
				let chunksizes = catMaybes $ map (keyChunkSize <=< headMaybe) ls
				forM_ chunksizes $ chunksRemoved u k . fromIntegral
			_ -> noop
	return ok

{- Retrieves a key from a remote, using a retriever action that
 - streams it to a ByteString.
 -
 - When the remote is chunked, tries each of the options returned by
 - chunkKeys until it finds one where the retriever successfully
 - gets the first key in the list. The content of that key, and any
 - other chunks in the list is fed to the sink.
 -
 - If retrival of one of the subsequent chunks throws an exception,
 - gives up and returns False. Note that partial data may have been
 - written to the sink in this case.
 -}
retrieveChunks 
	:: (Key -> IO L.ByteString)
	-> UUID
	-> ChunkConfig
	-> EncKey
	-> Key
	-> MeterUpdate
	-> (MeterUpdate -> L.ByteString -> IO ())
	-> Annex Bool
retrieveChunks retriever u chunkconfig encryptor basek basep sink = do
	ls <- chunkKeys u chunkconfig basek
	liftIO $ flip catchNonAsync giveup (firstavail ls)
  where
	giveup e = print e >> return False

	firstavail [] = return False
	firstavail ([]:ls) = firstavail ls
	firstavail ((k:ks):ls) = do
		v <- tryNonAsync $ retriever (encryptor k)
		case v of
			Left e
				| null ls -> giveup e
				| otherwise -> firstavail ls
			Right b -> do
				sink basep b
				let sz = toBytesProcessed $
					fromMaybe 0 $ keyChunkSize k
				getrest sz sz ks

	getrest _ _ [] = return True
	getrest sz bytesprocessed (k:ks) = do
		let p = offsetMeterUpdate basep bytesprocessed
		sink p =<< retriever (encryptor k)
		getrest sz (addBytesProcessed bytesprocessed sz) ks

{- Checks if a key is present in a remote. This requires any one
 - of the lists of options returned by chunkKeys to all check out
 - as being present using the checker action.
 -}
hasKeyChunks
	:: (Key -> Annex (Either String Bool))
	-> UUID
	-> ChunkConfig
	-> EncKey
	-> Key
	-> Annex (Either String Bool)
hasKeyChunks checker u chunkconfig encryptor basek = do
	checklists impossible =<< chunkKeys u chunkconfig basek
  where
	checklists lastfailmsg [] = return $ Left lastfailmsg
	checklists _ (l:ls)
		| not (null l) = do
			v <- checkchunks l
			case v of
				Left e -> checklists e ls
				Right True -> return (Right True)
				Right False
					| null ls -> return (Right False)
					| otherwise -> checklists impossible ls
		| otherwise = checklists impossible ls
	
	checkchunks :: [Key] -> Annex (Either String Bool)
	checkchunks [] = return (Right True)
	checkchunks (k:ks) = do
		v <- checker (encryptor k)
		if v == Right True
			then checkchunks ks
			else return v

	impossible = "no recorded chunks"
