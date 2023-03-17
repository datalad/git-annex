{- git-annex chunked remotes
 -
 - Copyright 2014-2021 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE RankNTypes #-}

module Remote.Helper.Chunked (
	ChunkSize,
	ChunkConfig(..),
	noChunks,
	describeChunkConfig,
	chunkConfigParsers,
	getChunkConfig,
	storeChunks,
	removeChunks,
	retrieveChunks,
	checkPresentChunks,
	chunkField,
) where

import Annex.Common
import qualified Annex
import Utility.DataUnits
import Types.StoreRetrieve
import Types.Remote
import Types.ProposedAccepted
import Logs.Chunk
import Utility.Metered
import Crypto
import Backend (isStableKey)
import Annex.SpecialRemote.Config
import Annex.Verify
import qualified Utility.RawFilePath as R

import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L

data ChunkConfig
	= NoChunks
	| UnpaddedChunks ChunkSize
	| LegacyChunks ChunkSize
	deriving (Show)

describeChunkConfig :: ChunkConfig -> String
describeChunkConfig NoChunks = "none"
describeChunkConfig (UnpaddedChunks sz) = describeChunkSize sz ++ " chunks"
describeChunkConfig (LegacyChunks sz) = describeChunkSize sz ++ " chunks (old style)"

describeChunkSize :: ChunkSize -> String
describeChunkSize sz = roughSize storageUnits False (fromIntegral sz)

noChunks :: ChunkConfig -> Bool
noChunks NoChunks = True
noChunks _ = False

chunkConfigParsers :: [RemoteConfigFieldParser]
chunkConfigParsers =
	[ optionalStringParser chunksizeField HiddenField -- deprecated
	, optionalStringParser chunkField
		(FieldDesc "size of chunks (eg, 1MiB)")
	]

getChunkConfig :: ParsedRemoteConfig -> ChunkConfig
getChunkConfig c =
	case getRemoteConfigValue chunksizeField c of
		Nothing -> case getRemoteConfigValue chunkField c of
			Nothing -> NoChunks
			Just v -> readsz UnpaddedChunks v chunkField
		Just v -> readsz LegacyChunks v chunksizeField
  where
	readsz mk v f = case readSize dataUnits v of
		Just size
			| size == 0 -> NoChunks
			| size > 0 -> mk (fromInteger size)
		_ -> giveup $ "bad configuration " ++ fromProposedAccepted f ++ "=" ++ v

-- An infinite stream of chunk keys, starting from chunk 1.
newtype ChunkKeyStream = ChunkKeyStream [Key]

chunkKeyStream :: Key -> ChunkSize -> ChunkKeyStream
chunkKeyStream basek chunksize = ChunkKeyStream $ map mk [1..]
  where
	mk chunknum = alterKey sizedk $ \d -> d
		{ keyChunkNum = Just chunknum }
	sizedk = alterKey basek $ \d -> d
		{ keyChunkSize = Just (toInteger chunksize) }

nextChunkKeyStream :: ChunkKeyStream -> (Key, ChunkKeyStream)
nextChunkKeyStream (ChunkKeyStream (k:l)) = (k, ChunkKeyStream l)
nextChunkKeyStream (ChunkKeyStream []) = error "expected infinite ChunkKeyStream"

takeChunkKeyStream :: ChunkCount -> ChunkKeyStream -> [Key]
takeChunkKeyStream n (ChunkKeyStream l) = genericTake n l

-- Number of chunks already consumed from the stream.
numChunks :: ChunkKeyStream -> Integer
numChunks = pred . fromJust . fromKey keyChunkNum . fst . nextChunkKeyStream

{- Splits up the key's content into chunks, passing each chunk to
 - the storer action, along with a corresponding chunk key and a
 - progress meter update callback.
 -
 - To support resuming, the checker is used to find the first missing
 - chunk key. Storing starts from that chunk.
 -
 - This buffers each chunk in memory, so can use a lot of memory
 - with a large ChunkSize.
 - More optimal versions of this can be written, that rely
 - on L.toChunks to split the lazy bytestring into chunks (typically
 - smaller than the ChunkSize), and eg, write those chunks to a Handle.
 - But this is the best that can be done with the storer interface that
 - writes a whole L.ByteString at a time.
 -}
storeChunks
	:: LensGpgEncParams encc
	=> UUID
	-> ChunkConfig
	-> EncKey
	-> Key
	-> FilePath
	-> MeterUpdate
	-> Maybe (Cipher, EncKey)
	-> encc
	-> Storer
	-> CheckPresent
	-> Annex ()
storeChunks u chunkconfig encryptor k f p enc encc storer checker = 
	case chunkconfig of
		-- Only stable keys are safe to store chunked,
		-- because an unstable key can have multiple different
		-- objects, and mixing up chunks from them would be
		-- possible without this check.
		(UnpaddedChunks chunksize) -> ifM (isStableKey k)
			( do
				h <- liftIO $ openBinaryFile f ReadMode
				go chunksize h
				liftIO $ hClose h
			, storechunk k (FileContent f) p
			)
		_ -> storechunk k (FileContent f) p
  where
	go chunksize h = do
		let chunkkeys = chunkKeyStream k chunksize
		(chunkkeys', startpos) <- seekResume h encryptor chunkkeys checker
		b <- liftIO $ L.hGetContents h
		gochunks p startpos chunksize b chunkkeys'

	gochunks :: MeterUpdate -> BytesProcessed -> ChunkSize -> L.ByteString -> ChunkKeyStream -> Annex ()
	gochunks meterupdate startpos chunksize = loop startpos . splitchunk
	  where
		splitchunk = L.splitAt chunksize
	
		loop bytesprocessed (chunk, bs) chunkkeys
			| L.null chunk && numchunks > 0 = do
				-- Once all chunks are successfully
				-- stored, update the chunk log.
				chunksStored u k (FixedSizeChunks chunksize) numchunks
			| otherwise = do
				liftIO $ meterupdate' zeroBytesProcessed
				let (chunkkey, chunkkeys') = nextChunkKeyStream chunkkeys
				storechunk chunkkey (ByteContent chunk) meterupdate'
				let bytesprocessed' = addBytesProcessed bytesprocessed (L.length chunk)
				loop bytesprocessed' (splitchunk bs) chunkkeys'
		  where
			numchunks = numChunks chunkkeys
			{- The MeterUpdate that is passed to the action
			 - storing a chunk is offset, so that it reflects
			 - the total bytes that have already been stored
			 - in previous chunks. -}
			meterupdate' = offsetMeterUpdate meterupdate bytesprocessed

	storechunk ck content meterupdate = case enc of
		Nothing -> storer ck content meterupdate
		(Just (cipher, enck)) -> do
			cmd <- gpgCmd <$> Annex.getGitConfig
			withBytes content $ \b ->
				encrypt cmd encc cipher (feedBytes b) $
					readBytes $ \encb ->
						storer (enck ck) (ByteContent encb) meterupdate

{- Check if any of the chunk keys are present. If found, seek forward
 - in the Handle, so it will be read starting at the first missing chunk.
 - Returns the ChunkKeyStream truncated to start at the first missing
 - chunk, and the number of bytes skipped due to resuming.
 -
 - As an optimisation, if the file fits into a single chunk, there's no need
 - to check if that chunk is present -- we know it's not, because otherwise
 - the whole file would be present and there would be no reason to try to
 - store it.
 -}
seekResume
	:: Handle
	-> EncKey
	-> ChunkKeyStream
	-> CheckPresent
	-> Annex (ChunkKeyStream, BytesProcessed)
seekResume h encryptor chunkkeys checker = do
	sz <- liftIO (hFileSize h)
	if sz <= fromMaybe 0 (fromKey keyChunkSize $ fst $ nextChunkKeyStream chunkkeys)
		then return (chunkkeys, zeroBytesProcessed)
		else check 0 chunkkeys sz
  where
	check pos cks sz
		| pos >= sz = do
			-- All chunks are already stored!
			liftIO $ hSeek h AbsoluteSeek sz
			return (cks, toBytesProcessed sz)
		| otherwise = do
			v <- tryNonAsync (checker (encryptor k))
			case v of
				Right True ->
					check pos' cks' sz
				_ -> do
					when (pos > 0) $
						liftIO $ hSeek h AbsoluteSeek pos
					return (cks, toBytesProcessed pos)
	  where
		(k, cks') = nextChunkKeyStream cks
		pos' = pos + fromMaybe 0 (fromKey keyChunkSize k)

{- Removes all chunks of a key from a remote, by calling a remover
 - action on each.
 -
 - This action may be called on a chunked key. It will simply remove it.
 -}
removeChunks :: Remover -> UUID -> ChunkConfig -> EncKey -> Key -> Annex ()
removeChunks remover u chunkconfig encryptor k = do
	ls <- map chunkKeyList <$> chunkKeys u chunkconfig k
	mapM_ (remover . encryptor) (concat ls)
	let chunksizes = catMaybes $ map (fromKey keyChunkSize <=< headMaybe) ls
	forM_ chunksizes $ chunksRemoved u k . FixedSizeChunks . fromIntegral

{- Retrieves a key from a remote, using a retriever action.
 -
 - When the remote is chunked, tries each of the options returned by
 - chunkKeys until it finds one where the retriever successfully
 - gets the first chunked key.
 -
 - If retrieval of one of the subsequent chunks throws an exception,
 - gives up. Note that partial data may have been written to the file
 - in this case.
 -
 - Resuming is supported when using chunks. When the destination file
 - already exists, it skips to the next chunked key that would be needed
 - to resume.
 -
 - Handles decrypting the content when encryption is used.
 -}
retrieveChunks 
	:: LensGpgEncParams encc
	=> Retriever
	-> UUID
	-> VerifyConfig
	-> ChunkConfig
	-> EncKey
	-> Key
	-> FilePath
	-> MeterUpdate
	-> Maybe (Cipher, EncKey)
	-> encc
	-> Annex Verification
retrieveChunks retriever u vc chunkconfig encryptor basek dest basep enc encc
	| noChunks chunkconfig = do
		-- Optimisation: Try the unchunked key first, to avoid
		-- looking in the git-annex branch for chunk counts
		-- that are likely not there.
		tryNonAsync getunchunked >>= \case
			Right r -> finalize r
			Left e -> go (Just e)
				=<< chunkKeysOnly u chunkconfig basek
	| otherwise = go Nothing 
		=<< chunkKeys u chunkconfig basek
  where
	go pe cks = do
		let ls = map chunkKeyList cks
		currsize <- liftIO $ catchMaybeIO $ getFileSize (toRawFilePath dest)
		let ls' = maybe ls (setupResume ls) currsize
		if any null ls'
			-- dest is already complete
			then finalize (Right Nothing)
			else finalize =<< firstavail pe currsize ls'

	firstavail Nothing _ [] = giveup "unable to determine the chunks to use for this remote"
	firstavail (Just e) _ [] = throwM e
	firstavail pe currsize ([]:ls) = firstavail pe currsize ls
	firstavail pe currsize ((k:ks):ls)
		| k == basek = getunchunked
			`catchNonAsync` (\e -> firstavail (Just (pickerr e)) currsize ls)
		| otherwise = do
			let offset = resumeOffset currsize k
			let p = maybe basep
				(offsetMeterUpdate basep . toBytesProcessed)
				offset
			v <- tryNonAsync $
				retriever (encryptor k) p Nothing $ \content ->
					bracket (maybe opennew openresume offset) (liftIO . hClose . fst) $ \(h, iv) -> do
						retrieved iv (Just h) p content
						let sz = toBytesProcessed $
							fromMaybe 0 $ fromKey keyChunkSize k
						getrest p h iv sz sz ks
			case v of
				Left e -> firstavail (Just (pickerr e)) currsize ls
				Right r -> return r
	  where
		-- Prefer an earlier exception to a later one, because the
		-- more probable location is tried first and less probable
		-- ones later.
		pickerr e = case pe of
			Just pe' -> pe'
			Nothing -> e

	getrest _ _ iv _ _ [] = return (Right iv)
	getrest p h iv sz bytesprocessed (k:ks) = do
		let p' = offsetMeterUpdate p bytesprocessed
		liftIO $ p' zeroBytesProcessed
		retriever (encryptor k) p' Nothing $ 
			retrieved iv (Just h) p'
		getrest p h iv sz (addBytesProcessed bytesprocessed sz) ks

	getunchunked = do
		iv <- startVerifyKeyContentIncrementally vc basek
		case enc of
			Just _ -> do
				retriever (encryptor basek) basep Nothing $
					retrieved iv Nothing basep
				return (Right iv)
			-- Not chunked and not encrypted, so ask the
			-- retriever to incrementally verify when it
			-- retrieves to a file. It may not finish
			-- passing the whole file content to the
			-- incremental verifier though.
			Nothing -> do
				retriever (encryptor basek) basep iv $
					retrieved iv Nothing basep
				return $ case iv of
					Nothing -> Right iv
					Just iv' -> Left (IncompleteVerify iv')

	opennew = do
		iv <- startVerifyKeyContentIncrementally vc basek
		h <- liftIO $ openBinaryFile dest WriteMode
		return (h, iv)

	-- Open the file and seek to the start point in order to resume.
	openresume startpoint = do
		-- ReadWriteMode allows seeking; AppendMode does not.
		h <- liftIO $ openBinaryFile dest ReadWriteMode
		liftIO $ hSeek h AbsoluteSeek startpoint
		-- No incremental verification when resuming, since that
		-- would need to read up to the startpoint.
		let iv = Nothing
		return (h, iv)

	{- Progress meter updating is a bit tricky: If the Retriever
	 - populates a file, it is responsible for updating progress
	 - as the file is being retrieved. 
	 -
	 - However, if the Retriever generates a lazy ByteString,
	 - it is not responsible for updating progress (often it cannot).
	 - Instead, writeRetrievedContent is passed a meter to update
	 - as it consumes the ByteString.
	 -}
	retrieved iv h p content =
		writeRetrievedContent dest enc encc h p' content iv
	  where
		p'
			| isByteContent content = Just p
			| otherwise = Nothing
	
	finalize (Right Nothing) = return UnVerified
	finalize (Right (Just iv)) =
		liftIO (finalizeIncrementalVerifier iv) >>= \case
			Just True -> return Verified
			_ -> return UnVerified
	finalize (Left v) = return v

{- Writes retrieved file content to the provided Handle, decrypting it
 - first if necessary.
 - 
 - If the remote did not store the content using chunks, no Handle
 - will be provided, and instead the content will be written to the
 - dest file.
 -
 - The IncrementalVerifier is updated as the file content is read.
 -
 - Note that when neither chunking nor encryption is used, and the remote
 - provides FileContent, that file only needs to be renamed
 - into place. (And it may even already be in the right place..)
 -}
writeRetrievedContent
	:: LensGpgEncParams encc
	=> FilePath
	-> Maybe (Cipher, EncKey)
	-> encc
	-> Maybe Handle
	-> Maybe MeterUpdate
	-> ContentSource
	-> Maybe IncrementalVerifier
	-> Annex ()
writeRetrievedContent dest enc encc mh mp content miv = case (enc, mh, content) of
	(Nothing, Nothing, FileContent f)
		| f == dest -> noop
		| otherwise -> liftIO $ moveFile (toRawFilePath f) (toRawFilePath dest)
	(Just (cipher, _), _, ByteContent b) -> do
		cmd <- gpgCmd <$> Annex.getGitConfig
		decrypt cmd encc cipher (feedBytes b) $
			readBytes write
	(Just (cipher, _), _, FileContent f) -> do
		cmd <- gpgCmd <$> Annex.getGitConfig
		withBytes content $ \b ->
			decrypt cmd encc cipher (feedBytes b) $
				readBytes write
		liftIO $ removeWhenExistsWith R.removeLink (toRawFilePath f)
	(Nothing, _, FileContent f) -> do
		withBytes content write
		liftIO $ removeWhenExistsWith R.removeLink (toRawFilePath f)
	(Nothing, _, ByteContent b) -> write b
  where
	write b = case mh of
		Just h -> liftIO $ write' b h
		Nothing -> liftIO $ bracket opendest hClose (write' b)
	write' b h = case mp of
		Just p -> 
			let writer = case miv of
				Just iv -> \s -> do
					updateIncrementalVerifier iv s
					S.hPut h s
				Nothing -> S.hPut h
			in meteredWrite p writer b
		Nothing -> L.hPut h b
	opendest = openBinaryFile dest WriteMode

{- Can resume when the chunk's offset is at or before the end of
 - the dest file. -}
resumeOffset :: Maybe Integer -> Key -> Maybe Integer
resumeOffset Nothing _ = Nothing
resumeOffset currsize k
	| offset <= currsize = offset
	| otherwise = Nothing
  where
	offset = chunkKeyOffset k

{- Drops chunks that are already present in a file, based on its size.
 - Keeps any non-chunk keys.
 -}
setupResume :: [[Key]] -> Integer -> [[Key]]
setupResume ls currsize = map dropunneeded ls
  where
	dropunneeded [] = []
	dropunneeded l@(k:_) = case fromKey keyChunkSize k of
		Just chunksize | chunksize > 0 ->
			genericDrop (currsize `div` chunksize) l
		_ -> l

{- Checks if a key is present in a remote. This requires any one
 - of the lists of options returned by chunkKeys to all check out
 - as being present using the checker action.
 -
 - Throws an exception if the remote is not accessible.
 -}
checkPresentChunks
	:: CheckPresent
	-> UUID
	-> ChunkConfig
	-> EncKey
	-> Key
	-> Annex Bool
checkPresentChunks checker u chunkconfig encryptor basek
	| noChunks chunkconfig = do
		-- Optimisation: Try the unchunked key first, to avoid
		-- looking in the git-annex branch for chunk counts
		-- that are likely not there.
		v <- check basek
		let getchunkkeys = chunkKeysOnly u chunkconfig basek
		case v of
			Right True -> return True
			Left e -> checklists (Just e) =<< getchunkkeys
			_ -> checklists Nothing =<< getchunkkeys
	| otherwise = checklists Nothing =<< chunkKeys u chunkconfig basek
  where
	checklists Nothing [] = return False
	checklists (Just deferrederror) [] = throwM deferrederror
	checklists d (ck:cks)
		| not (null l) = do
			v <- checkchunks l
			case v of
				Left e -> checklists (Just e) cks
				Right True -> do
					ensureChunksAreLogged u basek ck
					return True
				Right False -> checklists Nothing cks
		| otherwise = checklists d cks
	  where
		l = chunkKeyList ck
	
	checkchunks :: [Key] -> Annex (Either SomeException Bool)
	checkchunks [] = return (Right True)
	checkchunks (k:ks) = do
		v <- check k
		case v of
			Right True -> checkchunks ks
			Right False -> return $ Right False
			Left e -> return $ Left e

	check = tryNonAsync . checker . encryptor

data ChunkKeys
	= ChunkKeys [Key]
	| SpeculativeChunkKeys (ChunkMethod, ChunkCount) [Key]

chunkKeyList :: ChunkKeys -> [Key]
chunkKeyList (ChunkKeys l) = l
chunkKeyList (SpeculativeChunkKeys _ l) = l

{- A key can be stored in a remote unchunked, or as a list of chunked keys.
 - This can be the case whether or not the remote is currently configured
 - to use chunking.
 -
 - It's even possible for a remote to have the same key stored multiple
 - times with different chunk sizes!
 -
 - This finds all possible lists of keys that might be on the remote that
 - can be combined to get back the requested key, in order from most to
 - least likely to exist.
 -
 - Speculatively tries chunks using the ChunkConfig last of all
 - (when that's not the same as the recorded chunks). This can help
 - recover from data loss, where the chunk log didn't make it out,
 - though only as long as the ChunkConfig is unchanged.
 -}
chunkKeys :: UUID -> ChunkConfig -> Key -> Annex [ChunkKeys]
chunkKeys = chunkKeys' False

{- Same as chunkKeys, but excluding the unchunked key. -}
chunkKeysOnly :: UUID -> ChunkConfig -> Key -> Annex [ChunkKeys]
chunkKeysOnly = chunkKeys' True

chunkKeys' :: Bool -> UUID -> ChunkConfig -> Key -> Annex [ChunkKeys]
chunkKeys' onlychunks u chunkconfig k = do
	recorded <- getCurrentChunks u k
	let recordedl = map (ChunkKeys . toChunkList k) recorded
	return $ addspeculative recorded $ if onlychunks
		then recordedl
		else if noChunks chunkconfig
			then ChunkKeys [k] : recordedl
			else recordedl ++ [ChunkKeys [k]]
  where
	addspeculative recorded l = case chunkconfig of
		NoChunks -> l
		UnpaddedChunks chunksz -> case fromKey keySize k of
			Nothing -> l
			Just keysz -> 
				let (d, m) = keysz `divMod` fromIntegral chunksz
				    chunkcount = max 1 (d + if m == 0 then 0 else 1)
 				    v = (FixedSizeChunks chunksz, chunkcount)
				in if v `elem` recorded
					then l
					else l ++ [SpeculativeChunkKeys v (toChunkList k v)]
		LegacyChunks _ -> l

toChunkList :: Key -> (ChunkMethod, ChunkCount) -> [Key]
toChunkList k (FixedSizeChunks chunksize, chunkcount) =
	takeChunkKeyStream chunkcount $ chunkKeyStream k chunksize
toChunkList _ (UnknownChunks _, _) = []

{- When chunkKeys provided a speculative chunk list, and that has been
 - verified to be present, use this to log it in the chunk log. This way,
 - a later change to the chunk size of the remote won't prevent accessing
 - the chunks. -}
ensureChunksAreLogged :: UUID -> Key -> ChunkKeys -> Annex ()
ensureChunksAreLogged u k (SpeculativeChunkKeys (chunkmethod, chunkcount) _) = 
	chunksStored u k chunkmethod chunkcount
ensureChunksAreLogged _ _ (ChunkKeys _) = return ()

withBytes :: ContentSource -> (L.ByteString -> Annex a) -> Annex a
withBytes (ByteContent b) a = a b
withBytes (FileContent f) a = a =<< liftIO (L.readFile f)
