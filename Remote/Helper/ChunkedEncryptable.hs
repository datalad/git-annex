{- Remotes that support both chunking and encryption.
 -
 - Copyright 2014 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE RankNTypes #-}

module Remote.Helper.ChunkedEncryptable (
	Preparer,
	Storer,
	Retriever,
	simplyPrepare,
	ContentSource,
	checkPrepare,
	fileStorer,
	byteStorer,
	fileRetriever,
	byteRetriever,
	storeKeyDummy,
	retreiveKeyFileDummy,
	chunkedEncryptableRemote,
	module X
) where

import Common.Annex
import Types.StoreRetrieve
import Types.Remote
import Crypto
import Config.Cost
import Utility.Metered
import Remote.Helper.Chunked as X
import Remote.Helper.Encryptable as X
import Annex.Content
import Annex.Exception

import qualified Data.ByteString.Lazy as L
import Control.Exception (bracket)

-- Use when nothing needs to be done to prepare a helper.
simplyPrepare :: helper -> Preparer helper
simplyPrepare helper _ a = a $ Just helper

-- Use to run a check when preparing a helper.
checkPrepare :: (Key -> Annex Bool) -> helper -> Preparer helper
checkPrepare checker helper k a = ifM (checker k)
	( a (Just helper)
	, a Nothing
	)

-- A Storer that expects to be provided with a file containing
-- the content of the key to store.
fileStorer :: (Key -> FilePath -> MeterUpdate -> Annex Bool) -> Storer
fileStorer a k (FileContent f) m = a k f m
fileStorer a k (ByteContent b) m = withTmp k $ \f -> do
	liftIO $ L.writeFile f b
	a k f m

-- A Storer that expects to be provided with a L.ByteString of
-- the content to store.
byteStorer :: (Key -> L.ByteString -> MeterUpdate -> Annex Bool) -> Storer
byteStorer a k c m = withBytes c $ \b -> a k b m

-- A Retriever that writes the content of a Key to a provided file.
-- It is responsible for updating the progress meter as it retrieves data.
fileRetriever :: (FilePath -> Key -> MeterUpdate -> Annex ()) -> Retriever
fileRetriever a k m callback = do
	f <- prepTmp k
	a f k m
	callback (FileContent f)

-- A Retriever that generates a L.ByteString containing the Key's content.
byteRetriever :: (Key -> Annex L.ByteString) -> Retriever
byteRetriever a k _m callback = callback =<< (ByteContent <$> a k)

{- The base Remote that is provided to chunkedEncryptableRemote
 - needs to have storeKey and retreiveKeyFile methods, but they are
 - never actually used (since chunkedEncryptableRemote replaces
 - them). Here are some dummy ones.
 -}
storeKeyDummy :: Key -> AssociatedFile -> MeterUpdate -> Annex Bool
storeKeyDummy _ _ _ = return False
retreiveKeyFileDummy :: Key -> AssociatedFile -> FilePath -> MeterUpdate -> Annex Bool
retreiveKeyFileDummy _ _ _ _ = return False

-- Modifies a base Remote to support both chunking and encryption.
chunkedEncryptableRemote
	:: RemoteConfig
	-> Preparer Storer
	-> Preparer Retriever
	-> Remote
	-> Remote
chunkedEncryptableRemote c preparestorer prepareretriever baser = encr
  where
	encr = baser
		{ storeKey = \k _f p -> cip >>= storeKeyGen k p
		, retrieveKeyFile = \k _f d p -> cip >>= retrieveKeyFileGen k d p
		, retrieveKeyFileCheap = \k d -> cip >>= maybe
			(retrieveKeyFileCheap baser k d)
			(\_ -> return False)
		, removeKey = \k -> cip >>= removeKeyGen k
		, hasKey = \k -> cip >>= hasKeyGen k
		, cost = maybe
			(cost baser)
			(const $ cost baser + encryptedRemoteCostAdj)
			(extractCipher c)
		}
	cip = cipherKey c
	chunkconfig = chunkConfig c
	gpgopts = getGpgEncParams encr

	safely a = catchNonAsyncAnnex a (\e -> warning (show e) >> return False)

	-- chunk, then encrypt, then feed to the storer
	storeKeyGen k p enc =
		safely $ preparestorer k $ safely . go
	  where
		go (Just storer) = sendAnnex k rollback $ \src ->
			metered (Just p) k $ \p' ->
				storeChunks (uuid baser) chunkconfig k src p'
					(storechunk enc storer)
					(hasKey baser)
		go Nothing = return False
		rollback = void $ removeKey encr k

	storechunk Nothing storer k content p = storer k content p
	storechunk (Just (cipher, enck)) storer k content p =
		withBytes content $ \b ->
			encrypt gpgopts cipher (feedBytes b) $
				readBytes $ \encb ->
					storer (enck k) (ByteContent encb) p

	-- call retriever to get chunks; decrypt them; stream to dest file
	retrieveKeyFileGen k dest p enc =
		safely $ prepareretriever k $ safely . go
	  where
	  	go (Just retriever) = metered (Just p) k $ \p' ->
			retrieveChunks retriever (uuid baser) chunkconfig
				enck k dest p' (sink dest enc)
		go Nothing = return False
		enck = maybe id snd enc

	removeKeyGen k enc = removeChunks remover (uuid baser) chunkconfig enck k
	  where
		enck = maybe id snd enc
		remover = removeKey baser

	hasKeyGen k enc = hasKeyChunks checker (uuid baser) chunkconfig enck k
	  where
		enck = maybe id snd enc
		checker = hasKey baser

{- Sink callback for retrieveChunks. Stores the file content into the
 - provided Handle, decrypting it first if necessary.
 - 
 - If the remote did not store the content using chunks, no Handle
 - will be provided, and it's up to us to open the destination file.
 -
 - Note that when neither chunking nor encryption is used, and the remote
 - provides FileContent, that file only needs to be renamed
 - into place. (And it may even already be in the right place..)
 -}
sink
	:: FilePath
	-> Maybe (Cipher, EncKey)
	-> Maybe Handle
	-> Maybe MeterUpdate
	-> ContentSource
	-> Annex Bool
sink dest enc mh mp content = do
	case (enc, mh, content) of
		(Nothing, Nothing, FileContent f)
			| f == dest -> noop
			| otherwise -> liftIO $ moveFile f dest
		(Just (cipher, _), _, ByteContent b) ->
			decrypt cipher (feedBytes b) $
				readBytes write
		(Just (cipher, _), _, FileContent f) -> do
			withBytes content $ \b ->
				decrypt cipher (feedBytes b) $
					readBytes write
			liftIO $ nukeFile f
		(Nothing, _, FileContent f) -> do
			withBytes content write
			liftIO $ nukeFile f
		(Nothing, _, ByteContent b) -> write b
	return True
  where
	write b = case mh of
		Just h -> liftIO $ b `streamto` h
		Nothing -> liftIO $ bracket opendest hClose (b `streamto`)
	streamto b h = case mp of
		Just p -> meteredWrite p h b
		Nothing -> L.hPut h b
	opendest = openBinaryFile dest WriteMode

withBytes :: ContentSource -> (L.ByteString -> Annex a) -> Annex a
withBytes (ByteContent b) a = a b
withBytes (FileContent f) a = a =<< liftIO (L.readFile f)
