{- Remotes that support both chunking and encryption.
 -
 - Copyright 2014 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Remote.Helper.ChunkedEncryptable (
	chunkedEncryptableRemote,
	PrepareStorer,
	Storer,
	PrepareRetriever,
	Retriever,
	storeKeyDummy,
	retreiveKeyFileDummy,
	module X
) where

import qualified Data.ByteString.Lazy as L

import Common.Annex
import Types.Remote
import Crypto
import Config.Cost
import Utility.Metered
import Remote.Helper.Chunked as X
import Remote.Helper.Encryptable as X
import Annex.Content
import Annex.Exception

-- Prepares to store a Key, and returns a Storer action if possible.
type PrepareStorer = Key -> Annex (Maybe Storer)

-- Stores a Key, which may be encrypted and/or a chunk key.
type Storer = Key -> L.ByteString -> MeterUpdate -> IO Bool

-- Prepares to retrieve a Key, and returns a Retriever action if possible.
type PrepareRetriever = Key -> Annex (Maybe Retriever)

-- Retrieves a Key, which may be encrypted and/or a chunk key.
-- Throws exception if key is not present, or remote is not accessible.
type Retriever = Key -> IO L.ByteString

{- Modifies a base Remote to support both chunking and encryption.
 -}
chunkedEncryptableRemote
	:: RemoteConfig
	-> PrepareStorer
	-> PrepareRetriever
	-> Remote
	-> Remote
chunkedEncryptableRemote c preparestorer prepareretriever r = encr
  where
	encr = r
		{ storeKey = \k _f p -> cip >>= storeKeyGen k p
		, retrieveKeyFile = \k _f d p -> cip >>= retrieveKeyFileGen k d p
		, retrieveKeyFileCheap = \k d -> cip >>= maybe
			(retrieveKeyFileCheap r k d)
			(\_ -> return False)
		, removeKey = \k -> cip >>= removeKeyGen k
		, hasKey = \k -> cip >>= hasKeyGen k
		, cost = maybe
			(cost r)
			(const $ cost r + encryptedRemoteCostAdj)
			(extractCipher c)
		}
	cip = cipherKey c
	chunkconfig = chunkConfig c
	gpgopts = getGpgEncParams encr

	-- chunk, then encrypt, then feed to the storer
	storeKeyGen k p enc = maybe (return False) go =<< preparestorer k
	  where
		go storer = sendAnnex k rollback $ \src ->
			metered (Just p) k $ \p' ->
				storeChunks (uuid r) chunkconfig k src p' $
					storechunk storer
		rollback = void $ removeKey encr k
		storechunk storer k' b p' = case enc of
			Nothing -> storer k' b p'
			Just (cipher, enck) -> 
				encrypt gpgopts cipher (feedBytes b) $
					readBytes $ \encb ->
						storer (enck k') encb p'

	-- call retriever to get chunks; decrypt them; stream to dest file
	retrieveKeyFileGen k dest p enc =
		maybe (return False) go =<< prepareretriever k
	  where
	  	go retriever = metered (Just p) k $ \p' ->
			bracketIO (openBinaryFile dest WriteMode) hClose $ \h ->
				retrieveChunks retriever (uuid r) chunkconfig enck k p' $
					sink h
		sink h p' b = do
			let write = meteredWrite p' h
			case enc of
				Nothing -> write b
				Just (cipher, _) ->
					decrypt cipher (feedBytes b) $
						readBytes write
		enck = maybe id snd enc

	removeKeyGen k enc = removeChunks remover (uuid r) chunkconfig enck k
	  where
		enck = maybe id snd enc
		remover = removeKey r

	hasKeyGen k enc = hasKeyChunks checker (uuid r) chunkconfig enck k
	  where
		enck = maybe id snd enc
		checker = hasKey r

{- The base Remote that is provided to chunkedEncryptableRemote
 - needs to have storeKey and retreiveKeyFile methods, but they are
 - never actually used (since chunkedEncryptableRemote replaces
 - them). Here are some dummy ones.
 -}
storeKeyDummy :: Key -> AssociatedFile -> MeterUpdate -> Annex Bool
storeKeyDummy _ _ _ = return False
retreiveKeyFileDummy :: Key -> AssociatedFile -> FilePath -> MeterUpdate -> Annex Bool
retreiveKeyFileDummy _ _ _ _ = return False
