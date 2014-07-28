{- Remotes that support both chunking and encryption.
 -
 - Copyright 2014 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE Rank2Types #-}

module Remote.Helper.ChunkedEncryptable (
	Preparer,
	simplyPrepare,
	checkPrepare,
	Storer,
	Retriever,
	chunkedEncryptableRemote,
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

-- Prepares for and then runs an action that will act on a Key,
-- passing it a helper when the preparation is successful.
type Preparer helper = forall a. Key -> (Maybe helper -> Annex a) -> Annex a

simplyPrepare :: helper -> Preparer helper
simplyPrepare helper _ a = a $ Just helper

checkPrepare :: (Key -> Annex Bool) -> helper -> Preparer helper
checkPrepare checker helper k a = ifM (checker k)
	( a (Just helper)
	, a Nothing
	)

-- Stores a Key, which may be encrypted and/or a chunk key.
-- May throw exceptions.
type Storer = Key -> L.ByteString -> MeterUpdate -> IO Bool

-- Retrieves a Key, which may be encrypted and/or a chunk key.
-- Throws exception if key is not present, or remote is not accessible.
type Retriever = Key -> IO L.ByteString

{- Modifies a base Remote to support both chunking and encryption.
 -}
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
					(storechunk storer)
					(hasKey baser)
		go Nothing = return False
		rollback = void $ removeKey encr k
		storechunk storer k' b p' = case enc of
			Nothing -> storer k' b p'
			Just (cipher, enck) -> 
				encrypt gpgopts cipher (feedBytes b) $
					readBytes $ \encb ->
						storer (enck k') encb p'

	-- call retriever to get chunks; decrypt them; stream to dest file
	retrieveKeyFileGen k dest p enc =
		safely $ prepareretriever k $ safely . go
	  where
	  	go (Just retriever) = metered (Just p) k $ \p' ->
			retrieveChunks retriever (uuid baser) chunkconfig
				enck k dest p' sink
		go Nothing = return False
		sink h p' b = do
			let write = meteredWrite p' h
			case enc of
				Nothing -> write b
				Just (cipher, _) ->
					decrypt cipher (feedBytes b) $
						readBytes write
		enck = maybe id snd enc

	removeKeyGen k enc = removeChunks remover (uuid baser) chunkconfig enck k
	  where
		enck = maybe id snd enc
		remover = removeKey baser

	hasKeyGen k enc = hasKeyChunks checker (uuid baser) chunkconfig enck k
	  where
		enck = maybe id snd enc
		checker = hasKey baser

{- The base Remote that is provided to chunkedEncryptableRemote
 - needs to have storeKey and retreiveKeyFile methods, but they are
 - never actually used (since chunkedEncryptableRemote replaces
 - them). Here are some dummy ones.
 -}
storeKeyDummy :: Key -> AssociatedFile -> MeterUpdate -> Annex Bool
storeKeyDummy _ _ _ = return False
retreiveKeyFileDummy :: Key -> AssociatedFile -> FilePath -> MeterUpdate -> Annex Bool
retreiveKeyFileDummy _ _ _ _ = return False
