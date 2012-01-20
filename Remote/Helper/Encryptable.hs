{- common functions for encryptable remotes
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Remote.Helper.Encryptable where

import qualified Data.Map as M

import Common.Annex
import Types.Remote
import Crypto
import qualified Annex
import Config

{- Encryption setup for a remote. The user must specify whether to use
 - an encryption key, or not encrypt. An encrypted cipher is created, or is
 - updated to be accessible to an additional encryption key. -}
encryptionSetup :: RemoteConfig -> Annex RemoteConfig
encryptionSetup c =
	case (M.lookup "encryption" c, extractCipher c) of
		(Nothing, Nothing) -> error "Specify encryption=key or encryption=none"
		(Just "none", Nothing) -> return c
		(Just "none", Just _) -> error "Cannot change encryption type of existing remote."
		(Nothing, Just _) -> return c
		(Just _, Nothing) -> use "encryption setup" $ genCipher c
		(Just _, Just v) -> use "encryption updated" $ updateCipher c v
	where
		use m a = do
			cipher <- liftIO a
			showNote $ m ++ " " ++ describeCipher cipher
			return $ M.delete "encryption" $ storeCipher c cipher

{- Modifies a Remote to support encryption.
 -
 - Two additional functions must be provided by the remote,
 - to support storing and retrieving encrypted content. -}
encryptableRemote
	:: Maybe RemoteConfig
	-> ((Cipher, Key) -> Key -> Annex Bool)
	-> ((Cipher, Key) -> FilePath -> Annex Bool)
	-> Remote
	-> Remote
encryptableRemote c storeKeyEncrypted retrieveKeyFileEncrypted r = 
	r {
		storeKey = store,
		retrieveKeyFile = retrieve,
		retrieveKeyFileCheap = retrieveCheap,
		removeKey = withkey $ removeKey r,
		hasKey = withkey $ hasKey r,
		cost = cost r + encryptedRemoteCostAdj
	}
	where
		store k = cip k >>= maybe
			(storeKey r k)
			(`storeKeyEncrypted` k)
		retrieve k f = cip k >>= maybe
			(retrieveKeyFile r k f)
			(`retrieveKeyFileEncrypted` f)
		retrieveCheap k f = cip k >>= maybe
			(retrieveKeyFileCheap r k f)
			(\_ -> return False)
		withkey a k = cip k >>= maybe (a k) (a . snd)
		cip = cipherKey c

{- Gets encryption Cipher. The decrypted Ciphers are cached in the Annex
 - state. -}
remoteCipher :: RemoteConfig -> Annex (Maybe Cipher)
remoteCipher c = go $ extractCipher c
	where
		go Nothing = return Nothing
		go (Just encipher) = do
			cache <- Annex.getState Annex.ciphers
			case M.lookup encipher cache of
				Just cipher -> return $ Just cipher
				Nothing -> decrypt encipher cache
		decrypt encipher cache = do
			showNote "gpg"
			cipher <- liftIO $ decryptCipher c encipher
			Annex.changeState (\s -> s { Annex.ciphers = M.insert encipher cipher cache })
			return $ Just cipher

{- Gets encryption Cipher, and encrypted version of Key. -}
cipherKey :: Maybe RemoteConfig -> Key -> Annex (Maybe (Cipher, Key))
cipherKey Nothing _ = return Nothing
cipherKey (Just c) k = maybe Nothing encrypt <$> remoteCipher c
	where
		encrypt ciphertext = Just (ciphertext, encryptKey ciphertext k)
