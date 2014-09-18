{- common functions for encryptable remotes
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Remote.Helper.Encryptable (
	EncryptionIsSetup,
	encryptionSetup,
	noEncryptionUsed,
	encryptionAlreadySetup,
	remoteCipher,
	embedCreds,
	cipherKey,
	storeCipher,
	extractCipher,
) where

import qualified Data.Map as M

import Common.Annex
import Types.Remote
import Crypto
import Types.Crypto
import qualified Annex
import Utility.Base64

-- Used to ensure that encryption has been set up before trying to
-- eg, store creds in the remote config that would need to use the
-- encryption setup.
data EncryptionIsSetup = EncryptionIsSetup | NoEncryption

-- Remotes that don't use encryption can use this instead of
-- encryptionSetup.
noEncryptionUsed :: EncryptionIsSetup
noEncryptionUsed = NoEncryption

-- Using this avoids the type-safe check, so you'd better be sure
-- of what you're doing.
encryptionAlreadySetup :: EncryptionIsSetup
encryptionAlreadySetup = EncryptionIsSetup

{- Encryption setup for a remote. The user must specify whether to use
 - an encryption key, or not encrypt. An encrypted cipher is created, or is
 - updated to be accessible to an additional encryption key. Or the user
 - could opt to use a shared cipher, which is stored unencrypted. -}
encryptionSetup :: RemoteConfig -> Annex (RemoteConfig, EncryptionIsSetup)
encryptionSetup c = maybe genCipher updateCipher $ extractCipher c
  where
	-- The type of encryption
	encryption = M.lookup "encryption" c
	-- Generate a new cipher, depending on the chosen encryption scheme
	genCipher = case encryption of
		_ | M.member "cipher" c || M.member "cipherkeys" c -> cannotchange
		Just "none" -> return (c, NoEncryption)
		Just "shared" -> use "encryption setup" . genSharedCipher
			=<< highRandomQuality
		-- hybrid encryption is the default when a keyid is
                -- specified but no encryption
		_ | maybe (M.member "keyid" c) (== "hybrid") encryption ->
			use "encryption setup" . genEncryptedCipher key Hybrid
				=<< highRandomQuality
		Just "pubkey" -> use "encryption setup" . genEncryptedCipher key PubKey
				=<< highRandomQuality
		_ -> error $ "Specify " ++ intercalate " or "
			(map ("encryption=" ++)
				["none","shared","hybrid","pubkey"])
			++ "."
	key = fromMaybe (error "Specifiy keyid=...") $ M.lookup "keyid" c
	newkeys = maybe [] (\k -> [(True,k)]) (M.lookup "keyid+" c) ++
		maybe [] (\k -> [(False,k)]) (M.lookup "keyid-" c)
	cannotchange = error "Cannot set encryption type of existing remotes."
	-- Update an existing cipher if possible.
	updateCipher v = case v of
		SharedCipher _ | maybe True (== "shared") encryption -> return (c', EncryptionIsSetup)
		EncryptedCipher _ variant _
			| maybe True (== if variant == Hybrid then "hybrid" else "pubkey") encryption ->
				use "encryption update" $ updateEncryptedCipher newkeys v
		_ -> cannotchange
	use m a = do
		showNote m
		cipher <- liftIO a
		showNote $ describeCipher cipher
		return (storeCipher c' cipher, EncryptionIsSetup)
	highRandomQuality = 
		(&&) (maybe True ( /= "false") $ M.lookup "highRandomQuality" c)
			<$> fmap not (Annex.getState Annex.fast)
	c' = foldr M.delete c
                -- git-annex used to remove 'encryption' as well, since
                -- it was redundant; we now need to keep it for
                -- public-key encryption, hence we leave it on newer
                -- remotes (while being backward-compatible).
		[ "keyid", "keyid+", "keyid-", "highRandomQuality" ]

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
			Nothing -> do
				showNote "gpg"
				cipher <- liftIO $ decryptCipher encipher
				Annex.changeState (\s -> s { Annex.ciphers = M.insert encipher cipher cache })
				return $ Just cipher

{- Checks if the remote's config allows storing creds in the remote's config.
 - 
 - embedcreds=yes allows this, and embedcreds=no prevents it.
 -
 - If not set, the default is to only store creds when it's surely safe:
 - When gpg encryption is used, in which case the creds will be encrypted
 - using it. Not when a shared cipher is used.
 -}
embedCreds :: RemoteConfig -> Bool
embedCreds c
	| M.lookup "embedcreds" c == Just "yes" = True
	| M.lookup "embedcreds" c == Just "no" = False
	| isJust (M.lookup "cipherkeys" c) && isJust (M.lookup "cipher" c) = True
	| otherwise = False

{- Gets encryption Cipher, and key encryptor. -}
cipherKey :: RemoteConfig -> Annex (Maybe (Cipher, EncKey))
cipherKey c = fmap make <$> remoteCipher c
  where
	make ciphertext = (ciphertext, encryptKey mac ciphertext)
	mac = fromMaybe defaultMac $ M.lookup "mac" c >>= readMac

{- Stores an StorableCipher in a remote's configuration. -}
storeCipher :: RemoteConfig -> StorableCipher -> RemoteConfig
storeCipher c (SharedCipher t) = M.insert "cipher" (toB64 t) c
storeCipher c (EncryptedCipher t _ ks) =
	M.insert "cipher" (toB64 t) $ M.insert "cipherkeys" (showkeys ks) c
  where
	showkeys (KeyIds l) = intercalate "," l

{- Extracts an StorableCipher from a remote's configuration. -}
extractCipher :: RemoteConfig -> Maybe StorableCipher
extractCipher c = case (M.lookup "cipher" c,
			M.lookup "cipherkeys" c,
			M.lookup "encryption" c) of
	(Just t, Just ks, encryption) | maybe True (== "hybrid") encryption ->
		Just $ EncryptedCipher (fromB64 t) Hybrid (readkeys ks)
	(Just t, Just ks, Just "pubkey") ->
		Just $ EncryptedCipher (fromB64 t) PubKey (readkeys ks)
	(Just t, Nothing, encryption) | maybe True (== "shared") encryption ->
		Just $ SharedCipher (fromB64 t)
	_ -> Nothing
  where
	readkeys = KeyIds . split ","
