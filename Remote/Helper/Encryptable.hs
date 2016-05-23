{- common functions for encryptable remotes
 -
 - Copyright 2011 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Remote.Helper.Encryptable (
	EncryptionIsSetup,
	encryptionSetup,
	noEncryptionUsed,
	encryptionAlreadySetup,
	remoteCipher,
	remoteCipher',
	embedCreds,
	cipherKey,
	extractCipher,
	describeEncryption,
) where

import qualified Data.Map as M
import qualified "sandi" Codec.Binary.Base64 as B64
import qualified Data.ByteString as B
import Data.Bits.Utils

import Annex.Common
import Types.Remote
import Crypto
import Types.Crypto
import qualified Annex

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
encryptionSetup :: RemoteConfig -> RemoteGitConfig -> Annex (RemoteConfig, EncryptionIsSetup)
encryptionSetup c gc = do
	cmd <- gpgCmd <$> Annex.getGitConfig
	maybe (genCipher cmd) (updateCipher cmd) (extractCipher c)
  where
	-- The type of encryption
	encryption = M.lookup "encryption" c
	-- Generate a new cipher, depending on the chosen encryption scheme
	genCipher cmd = case encryption of
		_ | M.member "cipher" c || M.member "cipherkeys" c || M.member "pubkeys" c -> cannotchange
		Just "none" -> return (c, NoEncryption)
		Just "shared" -> encsetup $ genSharedCipher cmd
		-- hybrid encryption is the default when a keyid is
		-- specified but no encryption
		_ | maybe (M.member "keyid" c) (== "hybrid") encryption ->
			encsetup $ genEncryptedCipher cmd key Hybrid
		Just "pubkey" -> encsetup $ genEncryptedCipher cmd key PubKey
		Just "sharedpubkey" -> encsetup $ genSharedPubKeyCipher cmd key
		_ -> error $ "Specify " ++ intercalate " or "
			(map ("encryption=" ++)
				["none","shared","hybrid","pubkey", "sharedpubkey"])
			++ "."
	key = fromMaybe (error "Specifiy keyid=...") $ M.lookup "keyid" c
	newkeys = maybe [] (\k -> [(True,k)]) (M.lookup "keyid+" c) ++
		maybe [] (\k -> [(False,k)]) (M.lookup "keyid-" c)
	cannotchange = error "Cannot set encryption type of existing remotes."
	-- Update an existing cipher if possible.
	updateCipher cmd v = case v of
		SharedCipher _ | maybe True (== "shared") encryption -> return (c', EncryptionIsSetup)
		EncryptedCipher _ variant _
			| maybe True (== if variant == Hybrid then "hybrid" else "pubkey") encryption -> do
				use "encryption update" $ updateCipherKeyIds cmd (c, gc) newkeys v
		SharedPubKeyCipher _ _ ->
			use "encryption update" $ updateCipherKeyIds cmd (c, gc) newkeys v
		_ -> cannotchange
	encsetup a = use "encryption setup" . a =<< highRandomQuality
	use m a = do
		showNote m
		cipher <- liftIO a
		showNote (describeCipher cipher)
		return (storeCipher cipher c', EncryptionIsSetup)
	highRandomQuality = 
		(&&) (maybe True ( /= "false") $ M.lookup "highRandomQuality" c)
			<$> fmap not (Annex.getState Annex.fast)
	c' = foldr M.delete c
		-- git-annex used to remove 'encryption' as well, since
		-- it was redundant; we now need to keep it for
		-- public-key encryption, hence we leave it on newer
		-- remotes (while being backward-compatible).
		[ "keyid", "keyid+", "keyid-", "highRandomQuality" ]

remoteCipher :: RemoteConfig -> RemoteGitConfig -> Annex (Maybe Cipher)
remoteCipher c gc = fmap fst <$> remoteCipher' c gc

{- Gets encryption Cipher. The decrypted Ciphers are cached in the Annex
 - state. -}
remoteCipher' :: RemoteConfig -> RemoteGitConfig -> Annex (Maybe (Cipher, StorableCipher))
remoteCipher' c gc = go $ extractCipher c
  where
	go Nothing = return Nothing
	go (Just encipher) = do
		cache <- Annex.getState Annex.ciphers
		case M.lookup encipher cache of
			Just cipher -> return $ Just (cipher, encipher)
			Nothing -> do
				cmd <- gpgCmd <$> Annex.getGitConfig
				cipher <- liftIO $ decryptCipher cmd (c, gc) encipher
				Annex.changeState (\s -> s { Annex.ciphers = M.insert encipher cipher cache })
				return $ Just (cipher, encipher)

{- Checks if the remote's config allows storing creds in the remote's config.
 - 
 - embedcreds=yes allows this, and embedcreds=no prevents it.
 -
 - If not set, the default is to only store creds when it's surely safe:
 - When gpg encryption is used and the creds are encrypted using it.
 - Not when a shared cipher is used.
 -}
embedCreds :: RemoteConfig -> Bool
embedCreds c
	| M.lookup "embedcreds" c == Just "yes" = True
	| M.lookup "embedcreds" c == Just "no" = False
	| isJust (M.lookup "cipherkeys" c) && isJust (M.lookup "cipher" c) = True
	| otherwise = False

{- Gets encryption Cipher, and key encryptor. -}
cipherKey :: RemoteConfig -> RemoteGitConfig -> Annex (Maybe (Cipher, EncKey))
cipherKey c gc = fmap make <$> remoteCipher c gc
  where
	make ciphertext = (ciphertext, encryptKey mac ciphertext)
	mac = fromMaybe defaultMac $ M.lookup "mac" c >>= readMac

{- Stores an StorableCipher in a remote's configuration. -}
storeCipher :: StorableCipher -> RemoteConfig -> RemoteConfig
storeCipher cip = case cip of
	(SharedCipher t) -> addcipher t
	(EncryptedCipher t _ ks) -> addcipher t . storekeys ks "cipherkeys"
	(SharedPubKeyCipher t ks) -> addcipher t . storekeys ks "pubkeys"
  where
	addcipher t = M.insert "cipher" (toB64bs t)
	storekeys (KeyIds l) n = M.insert n (intercalate "," l)

{- Extracts an StorableCipher from a remote's configuration. -}
extractCipher :: RemoteConfig -> Maybe StorableCipher
extractCipher c = case (M.lookup "cipher" c,
			M.lookup "cipherkeys" c <|> M.lookup "pubkeys" c,
			M.lookup "encryption" c) of
	(Just t, Just ks, encryption) | maybe True (== "hybrid") encryption ->
		Just $ EncryptedCipher (fromB64bs t) Hybrid (readkeys ks)
	(Just t, Just ks, Just "pubkey") ->
		Just $ EncryptedCipher (fromB64bs t) PubKey (readkeys ks)
	(Just t, Just ks, Just "sharedpubkey") ->
		Just $ SharedPubKeyCipher (fromB64bs t) (readkeys ks)
	(Just t, Nothing, encryption) | maybe True (== "shared") encryption ->
		Just $ SharedCipher (fromB64bs t)
	_ -> Nothing
  where
	readkeys = KeyIds . split ","

describeEncryption :: RemoteConfig -> String
describeEncryption c = case extractCipher c of
	Nothing -> "none"
	Just cip -> nameCipher cip ++ " (" ++ describeCipher cip ++ ")"

nameCipher :: StorableCipher -> String
nameCipher (SharedCipher _) = "shared"
nameCipher (EncryptedCipher _ PubKey _) = "pubkey"
nameCipher (EncryptedCipher _ Hybrid _) = "hybrid"
nameCipher (SharedPubKeyCipher _ _) = "sharedpubkey"

describeCipher :: StorableCipher -> String
describeCipher c = case c of
	(SharedCipher _) -> "encryption key stored in git repository"
	(EncryptedCipher _ _ ks) -> showkeys ks
	(SharedPubKeyCipher _ ks) -> showkeys ks
  where
	showkeys (KeyIds { keyIds = ks }) = "to gpg keys: " ++ unwords ks

{- Not using Utility.Base64 because these "Strings" are really
 - bags of bytes and that would convert to unicode and not round-trip
 - cleanly. -}
toB64bs :: String -> String
toB64bs = w82s . B.unpack . B64.encode . B.pack . s2w8

fromB64bs :: String -> String
fromB64bs s = either (const bad) (w82s . B.unpack) (B64.decode $ B.pack $ s2w8 s)
  where
	bad = error "bad base64 encoded data"
