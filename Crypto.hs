{- git-annex crypto
 -
 - Currently using gpg; could later be modified to support different
 - crypto backends if neccessary.
 -
 - Copyright 2011-2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Crypto (
	Cipher,
	KeyIds(..),
	StorableCipher(..),
	genEncryptedCipher,
	genSharedCipher,
	updateEncryptedCipher,
	describeCipher,
	decryptCipher,		
	encryptKey,
	feedFile,
	feedBytes,
	readBytes,
	encrypt,
	decrypt,	
	Gpg.getGpgEncParams,

	prop_HmacSha1WithCipher_sane
) where

import qualified Data.ByteString.Lazy as L
import Data.ByteString.Lazy.UTF8 (fromString)
import Control.Applicative

import Common.Annex
import qualified Utility.Gpg as Gpg
import Types.Key
import Types.Crypto

{- The beginning of a Cipher is used for MAC'ing; the remainder is used
 - as the GPG symmetric encryption passphrase when using the hybrid
 - scheme. Note that the cipher itself is base-64 encoded, hence the
 - string is longer than 'cipherSize': 683 characters, padded to 684.
 -
 - The 256 first characters that feed the MAC represent at best 192
 - bytes of entropy.  However that's more than enough for both the
 - default MAC algorithm, namely HMAC-SHA1, and the "strongest"
 - currently supported, namely HMAC-SHA512, which respectively need
 - (ideally) 64 and 128 bytes of entropy.
 -
 - The remaining characters (320 bytes of entropy) is enough for GnuPG's
 - symetric cipher; unlike weaker public key crypto, the key does not
 - need to be too large.
 -}
cipherBeginning :: Int
cipherBeginning = 256

cipherSize :: Int
cipherSize = 512

cipherPassphrase :: Cipher -> String
cipherPassphrase (Cipher c) = drop cipherBeginning c

cipherMac :: Cipher -> String
cipherMac (Cipher c) = take cipherBeginning c

{- Creates a new Cipher, encrypted to the specified key id. -}
genEncryptedCipher :: String -> EncryptedCipherVariant -> Bool -> IO StorableCipher
genEncryptedCipher keyid variant highQuality = do
	ks <- Gpg.findPubKeys keyid
	random <- Gpg.genRandom highQuality size
	encryptCipher (Cipher random) variant ks
  where
	size = case variant of
		HybridCipher -> cipherSize -- used for MAC + symmetric
		PubKeyCipher -> cipherBeginning -- only used for MAC

{- Creates a new, shared Cipher. -}
genSharedCipher :: Bool -> IO StorableCipher
genSharedCipher highQuality =
	SharedCipher <$> Gpg.genRandom highQuality cipherSize

{- Updates an existing Cipher, re-encrypting it to add or remove keyids,
 - depending on whether the first component is True or False. -}
updateEncryptedCipher :: [(Bool, String)] -> StorableCipher -> IO StorableCipher
updateEncryptedCipher _ SharedCipher{} = undefined
updateEncryptedCipher [] encipher = return encipher
updateEncryptedCipher newkeys encipher@(EncryptedCipher _ symmetric (KeyIds ks)) = do
	dropKeys <- listKeyIds [ k | (False, k) <- newkeys ]
	forM_ dropKeys $ \k -> unless (k `elem` ks) $
		error $ "Key " ++ k ++ " was not present; cannot remove."
	addKeys <- listKeyIds [ k | (True, k) <- newkeys ]
	let ks' = (addKeys ++ ks) \\ dropKeys
	when (null ks') $
		error "Cannot remove the last key."
	cipher <- decryptCipher encipher
	encryptCipher cipher symmetric $ KeyIds ks'
  where
	listKeyIds = mapM (Gpg.findPubKeys >=*> keyIds) >=*> concat

describeCipher :: StorableCipher -> String
describeCipher (SharedCipher _) = "shared cipher"
describeCipher (EncryptedCipher _ variant (KeyIds ks)) =
	scheme ++ " with gpg " ++ keys ks ++ " " ++ unwords ks
  where
	scheme = case variant of
		HybridCipher -> "hybrid cipher"
		PubKeyCipher -> "pubkey crypto"
	keys [_] = "key"
	keys _ = "keys"

{- Encrypts a Cipher to the specified KeyIds. -}
encryptCipher :: Cipher -> EncryptedCipherVariant -> KeyIds -> IO StorableCipher
encryptCipher (Cipher c) variant (KeyIds ks) = do
	-- gpg complains about duplicate recipient keyids
	let ks' = nub $ sort ks
	let params = Gpg.pkEncTo ks' ++ Gpg.stdEncryptionParams False
	encipher <- Gpg.pipeStrict params c
	return $ EncryptedCipher encipher variant (KeyIds ks')

{- Decrypting an EncryptedCipher is expensive; the Cipher should be cached. -}
decryptCipher :: StorableCipher -> IO Cipher
decryptCipher (SharedCipher t) = return $ Cipher t
decryptCipher (EncryptedCipher t _ _) =
	Cipher <$> Gpg.pipeStrict [ Param "--decrypt" ] t

{- Generates an encrypted form of a Key. The encryption does not need to be
 - reversable, nor does it need to be the same type of encryption used
 - on content. It does need to be repeatable. -}
encryptKey :: Mac -> Cipher -> Key -> Key
encryptKey mac c k = Key
	{ keyName = macWithCipher mac c (key2file k)
	, keyBackendName = "GPG" ++ showMac mac
	, keySize = Nothing -- size and mtime omitted
	, keyMtime = Nothing -- to avoid leaking data
	}

type Feeder = Handle -> IO ()
type Reader a = Handle -> IO a

feedFile :: FilePath -> Feeder
feedFile f h = L.hPut h =<< L.readFile f

feedBytes :: L.ByteString -> Feeder
feedBytes = flip L.hPut

readBytes :: (L.ByteString -> IO a) -> Reader a
readBytes a h = L.hGetContents h >>= a

{- Runs a Feeder action, that generates content that is symmetrically
 - encrypted with the Cipher (unless it is empty, in which case
 - public-key encryption is used) using the given gpg options, and then
 - read by the Reader action.  Note: For public-key encryption,
 - recipients MUST be included in 'params' (for instance using
 - 'getGpgEncParams'). -}
encrypt :: [CommandParam] -> Cipher -> Feeder -> Reader a -> IO a
encrypt params cipher = Gpg.feedRead params' pass
  where
	pass = cipherPassphrase cipher
	params' = params ++ Gpg.stdEncryptionParams (not $ null pass)

{- Runs a Feeder action, that generates content that is decrypted with the
 - Cipher (or using a private key if the Cipher is empty), and read by the
 - Reader action. -}
decrypt :: Cipher -> Feeder -> Reader a -> IO a
decrypt = Gpg.feedRead [Param "--decrypt"] . cipherPassphrase

macWithCipher :: Mac -> Cipher -> String -> String
macWithCipher mac c = macWithCipher' mac (cipherMac c)
macWithCipher' :: Mac -> String -> String -> String
macWithCipher' mac c s = calcMac mac (fromString c) (fromString s)

{- Ensure that macWithCipher' returns the same thing forevermore. -}
prop_HmacSha1WithCipher_sane :: Bool
prop_HmacSha1WithCipher_sane = known_good == macWithCipher' HmacSha1 "foo" "bar"
  where
	known_good = "46b4ec586117154dacd49d664e5d63fdc88efb51"
