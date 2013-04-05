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
	GpgOpts(..),
	getGpgOpts,

	prop_HmacSha1WithCipher_sane
) where

import qualified Data.ByteString.Lazy as L
import Data.ByteString.Lazy.UTF8 (fromString)
import Control.Applicative

import Common.Annex
import qualified Utility.Gpg as Gpg
import Utility.Gpg.Types
import Types.Key
import Types.Crypto

{- The beginning of a Cipher is used for MAC'ing; the remainder is used
 - as the GPG symmetric encryption passphrase. Note that the cipher
 - itself is base-64 encoded, hence the string is longer than
 - 'cipherSize': 683 characters, padded to 684.
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
genEncryptedCipher :: String -> Bool -> IO StorableCipher
genEncryptedCipher keyid highQuality = do
	ks <- Gpg.findPubKeys keyid
	random <- Gpg.genRandom highQuality cipherSize
	encryptCipher (Cipher random) ks

{- Creates a new, shared Cipher. -}
genSharedCipher :: Bool -> IO StorableCipher
genSharedCipher highQuality =
	SharedCipher <$> Gpg.genRandom highQuality cipherSize

{- Updates an existing Cipher, re-encrypting it to add a keyid. -}
updateEncryptedCipher :: String -> StorableCipher -> IO StorableCipher
updateEncryptedCipher _ (SharedCipher _) = undefined
updateEncryptedCipher keyid encipher@(EncryptedCipher _ ks) = do
	ks' <- Gpg.findPubKeys keyid
	cipher <- decryptCipher encipher
	encryptCipher cipher (merge ks ks')
  where
	merge (KeyIds a) (KeyIds b) = KeyIds $ a ++ b

describeCipher :: StorableCipher -> String
describeCipher (SharedCipher _) = "shared cipher"
describeCipher (EncryptedCipher _ (KeyIds ks)) =
	"with gpg " ++ keys ks ++ " " ++ unwords ks
  where
	keys [_] = "key"
	keys _ = "keys"

{- Encrypts a Cipher to the specified KeyIds. -}
encryptCipher :: Cipher -> KeyIds -> IO StorableCipher
encryptCipher (Cipher c) (KeyIds ks) = do
	-- gpg complains about duplicate recipient keyids
	let ks' = nub $ sort ks
	encipher <- Gpg.pipeStrict (Params "--encrypt" : recipients ks') c
	return $ EncryptedCipher encipher (KeyIds ks')
  where
	recipients l = force_recipients :
		concatMap (\k -> [Param "--recipient", Param k]) l
	-- Force gpg to only encrypt to the specified
	-- recipients, not configured defaults.
	force_recipients = Params "--no-encrypt-to --no-default-recipient"

{- Decrypting an EncryptedCipher is expensive; the Cipher should be cached. -}
decryptCipher :: StorableCipher -> IO Cipher
decryptCipher (SharedCipher t) = return $ Cipher t
decryptCipher (EncryptedCipher t _) =
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

{- Runs a Feeder action, that generates content that is symmetrically encrypted
 - with the Cipher using the given GnuPG options, and then read by the Reader
 - action. -}
encrypt :: GpgOpts -> Cipher -> Feeder -> Reader a -> IO a
encrypt opts = Gpg.feedRead ( Params "--symmetric --force-mdc" : toParams opts )
		. cipherPassphrase

{- Runs a Feeder action, that generates content that is decrypted with the
 - Cipher, and read by the Reader action. -}
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
