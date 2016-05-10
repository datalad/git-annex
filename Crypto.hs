{- git-annex crypto
 -
 - Currently using gpg; could later be modified to support different
 - crypto backends if neccessary.
 -
 - Copyright 2011-2014 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE Rank2Types #-}

module Crypto (
	Cipher,
	KeyIds(..),
	EncKey,
	StorableCipher(..),
	genEncryptedCipher,
	genSharedCipher,
	updateEncryptedCipher,
	describeCipher,
	decryptCipher,		
	encryptKey,
	isEncKey,
	feedFile,
	feedBytes,
	readBytes,
	encrypt,
	decrypt,
	getGpgEncParams,
	getGpgDecParams,

	prop_HmacSha1WithCipher_sane
) where

import qualified Data.ByteString.Lazy as L
import Data.ByteString.UTF8 (fromString)
import qualified Data.Map as M
import Control.Monad.IO.Class

import Annex.Common
import qualified Utility.Gpg as Gpg
import Types.Crypto
import Types.Remote

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
cipherPassphrase (MacOnlyCipher _) = error "MAC-only cipher"

cipherMac :: Cipher -> String
cipherMac (Cipher c) = take cipherBeginning c
cipherMac (MacOnlyCipher c) = c

{- Creates a new Cipher, encrypted to the specified key id. -}
genEncryptedCipher :: Gpg.GpgCmd -> String -> EncryptedCipherVariant -> Bool -> IO StorableCipher
genEncryptedCipher cmd keyid variant highQuality = do
	ks <- Gpg.findPubKeys cmd keyid
	random <- Gpg.genRandom cmd highQuality size
	encryptCipher cmd (mkCipher random) variant ks
  where
	(mkCipher, size) = case variant of
		Hybrid -> (Cipher, cipherSize) -- used for MAC + symmetric
		PubKey -> (MacOnlyCipher, cipherBeginning) -- only used for MAC

{- Creates a new, shared Cipher. -}
genSharedCipher :: Gpg.GpgCmd -> Bool -> IO StorableCipher
genSharedCipher cmd highQuality =
	SharedCipher <$> Gpg.genRandom cmd highQuality cipherSize

{- Updates an existing Cipher, re-encrypting it to add or remove keyids,
 - depending on whether the first component is True or False. -}
updateEncryptedCipher :: Gpg.GpgCmd -> [(Bool, String)] -> StorableCipher -> IO StorableCipher
updateEncryptedCipher _ _ SharedCipher{} = error "Cannot update shared cipher"
updateEncryptedCipher _ [] encipher = return encipher
updateEncryptedCipher cmd newkeys encipher@(EncryptedCipher _ variant (KeyIds ks)) = do
	dropKeys <- listKeyIds [ k | (False, k) <- newkeys ]
	forM_ dropKeys $ \k -> unless (k `elem` ks) $
		error $ "Key " ++ k ++ " was not present; cannot remove."
	addKeys <- listKeyIds [ k | (True, k) <- newkeys ]
	let ks' = (addKeys ++ ks) \\ dropKeys
	when (null ks') $
		error "Cannot remove the last key."
	cipher <- decryptCipher cmd encipher
	encryptCipher cmd cipher variant $ KeyIds ks'
  where
	listKeyIds = concat <$$> mapM (keyIds <$$> Gpg.findPubKeys cmd)

describeCipher :: StorableCipher -> String
describeCipher (SharedCipher _) = "shared cipher"
describeCipher (EncryptedCipher _ variant (KeyIds ks)) =
	scheme ++ " with gpg " ++ keys ks ++ " " ++ unwords ks
  where
	scheme = case variant of
		Hybrid -> "hybrid cipher"
		PubKey -> "pubkey crypto"
	keys [_] = "key"
	keys _ = "keys"

{- Encrypts a Cipher to the specified KeyIds. -}
encryptCipher :: Gpg.GpgCmd -> Cipher -> EncryptedCipherVariant -> KeyIds -> IO StorableCipher
encryptCipher cmd c variant (KeyIds ks) = do
	-- gpg complains about duplicate recipient keyids
	let ks' = nub $ sort ks
	let params = Gpg.pkEncTo ks' ++ Gpg.stdEncryptionParams False
	encipher <- Gpg.pipeStrict cmd params cipher
	return $ EncryptedCipher encipher variant (KeyIds ks')
  where
	cipher = case c of
		Cipher x -> x
		MacOnlyCipher x -> x

{- Decrypting an EncryptedCipher is expensive; the Cipher should be cached. -}
decryptCipher :: Gpg.GpgCmd -> StorableCipher -> IO Cipher
decryptCipher _ (SharedCipher t) = return $ Cipher t
decryptCipher cmd (EncryptedCipher t variant _) =
	mkCipher <$> Gpg.pipeStrict cmd [ Param "--decrypt" ] t
  where
	mkCipher = case variant of
		Hybrid -> Cipher
		PubKey -> MacOnlyCipher

type EncKey = Key -> Key

{- Generates an encrypted form of a Key. The encryption does not need to be
 - reversable, nor does it need to be the same type of encryption used
 - on content. It does need to be repeatable. -}
encryptKey :: Mac -> Cipher -> EncKey
encryptKey mac c k = stubKey
	{ keyName = macWithCipher mac c (key2file k)
	, keyBackendName = encryptedBackendNamePrefix ++ showMac mac
	}

encryptedBackendNamePrefix :: String
encryptedBackendNamePrefix = "GPG"

isEncKey :: Key -> Bool
isEncKey k = encryptedBackendNamePrefix `isPrefixOf` keyBackendName k

type Feeder = Handle -> IO ()
type Reader m a = Handle -> m a

feedFile :: FilePath -> Feeder
feedFile f h = L.hPut h =<< L.readFile f

feedBytes :: L.ByteString -> Feeder
feedBytes = flip L.hPut

readBytes :: (MonadIO m) => (L.ByteString -> m a) -> Reader m a
readBytes a h = liftIO (L.hGetContents h) >>= a

{- Runs a Feeder action, that generates content that is symmetrically
 - encrypted with the Cipher (unless it is empty, in which case
 - public-key encryption is used) using the given gpg options, and then
 - read by the Reader action.  Note: For public-key encryption,
 - recipients MUST be included in 'params' (for instance using
 - 'getGpgEncParams'). -}
encrypt :: (MonadIO m, MonadMask m) => Gpg.GpgCmd -> [CommandParam] -> Cipher -> Feeder -> Reader m a -> m a
encrypt cmd params cipher = case cipher of
	Cipher{} -> Gpg.feedRead cmd (params ++ Gpg.stdEncryptionParams True) $
			cipherPassphrase cipher
	MacOnlyCipher{} -> Gpg.pipeLazy cmd $ params ++ Gpg.stdEncryptionParams False

{- Runs a Feeder action, that generates content that is decrypted with the
 - Cipher (or using a private key if the Cipher is empty), and read by the
 - Reader action. -}
decrypt :: (MonadIO m, MonadMask m) => Gpg.GpgCmd -> [CommandParam] -> Cipher -> Feeder -> Reader m a -> m a
decrypt cmd params cipher = case cipher of
	Cipher{} -> Gpg.feedRead cmd params' $ cipherPassphrase cipher
	MacOnlyCipher{} -> Gpg.pipeLazy cmd params'
  where
	params' = Param "--decrypt" : params

macWithCipher :: Mac -> Cipher -> String -> String
macWithCipher mac c = macWithCipher' mac (cipherMac c)
macWithCipher' :: Mac -> String -> String -> String
macWithCipher' mac c s = calcMac mac (fromString c) (fromString s)

{- Ensure that macWithCipher' returns the same thing forevermore. -}
prop_HmacSha1WithCipher_sane :: Bool
prop_HmacSha1WithCipher_sane = known_good == macWithCipher' HmacSha1 "foo" "bar"
  where
	known_good = "46b4ec586117154dacd49d664e5d63fdc88efb51"

class LensGpgEncParams a where
	{- Parameters for encrypting. -}
	getGpgEncParams :: a -> [CommandParam]
	{- Parameters for decrypting. -}
	getGpgDecParams :: a -> [CommandParam]

{- Extract the GnuPG options from a pair of a Remote Config and a Remote
 - Git Config. -}
instance LensGpgEncParams (RemoteConfig, RemoteGitConfig) where
	getGpgEncParams (c,gc) = map Param (remoteAnnexGnupgOptions gc) ++ getGpgEncParams c
	getGpgDecParams (c,gc) = map Param (remoteAnnexGnupgDecryptOptions gc) ++ getGpgDecParams c

{- Extract the GnuPG options from a Remote Config, ignoring any
 - git config settings. (Which is ok if the remote is just being set up 
 - and so doesn't have any.) -}
instance LensGpgEncParams RemoteConfig where
 	{- If the remote is configured to use public-key encryption,
	 - look up the recipient keys and add them to the option list. -}
	getGpgEncParams c = case M.lookup "encryption" c of
		Just "pubkey" -> Gpg.pkEncTo $ maybe [] (split ",") $ M.lookup "cipherkeys" c
		_ -> []
	getGpgDecParams _ = []

{- Extract the GnuPG options from a Remote. -}
instance LensGpgEncParams (RemoteA a) where
	getGpgEncParams r = getGpgEncParams (config r, gitconfig r)
	getGpgDecParams r = getGpgDecParams (config r, gitconfig r)
