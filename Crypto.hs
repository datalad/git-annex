{- git-annex crypto
 -
 - Currently using gpg; could later be modified to support different
 - crypto backends if neccessary.
 -
 - Copyright 2011-2016 Joey Hess <id@joeyh.name>
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
	genSharedPubKeyCipher,
	updateCipherKeyIds,
	decryptCipher,		
	encryptKey,
	isEncKey,
	feedFile,
	feedBytes,
	readBytes,
	encrypt,
	decrypt,
	LensGpgEncParams(..),

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
genEncryptedCipher :: LensGpgEncParams c => Gpg.GpgCmd -> c -> Gpg.KeyId -> EncryptedCipherVariant -> Bool -> IO StorableCipher
genEncryptedCipher cmd c keyid variant highQuality = do
	ks <- Gpg.findPubKeys cmd keyid
	random <- Gpg.genRandom cmd highQuality size
	encryptCipher cmd c (mkCipher random) variant ks
  where
	(mkCipher, size) = case variant of
		Hybrid -> (Cipher, cipherSize) -- used for MAC + symmetric
		PubKey -> (MacOnlyCipher, cipherBeginning) -- only used for MAC

{- Creates a new, shared Cipher. -}
genSharedCipher :: Gpg.GpgCmd -> Bool -> IO StorableCipher
genSharedCipher cmd highQuality =
	SharedCipher <$> Gpg.genRandom cmd highQuality cipherSize

{- Creates a new, shared Cipher, and looks up the gpg public key that will
 - be used for encrypting content. -}
genSharedPubKeyCipher :: Gpg.GpgCmd -> Gpg.KeyId -> Bool -> IO StorableCipher
genSharedPubKeyCipher cmd keyid highQuality = do
	ks <- Gpg.findPubKeys cmd keyid
	random <- Gpg.genRandom cmd highQuality cipherSize
	return $ SharedPubKeyCipher random ks

{- Updates an existing Cipher, making changes to its keyids.
 -
 - When the Cipher is encrypted, re-encrypts it. -}
updateCipherKeyIds :: LensGpgEncParams encparams => Gpg.GpgCmd -> encparams -> [(Bool, Gpg.KeyId)] -> StorableCipher -> IO StorableCipher
updateCipherKeyIds _ _ _ SharedCipher{} = giveup "Cannot update shared cipher"
updateCipherKeyIds _ _ [] c = return c
updateCipherKeyIds cmd encparams changes encipher@(EncryptedCipher _ variant ks) = do
	ks' <- updateCipherKeyIds' cmd changes ks
	cipher <- decryptCipher cmd encparams encipher
	encryptCipher cmd encparams cipher variant ks'
updateCipherKeyIds cmd _ changes (SharedPubKeyCipher cipher ks) =
	SharedPubKeyCipher cipher <$> updateCipherKeyIds' cmd changes ks

updateCipherKeyIds' :: Gpg.GpgCmd -> [(Bool, Gpg.KeyId)] -> KeyIds -> IO KeyIds
updateCipherKeyIds' cmd changes (KeyIds ks) = do
	dropkeys <- listKeyIds [ k | (False, k) <- changes ]
	forM_ dropkeys $ \k -> unless (k `elem` ks) $
		giveup $ "Key " ++ k ++ " was not present; cannot remove."
	addkeys <- listKeyIds [ k | (True, k) <- changes ]
	let ks' = (addkeys ++ ks) \\ dropkeys
	when (null ks') $
		giveup "Cannot remove the last key."
	return $ KeyIds ks'
  where
	listKeyIds = concat <$$> mapM (keyIds <$$> Gpg.findPubKeys cmd)

{- Encrypts a Cipher to the specified KeyIds. -}
encryptCipher :: LensGpgEncParams c => Gpg.GpgCmd -> c -> Cipher -> EncryptedCipherVariant -> KeyIds -> IO StorableCipher
encryptCipher cmd c cip variant (KeyIds ks) = do
	-- gpg complains about duplicate recipient keyids
	let ks' = nub $ sort ks
	let params = concat
		[ getGpgEncParamsBase c
		, Gpg.pkEncTo ks'
		, Gpg.stdEncryptionParams False
		]
	encipher <- Gpg.pipeStrict cmd params cipher
	return $ EncryptedCipher encipher variant (KeyIds ks')
  where
	cipher = case cip of
		Cipher x -> x
		MacOnlyCipher x -> x

{- Decrypting an EncryptedCipher is expensive; the Cipher should be cached. -}
decryptCipher :: LensGpgEncParams c => Gpg.GpgCmd -> c -> StorableCipher -> IO Cipher
decryptCipher _ _ (SharedCipher t) = return $ Cipher t
decryptCipher _ _ (SharedPubKeyCipher t _) = return $ MacOnlyCipher t
decryptCipher cmd c (EncryptedCipher t variant _) =
	mkCipher <$> Gpg.pipeStrict cmd params t
  where
	mkCipher = case variant of
		Hybrid -> Cipher
		PubKey -> MacOnlyCipher
	params = Param "--decrypt" : getGpgDecParams c

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
 - read by the Reader action. -}
encrypt :: (MonadIO m, MonadMask m, LensGpgEncParams c) => Gpg.GpgCmd -> c -> Cipher -> Feeder -> Reader m a -> m a
encrypt cmd c cipher = case cipher of
	Cipher{} -> Gpg.feedRead cmd (params ++ Gpg.stdEncryptionParams True) $
			cipherPassphrase cipher
	MacOnlyCipher{} -> Gpg.pipeLazy cmd $ params ++ Gpg.stdEncryptionParams False
  where
	params = getGpgEncParams c

{- Runs a Feeder action, that generates content that is decrypted with the
 - Cipher (or using a private key if the Cipher is empty), and read by the
 - Reader action. -}
decrypt :: (MonadIO m, MonadMask m, LensGpgEncParams c) => Gpg.GpgCmd -> c -> Cipher -> Feeder -> Reader m a -> m a
decrypt cmd c cipher = case cipher of
	Cipher{} -> Gpg.feedRead cmd params $ cipherPassphrase cipher
	MacOnlyCipher{} -> Gpg.pipeLazy cmd params
  where
	params = Param "--decrypt" : getGpgDecParams c

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
	{- Base parameters for encrypting. Does not include specification
	 - of recipient keys. -}
	getGpgEncParamsBase :: a -> [CommandParam]
	{- Parameters for encrypting. When the remote is configured to use
	 - public-key encryption, includes specification of recipient keys. -}
	getGpgEncParams :: a -> [CommandParam]
	{- Parameters for decrypting. -}
	getGpgDecParams :: a -> [CommandParam]

{- Extract the GnuPG options from a pair of a Remote Config and a Remote
 - Git Config. -}
instance LensGpgEncParams (RemoteConfig, RemoteGitConfig) where
	getGpgEncParamsBase (_c,gc) = map Param (remoteAnnexGnupgOptions gc)
	getGpgEncParams (c,gc) = getGpgEncParamsBase (c,gc) ++
 		{- When the remote is configured to use public-key encryption,
		 - look up the recipient keys and add them to the option list. -}
		case M.lookup "encryption" c of
			Just "pubkey" -> Gpg.pkEncTo $ maybe [] (split ",") $ M.lookup "cipherkeys" c
			Just "sharedpubkey" -> Gpg.pkEncTo $ maybe [] (split ",") $ M.lookup "pubkeys" c
			_ -> []
	getGpgDecParams (_c,gc) = map Param (remoteAnnexGnupgDecryptOptions gc)

{- Extract the GnuPG options from a Remote. -}
instance LensGpgEncParams (RemoteA a) where
	getGpgEncParamsBase r = getGpgEncParamsBase (config r, gitconfig r)
	getGpgEncParams r = getGpgEncParams (config r, gitconfig r)
	getGpgDecParams r = getGpgDecParams (config r, gitconfig r)
