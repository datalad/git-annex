{- git-annex crypto
 -
 - Currently using gpg by default, or optionally stateless OpenPGP.
 -
 - Copyright 2011-2024 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types #-}

module Crypto (
	EncryptionMethod(..),
	Cipher,
	KeyIds(..),
	EncKey,
	StorableCipher(..),
	genEncryptedCipher,
	genSharedCipher,
	genSharedPubKeyCipher,
	updateCipherKeyIds,
	decryptCipher,
	decryptCipher',
	encryptKey,
	isEncKey,
	feedBytes,
	readBytes,
	readBytesStrictly,
	encrypt,
	decrypt,
	LensEncParams(..),

	prop_HmacSha1WithCipher_sane
) where

import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import Control.Monad.IO.Class
import qualified Data.ByteString.Short as S (toShort)

import Annex.Common
import qualified Utility.Gpg as Gpg
import qualified Utility.StatelessOpenPGP as SOP
import Types.Crypto
import Types.Remote
import Types.Key
import Annex.SpecialRemote.Config
import Utility.Tmp.Dir

{- The number of bytes of entropy used to generate a Cipher.
 -
 - Since a Cipher is base-64 encoded, the actual size of a Cipher
 - is larger than this. 512 bytes of date base-64 encodes to 684
 - characters.
 -}
cipherSize :: Int
cipherSize = 512

{- The beginning of a Cipher is used for MAC'ing; the remainder is used
 - as the symmetric encryption passphrase.
 -
 - Due to the base-64 encoding of the Cipher, the beginning 265 characters
 - represent at best 192 bytes of entropy. However that's more than enough
 - for both the default MAC algorithm, namely HMAC-SHA1, and the "strongest"
 - currently supported, namely HMAC-SHA512, which respectively need
 - (ideally) 64 and 128 bytes of entropy.
 -
 - The remaining characters (320 bytes of entropy) is enough for
 - the symmetric encryption passphrase; unlike weaker public key crypto,
 - that does not need to be too large.
 -}
cipherBeginning :: Int
cipherBeginning = 256

cipherPassphrase :: Cipher -> S.ByteString
cipherPassphrase (Cipher c) = S.drop cipherBeginning c
cipherPassphrase (MacOnlyCipher _) = giveup "MAC-only cipher"

cipherMac :: Cipher -> S.ByteString
cipherMac (Cipher c) = S.take cipherBeginning c
cipherMac (MacOnlyCipher c) = c

{- Creates a new Cipher, encrypted to the specified key id. -}
genEncryptedCipher :: LensEncParams c => Gpg.GpgCmd -> c -> Gpg.KeyId -> EncryptedCipherVariant -> Bool -> IO StorableCipher
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
updateCipherKeyIds :: LensEncParams encparams => Gpg.GpgCmd -> encparams -> [(Bool, Gpg.KeyId)] -> StorableCipher -> IO StorableCipher
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
encryptCipher :: LensEncParams c => Gpg.GpgCmd -> c -> Cipher -> EncryptedCipherVariant -> KeyIds -> IO StorableCipher
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
decryptCipher :: LensEncParams c => Gpg.GpgCmd -> c -> StorableCipher -> IO Cipher
decryptCipher cmd c cip = decryptCipher' cmd Nothing c cip

decryptCipher' :: LensEncParams c => Gpg.GpgCmd -> Maybe [(String, String)] -> c -> StorableCipher -> IO Cipher
decryptCipher' _ _ _ (SharedCipher t) = return $ Cipher t
decryptCipher' _ _ _ (SharedPubKeyCipher t _) = return $ MacOnlyCipher t
decryptCipher' cmd environ c (EncryptedCipher t variant _) =
	mkCipher <$> Gpg.pipeStrict' cmd params environ t
  where
	mkCipher = case variant of
		Hybrid -> Cipher
		PubKey -> MacOnlyCipher
	params = Param "--decrypt" : getGpgDecParams c

type EncKey = Key -> Key

{- Generates an encrypted form of a Key. The encryption does not need to be
 - reversible, nor does it need to be the same type of encryption used
 - on content. It does need to be repeatable. -}
encryptKey :: Mac -> Cipher -> EncKey
encryptKey mac c k = mkKey $ \d -> d
	{ keyName = S.toShort $ encodeBS $ macWithCipher mac c (serializeKey' k)
	, keyVariety = OtherKey $
		encryptedBackendNamePrefix <> encodeBS (showMac mac)
	}

encryptedBackendNamePrefix :: S.ByteString
encryptedBackendNamePrefix = "GPG"

isEncKey :: Key -> Bool
isEncKey k = case fromKey keyVariety k of
	OtherKey s -> encryptedBackendNamePrefix `S.isPrefixOf` s
	_ -> False

type Feeder = Handle -> IO ()
type Reader m a = Handle -> m a

feedBytes :: L.ByteString -> Feeder
feedBytes = flip L.hPut

readBytes :: (MonadIO m) => (L.ByteString -> m a) -> Reader m a
readBytes a h = liftIO (L.hGetContents h) >>= a

readBytesStrictly :: (MonadIO m) => (S.ByteString -> m a) -> Reader m a
readBytesStrictly a h = liftIO (S.hGetContents h) >>= a

{- Runs a Feeder action, that generates content that is symmetrically
 - encrypted with the Cipher (unless it is empty, in which case
 - public-key encryption is used), and then read by the Reader action. 
 -
 - Note that the Reader must fully consume all input before returning.
 -}
encrypt :: (MonadIO m, MonadMask m, LensEncParams c) => Gpg.GpgCmd -> c -> Cipher -> Feeder -> Reader m a -> m a
encrypt gpgcmd c cipher feeder reader = case cipher of
	Cipher{} -> 
		let passphrase = cipherPassphrase cipher
		in case statelessOpenPGPCommand c of
			Just sopcmd -> withTmpDir (literalOsPath "sop") $ \d ->
				SOP.encryptSymmetric sopcmd passphrase
					(SOP.EmptyDirectory d)
					(statelessOpenPGPProfile c)
					(SOP.Armoring False)
					feeder reader
			Nothing -> Gpg.feedRead gpgcmd (params ++ Gpg.stdEncryptionParams True) passphrase feeder reader
	MacOnlyCipher{} -> Gpg.feedRead' gpgcmd (params ++ Gpg.stdEncryptionParams False) feeder reader
  where
	params = getGpgEncParams c

{- Runs a Feeder action, that generates content that is decrypted with the
 - Cipher (or using a private key if the Cipher is empty), and read by the
 - Reader action.
 -
 - Note that the Reader must fully consume all input before returning.
 - -}
decrypt :: (MonadIO m, MonadMask m, LensEncParams c) => Gpg.GpgCmd -> c -> Cipher -> Feeder -> Reader m a -> m a
decrypt cmd c cipher feeder reader = case cipher of
	Cipher{} -> 
		let passphrase = cipherPassphrase cipher
		in case statelessOpenPGPCommand c of
			Just sopcmd -> withTmpDir (literalOsPath "sop") $ \d ->
				SOP.decryptSymmetric sopcmd passphrase
					(SOP.EmptyDirectory d)
					feeder reader
			Nothing -> Gpg.feedRead cmd params passphrase feeder reader
	MacOnlyCipher{} -> Gpg.feedRead' cmd params feeder reader
  where
	params = Param "--decrypt" : getGpgDecParams c

macWithCipher :: Mac -> Cipher -> S.ByteString -> String
macWithCipher mac c = macWithCipher' mac (cipherMac c)
macWithCipher' :: Mac -> S.ByteString -> S.ByteString -> String
macWithCipher' mac c s = calcMac show mac c s

{- Ensure that macWithCipher' returns the same thing forevermore. -}
prop_HmacSha1WithCipher_sane :: Bool
prop_HmacSha1WithCipher_sane = known_good == macWithCipher' HmacSha1 "foo" "bar"
  where
	known_good = "46b4ec586117154dacd49d664e5d63fdc88efb51"

class LensEncParams a where
	{- Base gpg parameters for encrypting. Does not include specification
	 - of recipient keys. -}
	getGpgEncParamsBase :: a -> [CommandParam]
	{- Gpg parameters for encrypting. When the remote is configured to use
	 - public-key encryption, includes specification of recipient keys. -}
	getGpgEncParams :: a -> [CommandParam]
	{- Gpg parameters for decrypting. -}
	getGpgDecParams :: a -> [CommandParam]
	{- Set when stateless OpenPGP should be used rather than gpg.
	 - It is currently only used for SharedEncryption and not the other
	 - schemes which use public keys. -}
	statelessOpenPGPCommand :: a -> Maybe SOP.SOPCmd
	{- When using stateless OpenPGP, this may be set to a profile
	 - which should be used instead of the default. -}
	statelessOpenPGPProfile :: a -> Maybe SOP.SOPProfile

{- Extract the GnuPG options from a pair of a Remote Config and a Remote
 - Git Config. -}
instance LensEncParams (ParsedRemoteConfig, RemoteGitConfig) where
	getGpgEncParamsBase (_c,gc) = map Param (remoteAnnexGnupgOptions gc)
	getGpgEncParams (c,gc) = getGpgEncParamsBase (c,gc) ++
 		{- When the remote is configured to use public-key encryption,
		 - look up the recipient keys and add them to the option list. -}
		case getRemoteConfigValue encryptionField c of
			Just PubKeyEncryption -> 
				Gpg.pkEncTo $ maybe [] (splitc ',') $
					getRemoteConfigValue cipherkeysField c
			Just SharedPubKeyEncryption ->
				Gpg.pkEncTo $ maybe [] (splitc ',') $
					getRemoteConfigValue pubkeysField c
			_ -> []
	getGpgDecParams (_c,gc) = map Param (remoteAnnexGnupgDecryptOptions gc)
	statelessOpenPGPCommand (c,gc) = case remoteAnnexSharedSOPCommand gc of
		Nothing -> Nothing
		Just sopcmd ->
			{- So far stateless OpenPGP is only supported
			 - for SharedEncryption, not other encryption
			 - methods that involve public keys. -}
			case getRemoteConfigValue encryptionField c of
				Just SharedEncryption -> Just sopcmd
				_ -> Nothing
	statelessOpenPGPProfile (_c,gc) = remoteAnnexSharedSOPProfile gc

{- Extract the GnuPG options from a Remote. -}
instance LensEncParams (RemoteA a) where
	getGpgEncParamsBase r = getGpgEncParamsBase (config r, gitconfig r)
	getGpgEncParams r = getGpgEncParams (config r, gitconfig r)
	getGpgDecParams r = getGpgDecParams (config r, gitconfig r)
	statelessOpenPGPCommand r = statelessOpenPGPCommand (config r, gitconfig r)
	statelessOpenPGPProfile r  = statelessOpenPGPProfile (config r, gitconfig r)
