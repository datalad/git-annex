{- common functions for encryptable remotes
 -
 - Copyright 2011-2020 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE FlexibleContexts, ScopedTypeVariables #-}

module Remote.Helper.Encryptable (
	EncryptionIsSetup,
	encryptionSetup,
	noEncryptionUsed,
	encryptionAlreadySetup,
	encryptionConfigParsers,
	parseEncryptionConfig,
	remoteCipher,
	remoteCipher',
	embedCreds,
	cipherKey,
	extractCipher,
	isEncrypted,
	describeEncryption,
	encryptionField,
	highRandomQualityField
) where

import qualified Data.Map as M
import qualified Data.Set as S
import qualified "sandi" Codec.Binary.Base64 as B64
import qualified Data.ByteString as B

import Annex.Common
import Types.Remote
import Crypto
import Types.Crypto
import Types.ProposedAccepted
import qualified Annex
import Annex.SpecialRemote.Config

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

encryptionConfigParsers :: [RemoteConfigFieldParser]
encryptionConfigParsers =
	[ encryptionFieldParser
	, optionalStringParser cipherField HiddenField
	, optionalStringParser cipherkeysField HiddenField
	, optionalStringParser pubkeysField HiddenField
	, yesNoParser embedCredsField False
		(FieldDesc "embed credentials into git repository")
	, macFieldParser
	, optionalStringParser (Accepted "keyid")
		(FieldDesc "gpg key id")
	, optionalStringParser (Accepted "keyid+")
		(FieldDesc "add additional gpg key")
	, optionalStringParser (Accepted "keyid-")
		(FieldDesc "remove gpg key")
	, highRandomQualityFieldParser
	]

encryptionConfigs :: S.Set RemoteConfigField
encryptionConfigs = S.fromList (map parserForField encryptionConfigParsers)

-- Parse only encryption fields, ignoring all others.
parseEncryptionConfig :: RemoteConfig -> Either String ParsedRemoteConfig
parseEncryptionConfig c = parseRemoteConfig
	(M.restrictKeys c encryptionConfigs)
	(RemoteConfigParser encryptionConfigParsers Nothing)

encryptionFieldParser :: RemoteConfigFieldParser
encryptionFieldParser = RemoteConfigFieldParser
	{ parserForField = encryptionField
	, valueParser = \v c -> Just . RemoteConfigValue
		<$> parseEncryptionMethod (fmap fromProposedAccepted v) c
	, fieldDesc = FieldDesc "how to encrypt data stored in the special remote"
	, valueDesc = Just $ ValueDesc $
		intercalate " or " (M.keys encryptionMethods)
	}

encryptionMethods :: M.Map String EncryptionMethod
encryptionMethods = M.fromList
	[ ("none", NoneEncryption)
	, ("shared", SharedEncryption)
	, ("hybrid", HybridEncryption)
	, ("pubkey", PubKeyEncryption)
	, ("sharedpubkey", SharedPubKeyEncryption)
	]

parseEncryptionMethod :: Maybe String -> RemoteConfig -> Either String EncryptionMethod
parseEncryptionMethod (Just s) _ = case M.lookup s encryptionMethods of
	Just em -> Right em
	Nothing -> Left badEncryptionMethod
-- Hybrid encryption is the default when a keyid is specified without
-- an encryption field, or when there's a cipher already but no encryption
-- field.
parseEncryptionMethod Nothing c
	| M.member (Accepted "keyid") c || M.member cipherField c = Right HybridEncryption
	| otherwise = Left badEncryptionMethod

badEncryptionMethod :: String
badEncryptionMethod = "Specify " ++ intercalate " or "
	(map ((fromProposedAccepted encryptionField ++ "=") ++)
		(M.keys encryptionMethods))
	++ "."

highRandomQualityField :: RemoteConfigField
highRandomQualityField = Accepted "highRandomQuality"

highRandomQualityFieldParser :: RemoteConfigFieldParser
highRandomQualityFieldParser = RemoteConfigFieldParser
	{ parserForField = highRandomQualityField
	, valueParser = \v _c -> Just . RemoteConfigValue
		<$> parseHighRandomQuality (fmap fromProposedAccepted v)
	, fieldDesc = HiddenField
	, valueDesc = Nothing
	}
 
parseHighRandomQuality :: Maybe String -> Either String Bool
parseHighRandomQuality Nothing = Right True
parseHighRandomQuality (Just "false") = Right False
parseHighRandomQuality (Just "true") = Right True
parseHighRandomQuality _ = Left "expected highRandomQuality=true/false"
	
macFieldParser :: RemoteConfigFieldParser
macFieldParser = RemoteConfigFieldParser
	{ parserForField = macField
	, valueParser = \v _c -> Just . RemoteConfigValue <$> parseMac v
	, fieldDesc = FieldDesc "how to encrypt filenames used on the remote"
	, valueDesc = Just $ ValueDesc $
		intercalate " or " (M.keys macMap)
	}

parseMac :: Maybe (ProposedAccepted String) -> Either String Mac
parseMac Nothing = Right defaultMac
parseMac (Just (Accepted s)) = Right $ fromMaybe defaultMac (readMac s)
parseMac (Just (Proposed s)) = case readMac s of
	Just mac -> Right mac
	Nothing -> Left "bad mac value"

{- Encryption setup for a remote. The user must specify whether to use
 - an encryption key, or not encrypt. An encrypted cipher is created, or is
 - updated to be accessible to an additional encryption key. Or the user
 - could opt to use a shared cipher, which is stored unencrypted. -}
encryptionSetup :: RemoteConfig -> RemoteGitConfig -> Annex (RemoteConfig, EncryptionIsSetup)
encryptionSetup c gc = do
	pc <- either giveup return $ parseEncryptionConfig c
	cmd <- gpgCmd <$> Annex.getGitConfig
	maybe (genCipher pc cmd) (updateCipher pc cmd) (extractCipher pc)
  where
	-- The type of encryption
	encryption = parseEncryptionMethod (fromProposedAccepted <$> M.lookup encryptionField c) c
	-- Generate a new cipher, depending on the chosen encryption scheme
	genCipher pc cmd = case encryption of
		Right NoneEncryption -> return (c, NoEncryption)
		Right SharedEncryption -> encsetup $ genSharedCipher cmd
		Right HybridEncryption -> encsetup $ genEncryptedCipher cmd (pc, gc) key Hybrid
		Right PubKeyEncryption -> encsetup $ genEncryptedCipher cmd (pc, gc) key PubKey
		Right SharedPubKeyEncryption -> encsetup $ genSharedPubKeyCipher cmd key
		Left err -> giveup err
	key = maybe (giveup "Specify keyid=...") fromProposedAccepted $
		M.lookup (Accepted "keyid") c
	newkeys = maybe [] (\k -> [(True,fromProposedAccepted k)]) (M.lookup (Accepted "keyid+") c) ++
		maybe [] (\k -> [(False,fromProposedAccepted k)]) (M.lookup (Accepted "keyid-") c)
	cannotchange = giveup "Cannot set encryption type of existing remotes."
	-- Update an existing cipher if possible.
	updateCipher pc cmd v = case v of
		SharedCipher _ | encryption == Right SharedEncryption ->
			return (c', EncryptionIsSetup)
		EncryptedCipher _ variant _ | sameasencryption variant ->
			use "encryption update" $ updateCipherKeyIds cmd (pc, gc) newkeys v
		SharedPubKeyCipher _ _ ->
			use "encryption update" $ updateCipherKeyIds cmd (pc, gc) newkeys v
		_ -> cannotchange
	sameasencryption variant = case encryption of
		Right HybridEncryption -> variant == Hybrid
		Right PubKeyEncryption -> variant == PubKey
		Right _ -> False
		Left _ -> True
	encsetup a = use "encryption setup" . a =<< highRandomQuality
	use m a = do
		showNote m
		cipher <- liftIO a
		showNote (describeCipher cipher)
		return (storeCipher cipher c', EncryptionIsSetup)
	highRandomQuality = ifM (Annex.getState Annex.fast)
		( return False
		, case parseHighRandomQuality (fromProposedAccepted <$> M.lookup highRandomQualityField c) of
			Left err -> giveup err
			Right v -> return v
		)
	c' = foldr M.delete c
		-- Remove configs that are only used in here to generate
		-- the encryption keys, and should not be stored in
		-- remote.log.
		-- Older versions used to remove 'encryption' as well, since
		-- it was redundant; we now need to keep it for
		-- public-key encryption, hence we leave it on newer
		-- remotes (while being backward-compatible).
		(map Accepted ["keyid", "keyid+", "keyid-", "highRandomQuality"])

remoteCipher :: ParsedRemoteConfig -> RemoteGitConfig -> Annex (Maybe Cipher)
remoteCipher c gc = fmap fst <$> remoteCipher' c gc

{- Gets encryption Cipher. The decrypted Ciphers are cached in the Annex
 - state. -}
remoteCipher' :: ParsedRemoteConfig -> RemoteGitConfig -> Annex (Maybe (Cipher, StorableCipher))
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
embedCreds :: ParsedRemoteConfig -> Bool
embedCreds c = case getRemoteConfigValue embedCredsField c of
	Just v -> v
	Nothing -> case (getRemoteConfigValue cipherkeysField c, getRemoteConfigValue cipherField c) of
		(Just (_ :: ProposedAccepted String), Just (_ :: ProposedAccepted String)) -> True
		_ -> False

{- Gets encryption Cipher, and key encryptor. -}
cipherKey :: ParsedRemoteConfig -> RemoteGitConfig -> Annex (Maybe (Cipher, EncKey))
cipherKey c gc = fmap make <$> remoteCipher c gc
  where
	make ciphertext = (ciphertext, encryptKey mac ciphertext)
	mac = fromMaybe defaultMac $ getRemoteConfigValue macField c

{- Stores an StorableCipher in a remote's configuration. -}
storeCipher :: StorableCipher -> RemoteConfig -> RemoteConfig
storeCipher cip = case cip of
	(SharedCipher t) -> addcipher t
	(EncryptedCipher t _ ks) -> addcipher t . storekeys ks cipherkeysField
	(SharedPubKeyCipher t ks) -> addcipher t . storekeys ks pubkeysField
  where
	addcipher t = M.insert cipherField (Accepted (toB64bs t))
	storekeys (KeyIds l) n = M.insert n (Accepted (intercalate "," l))

{- Extracts an StorableCipher from a remote's configuration. -}
extractCipher :: ParsedRemoteConfig -> Maybe StorableCipher
extractCipher c = case (getRemoteConfigValue cipherField c,
			(getRemoteConfigValue cipherkeysField c <|> getRemoteConfigValue pubkeysField c),
			getRemoteConfigValue encryptionField c) of
	(Just t, Just ks, Just HybridEncryption) ->
		Just $ EncryptedCipher (fromB64bs t) Hybrid (readkeys ks)
	(Just t, Just ks, Just PubKeyEncryption) ->
		Just $ EncryptedCipher (fromB64bs t) PubKey (readkeys ks)
	(Just t, Just ks, Just SharedPubKeyEncryption) ->
		Just $ SharedPubKeyCipher (fromB64bs t) (readkeys ks)
	(Just t, Nothing, Just SharedEncryption) ->
		Just $ SharedCipher (fromB64bs t)
	_ -> Nothing
  where
	readkeys = KeyIds . splitc ','

isEncrypted :: ParsedRemoteConfig -> Bool
isEncrypted = isJust . extractCipher

describeEncryption :: ParsedRemoteConfig -> String
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
