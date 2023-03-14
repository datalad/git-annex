{- git-annex hashing backends
 -
 - Copyright 2011-2021 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Backend.Hash (
	backends,
	testKeyBackend,
	keyHash,
) where

import Annex.Common
import qualified Annex
import Backend.Utilities
import Types.Key
import Types.Backend
import Types.KeySource
import Utility.Hash
import Utility.Metered
import qualified Utility.RawFilePath as R

import qualified Data.ByteString as S
import qualified Data.ByteString.Short as S (toShort, fromShort)
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import Control.DeepSeq
import Control.Exception (evaluate)

data Hash
	= MD5Hash
	| SHA1Hash
	| SHA2Hash HashSize
	| SHA3Hash HashSize
	| SkeinHash HashSize
	| Blake2bHash HashSize
	| Blake2bpHash HashSize
	| Blake2sHash HashSize
	| Blake2spHash HashSize

cryptographicallySecure :: Hash -> Bool
cryptographicallySecure (SHA2Hash _) = True
cryptographicallySecure (SHA3Hash _) = True
cryptographicallySecure (SkeinHash _) = True
cryptographicallySecure (Blake2bHash _) = True
cryptographicallySecure (Blake2bpHash _) = True
cryptographicallySecure (Blake2sHash _) = True
cryptographicallySecure (Blake2spHash _) = True
cryptographicallySecure SHA1Hash = False
cryptographicallySecure MD5Hash = False

{- Order is slightly significant; want SHA256 first, and more general
 - sizes earlier. -}
hashes :: [Hash]
hashes = concat 
	[ map (SHA2Hash . HashSize) [256, 512, 224, 384]
	, map (SHA3Hash . HashSize) [256, 512, 224, 384]
	, map (SkeinHash . HashSize) [256, 512]
	, map (Blake2bHash . HashSize) [256, 512, 160, 224, 384]
	, map (Blake2bpHash . HashSize) [512]
	, map (Blake2sHash . HashSize) [256, 160, 224]
	, map (Blake2spHash . HashSize) [256, 224]
	, [SHA1Hash]
	, [MD5Hash]
	]

{- The SHA256E backend is the default, so genBackendE comes first. -}
backends :: [Backend]
backends = concatMap (\h -> [genBackendE h, genBackend h]) hashes

genBackend :: Hash -> Backend
genBackend hash = Backend
	{ backendVariety = hashKeyVariety hash (HasExt False)
	, genKey = Just (keyValue hash)
	, verifyKeyContent = Just $ checkKeyChecksum hash
	, verifyKeyContentIncrementally = Just $ checkKeyChecksumIncremental hash
	, canUpgradeKey = Just needsUpgrade
	, fastMigrate = Just trivialMigrate
	, isStableKey = const True
	, isCryptographicallySecure = const (cryptographicallySecure hash)
	}

genBackendE :: Hash -> Backend
genBackendE hash = (genBackend hash)
	{ backendVariety = hashKeyVariety hash (HasExt True)
	, genKey = Just (keyValueE hash)
	}

hashKeyVariety :: Hash -> HasExt -> KeyVariety
hashKeyVariety MD5Hash he = MD5Key he
hashKeyVariety SHA1Hash he = SHA1Key he
hashKeyVariety (SHA2Hash size) he = SHA2Key size he
hashKeyVariety (SHA3Hash size) he = SHA3Key size he
hashKeyVariety (SkeinHash size) he = SKEINKey size he
hashKeyVariety (Blake2bHash size) he = Blake2bKey size he
hashKeyVariety (Blake2bpHash size) he = Blake2bpKey size he
hashKeyVariety (Blake2sHash size) he = Blake2sKey size he
hashKeyVariety (Blake2spHash size) he = Blake2spKey size he

{- A key is a hash of its contents. -}
keyValue :: Hash -> KeySource -> MeterUpdate -> Annex Key
keyValue hash source meterupdate = do
	let file = contentLocation source
	filesize <- liftIO $ getFileSize file
	s <- hashFile hash file meterupdate
	return $ mkKey $ \k -> k
		{ keyName = S.toShort (encodeBS s)
		, keyVariety = hashKeyVariety hash (HasExt False)
		, keySize = Just filesize
		}

{- Extension preserving keys. -}
keyValueE :: Hash -> KeySource -> MeterUpdate -> Annex Key
keyValueE hash source meterupdate =
	keyValue hash source meterupdate
		>>= addE source (const $ hashKeyVariety hash (HasExt True))

checkKeyChecksum :: Hash -> Key -> RawFilePath -> Annex Bool
checkKeyChecksum hash key file = catchIOErrorType HardwareFault hwfault $ do
	fast <- Annex.getRead Annex.fast
	exists <- liftIO $ R.doesPathExist file
	case (exists, fast) of
		(True, False) -> do
			showAction descChecksum
			sameCheckSum key 
				<$> hashFile hash file nullMeterUpdate
		_ -> return True
  where
	hwfault e = do
		warning $ "hardware fault: " ++ show e
		return False

sameCheckSum :: Key -> String -> Bool
sameCheckSum key s
	| s == expected = True
	{- A bug caused checksums to be prefixed with \ in some
	 - cases; still accept these as legal now that the bug
	 - has been fixed. -}
	| '\\' : s == expected = True
	| otherwise = False
  where
	expected = decodeBS (keyHash key)

checkKeyChecksumIncremental :: Hash -> Key -> Annex IncrementalVerifier
checkKeyChecksumIncremental hash key = liftIO $ (snd $ hasher hash) key

keyHash :: Key -> S.ByteString
keyHash = fst . splitKeyNameExtension

{- Upgrade keys that have the \ prefix on their hash due to a bug, or
 - that contain non-alphanumeric characters in their extension.
 -
 - Also, for a while migrate from eg SHA256E to SHA256 resulted in a SHA256
 - key that contained an extension inside its keyName. Upgrade those
 - keys, removing the extension.
 -}
needsUpgrade :: Key -> Bool
needsUpgrade key = or
	[ "\\" `S8.isPrefixOf` keyHash key
	, S.any (not . validInExtension) (snd $ splitKeyNameExtension key)
	, not (hasExt (fromKey keyVariety key)) && keyHash key /= S.fromShort (fromKey keyName key)
	]

trivialMigrate :: Key -> Backend -> AssociatedFile -> Annex (Maybe Key)
trivialMigrate oldkey newbackend afile = trivialMigrate' oldkey newbackend afile
	<$> (annexMaxExtensionLength <$> Annex.getGitConfig)

trivialMigrate' :: Key -> Backend -> AssociatedFile -> Maybe Int -> Maybe Key
trivialMigrate' oldkey newbackend afile maxextlen
	{- Fast migration from hashE to hash backend. -}
	| migratable && hasExt oldvariety = Just $ alterKey oldkey $ \d -> d
		{ keyName = S.toShort (keyHash oldkey)
		, keyVariety = newvariety
		}
	{- Fast migration from hash to hashE backend. -}
	| migratable && hasExt newvariety = case afile of
		AssociatedFile Nothing -> Nothing
		AssociatedFile (Just file) -> Just $ alterKey oldkey $ \d -> d
			{ keyName = S.toShort $ keyHash oldkey 
				<> selectExtension maxextlen file
			, keyVariety = newvariety
			}
	{- Upgrade to fix bad previous migration that created a
	 - non-extension preserving key, with an extension
	 - in its keyName. -}
	| newvariety == oldvariety && not (hasExt oldvariety) &&
		keyHash oldkey /= S.fromShort (fromKey keyName oldkey) = 
			Just $ alterKey oldkey $ \d -> d
				{ keyName = S.toShort (keyHash oldkey)
				}
	| otherwise = Nothing
  where
	migratable = oldvariety /= newvariety 
		&& sameExceptExt oldvariety newvariety
	oldvariety = fromKey keyVariety oldkey
	newvariety = backendVariety newbackend

hashFile :: Hash -> RawFilePath -> MeterUpdate -> Annex String
hashFile hash file meterupdate = 
	liftIO $ withMeteredFile (fromRawFilePath file) meterupdate $ \b -> do
		let h = (fst $ hasher hash) b
		-- Force full evaluation of hash so whole file is read
		-- before returning.
		evaluate (rnf h)
		return h

type Hasher = (L.ByteString -> String, Key -> IO IncrementalVerifier)

hasher :: Hash -> Hasher
hasher MD5Hash = md5Hasher
hasher SHA1Hash = sha1Hasher
hasher (SHA2Hash hashsize) = sha2Hasher hashsize
hasher (SHA3Hash hashsize) = sha3Hasher hashsize
hasher (SkeinHash hashsize) = skeinHasher hashsize
hasher (Blake2bHash hashsize) = blake2bHasher hashsize
hasher (Blake2bpHash hashsize) = blake2bpHasher hashsize
hasher (Blake2sHash hashsize) = blake2sHasher hashsize
hasher (Blake2spHash hashsize) = blake2spHasher hashsize

mkHasher :: HashAlgorithm h => (L.ByteString -> Digest h) -> Context h -> Hasher
mkHasher h c = (show . h, mkIncrementalVerifier c descChecksum . sameCheckSum)

sha2Hasher :: HashSize -> Hasher
sha2Hasher (HashSize hashsize)
	| hashsize == 256 = mkHasher sha2_256 sha2_256_context
	| hashsize == 224 = mkHasher sha2_224 sha2_224_context
	| hashsize == 384 = mkHasher sha2_384 sha2_384_context
	| hashsize == 512 = mkHasher sha2_512 sha2_512_context
	| otherwise = error $ "unsupported SHA2 size " ++ show hashsize

sha3Hasher :: HashSize -> Hasher
sha3Hasher (HashSize hashsize)
	| hashsize == 256 = mkHasher sha3_256 sha3_256_context
	| hashsize == 224 = mkHasher sha3_224 sha3_224_context
	| hashsize == 384 = mkHasher sha3_384 sha3_384_context
	| hashsize == 512 = mkHasher sha3_512 sha3_512_context
	| otherwise = error $ "unsupported SHA3 size " ++ show hashsize

skeinHasher :: HashSize -> Hasher
skeinHasher (HashSize hashsize)
	| hashsize == 256 = mkHasher skein256 skein256_context
	| hashsize == 512 = mkHasher skein512 skein512_context
	| otherwise = error $ "unsupported SKEIN size " ++ show hashsize

blake2bHasher :: HashSize -> Hasher
blake2bHasher (HashSize hashsize)
	| hashsize == 256 = mkHasher blake2b_256 blake2b_256_context
	| hashsize == 512 = mkHasher blake2b_512 blake2b_512_context
	| hashsize == 160 = mkHasher blake2b_160 blake2b_160_context
	| hashsize == 224 = mkHasher blake2b_224 blake2b_224_context
	| hashsize == 384 = mkHasher blake2b_384 blake2b_384_context
	| otherwise = error $ "unsupported BLAKE2B size " ++ show hashsize

blake2bpHasher :: HashSize -> Hasher
blake2bpHasher (HashSize hashsize)
	| hashsize == 512 = mkHasher blake2bp_512 blake2bp_512_context
	| otherwise = error $ "unsupported BLAKE2BP size " ++ show hashsize

blake2sHasher :: HashSize -> Hasher
blake2sHasher (HashSize hashsize)
	| hashsize == 256 = mkHasher blake2s_256 blake2s_256_context
	| hashsize == 160 = mkHasher blake2s_160 blake2s_160_context
	| hashsize == 224 = mkHasher blake2s_224 blake2s_224_context
	| otherwise = error $ "unsupported BLAKE2S size " ++ show hashsize

blake2spHasher :: HashSize -> Hasher
blake2spHasher (HashSize hashsize)
	| hashsize == 256 = mkHasher blake2sp_256 blake2sp_256_context
	| hashsize == 224 = mkHasher blake2sp_224 blake2sp_224_context
	| otherwise = error $ "unsupported BLAKE2SP size " ++ show hashsize

sha1Hasher :: Hasher
sha1Hasher = mkHasher sha1 sha1_context

md5Hasher :: Hasher
md5Hasher = mkHasher md5 md5_context

descChecksum :: String
descChecksum = "checksum"

{- A variant of the SHA256E backend, for testing that needs special keys
 - that cannot collide with legitimate keys in the repository.
 -
 - This is accomplished by appending a special extension to the key,
 - that is not one that selectExtension would select (due to being too
 - long).
 -}
testKeyBackend :: Backend
testKeyBackend = 
	let b = genBackendE (SHA2Hash (HashSize 256))
	    gk = case genKey b of
		Nothing -> Nothing
		Just f -> Just (\ks p -> addTestE <$> f ks p)
	in b { genKey = gk }
  where
	addTestE k = alterKey k $ \d -> d
		{ keyName = keyName d <> longext
		}
	longext = ".this-is-a-test-key"
