{- git-annex hashing backends
 -
 - Copyright 2011-2020 Joey Hess <id@joeyh.name>
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
import Types.Key
import Types.Backend
import Types.KeySource
import Utility.Hash
import Utility.Metered

import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import qualified System.FilePath.ByteString as P
import Data.Char
import Data.Word
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
	, getKey = keyValue hash
	, verifyKeyContent = Just $ checkKeyChecksum hash
	, canUpgradeKey = Just needsUpgrade
	, fastMigrate = Just trivialMigrate
	, isStableKey = const True
	}

genBackendE :: Hash -> Backend
genBackendE hash = (genBackend hash)
	{ backendVariety = hashKeyVariety hash (HasExt True)
	, getKey = keyValueE hash
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
keyValue :: Hash -> KeySource -> MeterUpdate -> Annex (Maybe Key)
keyValue hash source meterupdate = do
	let file = fromRawFilePath (contentLocation source)
	filesize <- liftIO $ getFileSize file
	s <- hashFile hash file meterupdate
	return $ Just $ mkKey $ \k -> k
		{ keyName = encodeBS s
		, keyVariety = hashKeyVariety hash (HasExt False)
		, keySize = Just filesize
		}

{- Extension preserving keys. -}
keyValueE :: Hash -> KeySource -> MeterUpdate -> Annex (Maybe Key)
keyValueE hash source meterupdate =
	keyValue hash source meterupdate >>= maybe (return Nothing) addE
  where
	addE k = do
		maxlen <- annexMaxExtensionLength <$> Annex.getGitConfig
		let ext = selectExtension maxlen (keyFilename source)
		return $ Just $ alterKey k $ \d -> d
			{ keyName = keyName d <> ext
			, keyVariety = hashKeyVariety hash (HasExt True)
			}

selectExtension :: Maybe Int -> RawFilePath -> S.ByteString
selectExtension maxlen f
	| null es = ""
	| otherwise = S.intercalate "." ("":es)
  where
	es = filter (not . S.null) $ reverse $
		take 2 $ filter (S.all validInExtension) $
		takeWhile shortenough $
		reverse $ S.split (fromIntegral (ord '.')) (P.takeExtensions f)
	shortenough e = S.length e <= fromMaybe maxExtensionLen maxlen

maxExtensionLen :: Int
maxExtensionLen = 4 -- long enough for "jpeg"

{- A key's checksum is checked during fsck when it's content is present
 - except for in fast mode. -}
checkKeyChecksum :: Hash -> Key -> FilePath -> Annex Bool
checkKeyChecksum hash key file = catchIOErrorType HardwareFault hwfault $ do
	fast <- Annex.getState Annex.fast
	exists <- liftIO $ doesFileExist file
	case (exists, fast) of
		(True, False) -> do
			showAction "checksum"
			check <$> hashFile hash file nullMeterUpdate
		_ -> return True
  where
	expected = decodeBS (keyHash key)
	check s
		| s == expected = True
		{- A bug caused checksums to be prefixed with \ in some
		 - cases; still accept these as legal now that the bug has been
		 - fixed. -}
		| '\\' : s == expected = True
		| otherwise = False

	hwfault e = do
		warning $ "hardware fault: " ++ show e
		return False

keyHash :: Key -> S.ByteString
keyHash = fst . splitKeyNameExtension

validInExtension :: Word8 -> Bool
validInExtension c
	| isAlphaNum (chr (fromIntegral c)) = True
	| c <= 127 = False -- other ascii, spaces, punctuation, control chars
	| otherwise = True -- utf8 is allowed, also other encodings

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
	, not (hasExt (fromKey keyVariety key)) && keyHash key /= fromKey keyName key
	]

trivialMigrate :: Key -> Backend -> AssociatedFile -> Annex (Maybe Key)
trivialMigrate oldkey newbackend afile = trivialMigrate' oldkey newbackend afile
	<$> (annexMaxExtensionLength <$> Annex.getGitConfig)

trivialMigrate' :: Key -> Backend -> AssociatedFile -> Maybe Int -> Maybe Key
trivialMigrate' oldkey newbackend afile maxextlen
	{- Fast migration from hashE to hash backend. -}
	| migratable && hasExt oldvariety = Just $ alterKey oldkey $ \d -> d
		{ keyName = keyHash oldkey
		, keyVariety = newvariety
		}
	{- Fast migration from hash to hashE backend. -}
	| migratable && hasExt newvariety = case afile of
		AssociatedFile Nothing -> Nothing
		AssociatedFile (Just file) -> Just $ alterKey oldkey $ \d -> d
			{ keyName = keyHash oldkey 
				<> selectExtension maxextlen file
			, keyVariety = newvariety
			}
	{- Upgrade to fix bad previous migration that created a
	 - non-extension preserving key, with an extension
	 - in its keyName. -}
	| newvariety == oldvariety && not (hasExt oldvariety) &&
		keyHash oldkey /= fromKey keyName oldkey = 
			Just $ alterKey oldkey $ \d -> d
				{ keyName = keyHash oldkey
				}
	| otherwise = Nothing
  where
	migratable = oldvariety /= newvariety 
		&& sameExceptExt oldvariety newvariety
	oldvariety = fromKey keyVariety oldkey
	newvariety = backendVariety newbackend

hashFile :: Hash -> FilePath -> MeterUpdate -> Annex String
hashFile hash file meterupdate = 
	liftIO $ withMeteredFile file meterupdate $ \b -> do
		let h = hasher b
		-- Force full evaluation of hash so whole file is read
		-- before returning.
		evaluate (rnf h)
		return h
  where
	hasher = case hash of
		MD5Hash -> md5Hasher
		SHA1Hash -> sha1Hasher
		SHA2Hash hashsize -> sha2Hasher hashsize
		SHA3Hash hashsize -> sha3Hasher hashsize
		SkeinHash hashsize -> skeinHasher hashsize
		Blake2bHash hashsize -> blake2bHasher hashsize
		Blake2bpHash hashsize -> blake2bpHasher hashsize
		Blake2sHash hashsize -> blake2sHasher hashsize
		Blake2spHash hashsize -> blake2spHasher hashsize

sha2Hasher :: HashSize -> (L.ByteString -> String)
sha2Hasher (HashSize hashsize)
	| hashsize == 256 = use sha2_256
	| hashsize == 224 = use sha2_224
	| hashsize == 384 = use sha2_384
	| hashsize == 512 = use sha2_512
	| otherwise = error $ "unsupported SHA size " ++ show hashsize
  where
	use hasher = show . hasher

sha3Hasher :: HashSize -> (L.ByteString -> String)
sha3Hasher (HashSize hashsize)
	| hashsize == 256 = show . sha3_256
	| hashsize == 224 = show . sha3_224
	| hashsize == 384 = show . sha3_384
	| hashsize == 512 = show . sha3_512
	| otherwise = error $ "unsupported SHA3 size " ++ show hashsize

skeinHasher :: HashSize -> (L.ByteString -> String)
skeinHasher (HashSize hashsize)
	| hashsize == 256 = show . skein256
	| hashsize == 512 = show . skein512
	| otherwise = error $ "unsupported SKEIN size " ++ show hashsize

blake2bHasher :: HashSize -> (L.ByteString -> String)
blake2bHasher (HashSize hashsize)
	| hashsize == 256 = show . blake2b_256
	| hashsize == 512 = show . blake2b_512
	| hashsize == 160 = show . blake2b_160
	| hashsize == 224 = show . blake2b_224
	| hashsize == 384 = show . blake2b_384
	| otherwise = error $ "unsupported BLAKE2B size " ++ show hashsize

blake2bpHasher :: HashSize -> (L.ByteString -> String)
blake2bpHasher (HashSize hashsize)
	| hashsize == 512 = show . blake2bp_512
	| otherwise = error $ "unsupported BLAKE2BP size " ++ show hashsize

blake2sHasher :: HashSize -> (L.ByteString -> String)
blake2sHasher (HashSize hashsize)
	| hashsize == 256 = show . blake2s_256
	| hashsize == 160 = show . blake2s_160
	| hashsize == 224 = show . blake2s_224
	| otherwise = error $ "unsupported BLAKE2S size " ++ show hashsize

blake2spHasher :: HashSize -> (L.ByteString -> String)
blake2spHasher (HashSize hashsize)
	| hashsize == 256 = show . blake2sp_256
	| hashsize == 224 = show . blake2sp_224
	| otherwise = error $ "unsupported BLAKE2SP size " ++ show hashsize

sha1Hasher :: L.ByteString -> String
sha1Hasher = show . sha1

md5Hasher :: L.ByteString -> String
md5Hasher = show . md5

{- A varient of the SHA256E backend, for testing that needs special keys
 - that cannot collide with legitimate keys in the repository.
 -
 - This is accomplished by appending a special extension to the key,
 - that is not one that selectExtension would select (due to being too
 - long).
 -}
testKeyBackend :: Backend
testKeyBackend = 
	let b = genBackendE (SHA2Hash (HashSize 256))
	in b { getKey = \ks p -> (fmap addE) <$> getKey b ks p } 
  where
	addE k = alterKey k $ \d -> d
		{ keyName = keyName d <> longext
		}
	longext = ".this-is-a-test-key"
