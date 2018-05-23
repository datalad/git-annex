{- git-annex hashing backends
 -
 - Copyright 2011-2018 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Backend.Hash (
	backends,
	testKeyBackend,
) where

import Annex.Common
import qualified Annex
import Types.Key
import Types.Backend
import Types.KeySource
import Utility.Hash
import Utility.ExternalSHA

import qualified BuildInfo
import qualified Data.ByteString.Lazy as L
import Data.Char

data Hash
	= MD5Hash
	| SHA1Hash
	| SHA2Hash HashSize
	| SHA3Hash HashSize
	| SkeinHash HashSize
#if MIN_VERSION_cryptonite(0,23,0)
	| Blake2bHash HashSize
	| Blake2sHash HashSize
	| Blake2spHash HashSize
#endif

{- Order is slightly significant; want SHA256 first, and more general
 - sizes earlier. -}
hashes :: [Hash]
hashes = concat 
	[ map (SHA2Hash . HashSize) [256, 512, 224, 384]
	, map (SHA3Hash . HashSize) [256, 512, 224, 384]
	, map (SkeinHash . HashSize) [256, 512]
#if MIN_VERSION_cryptonite(0,23,0)
	, map (Blake2bHash . HashSize) [256, 512, 160, 224, 384]
	, map (Blake2sHash . HashSize) [256, 160, 224]
	, map (Blake2spHash . HashSize) [256, 224]
#endif
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
hashKeyVariety MD5Hash = MD5Key
hashKeyVariety SHA1Hash = SHA1Key
hashKeyVariety (SHA2Hash size) = SHA2Key size
hashKeyVariety (SHA3Hash size) = SHA3Key size
hashKeyVariety (SkeinHash size) = SKEINKey size
#if MIN_VERSION_cryptonite(0,23,0)
hashKeyVariety (Blake2bHash size) = Blake2bKey size
hashKeyVariety (Blake2sHash size) = Blake2sKey size
hashKeyVariety (Blake2spHash size) = Blake2spKey size
#endif

{- A key is a hash of its contents. -}
keyValue :: Hash -> KeySource -> Annex (Maybe Key)
keyValue hash source = do
	let file = contentLocation source
	filesize <- liftIO $ getFileSize file
	s <- hashFile hash file filesize
	return $ Just $ stubKey
		{ keyName = s
		, keyVariety = hashKeyVariety hash (HasExt False)
		, keySize = Just filesize
		}

{- Extension preserving keys. -}
keyValueE :: Hash -> KeySource -> Annex (Maybe Key)
keyValueE hash source = keyValue hash source >>= maybe (return Nothing) addE
  where
	addE k = return $ Just $ k
		{ keyName = keyName k ++ selectExtension (keyFilename source)
		, keyVariety = hashKeyVariety hash (HasExt True)
		}

selectExtension :: FilePath -> String
selectExtension f
	| null es = ""
	| otherwise = intercalate "." ("":es)
  where
	es = filter (not . null) $ reverse $
		take 2 $ filter (all validInExtension) $
		takeWhile shortenough $
		reverse $ splitc '.' $ takeExtensions f
	shortenough e = length e <= 4 -- long enough for "jpeg"

{- A key's checksum is checked during fsck. -}
checkKeyChecksum :: Hash -> Key -> FilePath -> Annex Bool
checkKeyChecksum hash key file = catchIOErrorType HardwareFault hwfault $ do
	fast <- Annex.getState Annex.fast
	mstat <- liftIO $ catchMaybeIO $ getFileStatus file
	case (mstat, fast) of
		(Just stat, False) -> do
			filesize <- liftIO $ getFileSize' file stat
			showAction "checksum"
			check <$> hashFile hash file filesize
		_ -> return True
  where
	expected = keyHash key
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

keyHash :: Key -> String
keyHash key = dropExtensions (keyName key)

validInExtension :: Char -> Bool
validInExtension c
	| isAlphaNum c = True
	| c == '.' = True
	| otherwise = False

{- Upgrade keys that have the \ prefix on their hash due to a bug, or
 - that contain non-alphanumeric characters in their extension.
 -
 - Also, for a while migrate from eg SHA256E to SHA256 resulted in a SHA256
 - key that contained an extension inside its keyName. Upgrade those
 - keys, removing the extension.
 -}
needsUpgrade :: Key -> Bool
needsUpgrade key = or
	[ "\\" `isPrefixOf` keyHash key
	, any (not . validInExtension) (takeExtensions $ keyName key)
	, not (hasExt (keyVariety key)) && keyHash key /= keyName key
	]

trivialMigrate :: Key -> Backend -> AssociatedFile -> Maybe Key
trivialMigrate oldkey newbackend afile
	{- Fast migration from hashE to hash backend. -}
	| migratable && hasExt oldvariety = Just $ oldkey
		{ keyName = keyHash oldkey
		, keyVariety = newvariety
		}
	{- Fast migration from hash to hashE backend. -}
	| migratable && hasExt newvariety = case afile of
		AssociatedFile Nothing -> Nothing
		AssociatedFile (Just file) -> Just $ oldkey
			{ keyName = keyHash oldkey ++ selectExtension file
			, keyVariety = newvariety
			}
	{- Upgrade to fix bad previous migration that created a
	 - non-extension preserving key, with an extension
	 - in its keyName. -}
	| newvariety == oldvariety && not (hasExt oldvariety) &&
		keyHash oldkey /= keyName oldkey = Just $ oldkey
			{ keyName = keyHash oldkey
			}
	| otherwise = Nothing
  where
	migratable = oldvariety /= newvariety 
		&& sameExceptExt oldvariety newvariety
	oldvariety = keyVariety oldkey
	newvariety = backendVariety newbackend

hashFile :: Hash -> FilePath -> Integer -> Annex String
hashFile hash file filesize = go hash
  where
	go MD5Hash = use md5Hasher
	go SHA1Hash = usehasher (HashSize 1)
	go (SHA2Hash hashsize) = usehasher hashsize
	go (SHA3Hash hashsize) = use (sha3Hasher hashsize)
	go (SkeinHash hashsize) = use (skeinHasher hashsize)
#if MIN_VERSION_cryptonite(0,23,0)
	go (Blake2bHash hashsize) = use (blake2bHasher hashsize)
	go (Blake2sHash hashsize) = use (blake2sHasher hashsize)
	go (Blake2spHash hashsize) = use (blake2spHasher hashsize)
#endif
	
	use hasher = liftIO $ do
		h <- hasher <$> L.readFile file
		-- Force full evaluation so file is read and closed.
		return (length h `seq` h)
	
	usehasher hashsize@(HashSize sz) = case shaHasher hashsize filesize of
		Left sha -> use sha
		Right (external, internal) ->
			liftIO (externalSHA external sz file) >>= \case
				Right r -> return r
				Left e -> do
					warning e
					-- fall back to internal since
					-- external command failed
					use internal

shaHasher :: HashSize -> Integer -> Either (L.ByteString -> String) (String, L.ByteString -> String)
shaHasher (HashSize hashsize) filesize
	| hashsize == 1 = use BuildInfo.sha1 sha1
	| hashsize == 256 = use BuildInfo.sha256 sha2_256
	| hashsize == 224 = use BuildInfo.sha224 sha2_224
	| hashsize == 384 = use BuildInfo.sha384 sha2_384
	| hashsize == 512 = use BuildInfo.sha512 sha2_512
	| otherwise = error $ "unsupported SHA size " ++ show hashsize
  where
	use Nothing hasher = Left $ usehasher hasher
	use (Just c) hasher
		{- Use builtin, but slightly slower hashing for
		 - smallish files. Cryptohash benchmarks 90 to 101%
		 - faster than external hashers, depending on the hash
		 - and system. So there is no point forking an external
		 - process unless the file is large. -}
		| filesize < 1048576 = Left $ usehasher hasher
		| otherwise = Right (c, usehasher hasher)
	usehasher hasher = show . hasher

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

#if MIN_VERSION_cryptonite(0,23,0)
blake2bHasher :: HashSize -> (L.ByteString -> String)
blake2bHasher (HashSize hashsize)
	| hashsize == 256 = show . blake2b_256
	| hashsize == 512 = show . blake2b_512
	| hashsize == 160 = show . blake2b_160
	| hashsize == 224 = show . blake2b_224
	| hashsize == 384 = show . blake2b_384
	| otherwise = error $ "unsupported BLAKE2B size " ++ show hashsize

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
#endif

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
	in b { getKey = (fmap addE) <$$> getKey b } 
  where
	addE k = k { keyName = keyName k ++ longext }
	longext = ".this-is-a-test-key"
