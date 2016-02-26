{- git-annex hashing backends
 -
 - Copyright 2011-2015 Joey Hess <id@joeyh.name>
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
import Types.Backend
import Types.KeySource
import Utility.Hash
import Utility.ExternalSHA

import qualified Build.SysConfig as SysConfig
import qualified Data.ByteString.Lazy as L
import Data.Char

data Hash
	= MD5Hash
	| SHA1Hash
	| SHA2Hash HashSize
	| SHA3Hash HashSize
	| SkeinHash HashSize
type HashSize = Int

{- Order is slightly significant; want SHA256 first, and more general
 - sizes earlier. -}
hashes :: [Hash]
hashes = concat 
	[ map SHA2Hash [256, 512, 224, 384]
#ifdef WITH_CRYPTONITE
	, map SHA3Hash [256, 512, 224, 384]
#endif
	, map SkeinHash [256, 512]
	, [SHA1Hash]
	, [MD5Hash]
	]

{- The SHA256E backend is the default, so genBackendE comes first. -}
backends :: [Backend]
backends = concatMap (\h -> [genBackendE h, genBackend h]) hashes

genBackend :: Hash -> Backend
genBackend hash = Backend
	{ name = hashName hash
	, getKey = keyValue hash
	, verifyKeyContent = Just $ checkKeyChecksum hash
	, canUpgradeKey = Just needsUpgrade
	, fastMigrate = Just trivialMigrate
	, isStableKey = const True
	}

genBackendE :: Hash -> Backend
genBackendE hash = (genBackend hash)
	{ name = hashNameE hash
	, getKey = keyValueE hash
	}

hashName :: Hash -> String
hashName MD5Hash = "MD5"
hashName SHA1Hash = "SHA1"
hashName (SHA2Hash size) = "SHA" ++ show size
hashName (SHA3Hash size) = "SHA3_" ++ show size
hashName (SkeinHash size) = "SKEIN" ++ show size

hashNameE :: Hash -> String
hashNameE hash = hashName hash ++ "E"

{- A key is a hash of its contents. -}
keyValue :: Hash -> KeySource -> Annex (Maybe Key)
keyValue hash source = do
	let file = contentLocation source
	filesize <- liftIO $ getFileSize file
	s <- hashFile hash file filesize
	return $ Just $ stubKey
		{ keyName = s
		, keyBackendName = hashName hash
		, keySize = Just filesize
		}

{- Extension preserving keys. -}
keyValueE :: Hash -> KeySource -> Annex (Maybe Key)
keyValueE hash source = keyValue hash source >>= maybe (return Nothing) addE
  where
	addE k = return $ Just $ k
		{ keyName = keyName k ++ selectExtension (keyFilename source)
		, keyBackendName = hashNameE hash
		}

selectExtension :: FilePath -> String
selectExtension f
	| null es = ""
	| otherwise = intercalate "." ("":es)
  where
	es = filter (not . null) $ reverse $
		take 2 $ takeWhile shortenough $
		reverse $ split "." $ filter validExtension $ takeExtensions f
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

validExtension :: Char -> Bool
validExtension c
	| isAlphaNum c = True
	| c == '.' = True
	| otherwise = False

{- Upgrade keys that have the \ prefix on their sha due to a bug, or
 - that contain non-alphanumeric characters in their extension. -}
needsUpgrade :: Key -> Bool
needsUpgrade key = "\\" `isPrefixOf` keyHash key ||
	any (not . validExtension) (takeExtensions $ keyName key)

trivialMigrate :: Key -> Backend -> AssociatedFile -> Maybe Key
trivialMigrate oldkey newbackend afile
	{- Fast migration from hashE to hash backend. -}
	| keyBackendName oldkey == name newbackend ++ "E" = Just $ oldkey
		{ keyName = keyHash oldkey
		, keyBackendName = name newbackend
		}
	{- Fast migration from hash to hashE backend. -}
	| keyBackendName oldkey ++"E" == name newbackend = case afile of
		Nothing -> Nothing
		Just file -> Just $ oldkey
			{ keyName = keyHash oldkey ++ selectExtension file
			, keyBackendName = name newbackend
			}
	| otherwise = Nothing

hashFile :: Hash -> FilePath -> Integer -> Annex String
hashFile hash file filesize = go hash
  where
	go MD5Hash = use md5Hasher
	go SHA1Hash = usehasher 1
	go (SHA2Hash hashsize) = usehasher hashsize
	go (SHA3Hash hashsize) = use (sha3Hasher hashsize)
	go (SkeinHash hashsize) = use (skeinHasher hashsize)
	
	use hasher = liftIO $ hasher <$> L.readFile file
	
	usehasher hashsize = case shaHasher hashsize filesize of
		Left sha -> use sha
		Right (external, internal) -> do
			v <- liftIO $ externalSHA external hashsize file
			case v of
				Right r -> return r
				Left e -> do
					warning e
					-- fall back to internal since
					-- external command failed
					use internal

shaHasher :: HashSize -> Integer -> Either (L.ByteString -> String) (String, L.ByteString -> String)
shaHasher hashsize filesize
	| hashsize == 1 = use SysConfig.sha1 sha1
	| hashsize == 256 = use SysConfig.sha256 sha2_256
	| hashsize == 224 = use SysConfig.sha224 sha2_224
	| hashsize == 384 = use SysConfig.sha384 sha2_384
	| hashsize == 512 = use SysConfig.sha512 sha2_512
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
sha3Hasher hashsize
#ifdef WITH_CRYPTONITE
	| hashsize == 256 = show . sha3_256
	| hashsize == 224 = show . sha3_224
	| hashsize == 384 = show . sha3_384
	| hashsize == 512 = show . sha3_512
#endif
	| otherwise = error $ "unsupported SHA3 size " ++ show hashsize

skeinHasher :: HashSize -> (L.ByteString -> String)
skeinHasher hashsize 
	| hashsize == 256 = show . skein256
	| hashsize == 512 = show . skein512
	| otherwise = error $ "unsupported SKEIN size " ++ show hashsize

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
	let b = genBackendE (SHA2Hash 256)
	in b { getKey = (fmap addE) <$$> getKey b } 
  where
	addE k = k { keyName = keyName k ++ longext }
	longext = ".this-is-a-test-key"
