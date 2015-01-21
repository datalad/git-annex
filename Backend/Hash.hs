{- git-tnnex hashing backends
 -
 - Copyright 2011-2013 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Backend.Hash (
	backends,
	testKeyBackend,
) where

import Common.Annex
import qualified Annex
import Types.Backend
import Types.Key
import Types.KeySource
import Utility.Hash
import Utility.ExternalSHA

import qualified Build.SysConfig as SysConfig
import qualified Data.ByteString.Lazy as L
import Data.Char

data Hash = SHAHash HashSize | SkeinHash HashSize
type HashSize = Int

{- Order is slightly significant; want SHA256 first, and more general
 - sizes earlier. -}
hashes :: [Hash]
hashes = concat 
	[ map SHAHash [256, 1, 512, 224, 384]
#ifdef WITH_CRYPTOHASH
	, map SkeinHash [256, 512]
#endif
	]

{- The SHA256E backend is the default, so genBackendE comes first. -}
backends :: [Backend]
backends = map genBackendE hashes ++ map genBackend hashes

genBackend :: Hash -> Backend
genBackend hash = Backend
	{ name = hashName hash
	, getKey = keyValue hash
	, fsckKey = Just $ checkKeyChecksum hash
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
hashName (SHAHash size) = "SHA" ++ show size
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
checkKeyChecksum hash key file = do
	fast <- Annex.getState Annex.fast
	mstat <- liftIO $ catchMaybeIO $ getFileStatus file
	case (mstat, fast) of
		(Just stat, False) -> do
			filesize <- liftIO $ getFileSize' file stat
			showSideAction "checksum"
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
hashFile hash file filesize = liftIO $ go hash
  where
	go (SHAHash hashsize) = case shaHasher hashsize filesize of
		Left sha -> sha <$> L.readFile file
		Right command ->
			either error return 
				=<< externalSHA command hashsize file
	go (SkeinHash hashsize) = skeinHasher hashsize <$> L.readFile file

shaHasher :: HashSize -> Integer -> Either (L.ByteString -> String) String
shaHasher hashsize filesize
	| hashsize == 1 = use SysConfig.sha1 sha1
	| hashsize == 256 = use SysConfig.sha256 sha256
	| hashsize == 224 = use SysConfig.sha224 sha224
	| hashsize == 384 = use SysConfig.sha384 sha384
	| hashsize == 512 = use SysConfig.sha512 sha512
	| otherwise = error $ "unsupported sha size " ++ show hashsize
  where
	use Nothing hasher = Left $ show . hasher
	use (Just c) hasher
		{- Use builtin, but slightly slower hashing for
		 - smallish files. Cryptohash benchmarks 90 to 101%
		 - faster than external hashers, depending on the hash
		 - and system. So there is no point forking an external
		 - process unless the file is large. -}
		| filesize < 1048576 = use Nothing hasher
		| otherwise = Right c

skeinHasher :: HashSize -> (L.ByteString -> String)
skeinHasher hashsize 
#ifdef WITH_CRYPTOHASH
	| hashsize == 256 = show . skein256
	| hashsize == 512 = show . skein512
#endif
	| otherwise = error $ "unsupported skein size " ++ show hashsize

{- A varient of the SHA256E backend, for testing that needs special keys
 - that cannot collide with legitimate keys in the repository.
 -
 - This is accomplished by appending a special extension to the key,
 - that is not one that selectExtension would select (due to being too
 - long).
 -}
testKeyBackend :: Backend
testKeyBackend = 
	let b = genBackendE (SHAHash 256)
	in b { getKey = (fmap addE) <$$> getKey b } 
  where
	addE k = k { keyName = keyName k ++ longext }
	longext = ".this-is-a-test-key"
