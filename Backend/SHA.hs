{- git-annex SHA backends
 -
 - Copyright 2011-2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Backend.SHA (backends) where

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

type SHASize = Int

{- Order is slightly significant; want SHA256 first, and more general
 - sizes earlier. -}
sizes :: [Int]
sizes = [256, 1, 512, 224, 384]

{- The SHA256E backend is the default. -}
backends :: [Backend]
backends = catMaybes $ map genBackendE sizes ++ map genBackend sizes

genBackend :: SHASize -> Maybe Backend
genBackend size = Just $ Backend
	{ name = shaName size
	, getKey = keyValue size
	, fsckKey = Just $ checkKeyChecksum size
	, canUpgradeKey = Just $ needsUpgrade
	}

genBackendE :: SHASize -> Maybe Backend
genBackendE size = do
	b <- genBackend size
	return $ b 
		{ name = shaNameE size
		, getKey = keyValueE size
		}

shaName :: SHASize -> String
shaName size = "SHA" ++ show size

shaNameE :: SHASize -> String
shaNameE size = shaName size ++ "E"

shaN :: SHASize -> FilePath -> Integer -> Annex String
shaN shasize file filesize = do
	showAction "checksum"
	liftIO $ case shaCommand shasize filesize of
		Left sha -> sha <$> L.readFile file
		Right command ->
			either error return 
				=<< externalSHA command shasize file

shaCommand :: SHASize -> Integer -> Either (L.ByteString -> String) String
shaCommand shasize filesize
	| shasize == 1 = use SysConfig.sha1 sha1
	| shasize == 256 = use SysConfig.sha256 sha256
	| shasize == 224 = use SysConfig.sha224 sha224
	| shasize == 384 = use SysConfig.sha384 sha384
	| shasize == 512 = use SysConfig.sha512 sha512
	| otherwise = error $ "bad sha size " ++ show shasize
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

{- A key is a checksum of its contents. -}
keyValue :: SHASize -> KeySource -> Annex (Maybe Key)
keyValue shasize source = do
	let file = contentLocation source
	stat <- liftIO $ getFileStatus file
	let filesize = fromIntegral $ fileSize stat
	s <- shaN shasize file filesize
	return $ Just $ stubKey
		{ keyName = s
		, keyBackendName = shaName shasize
		, keySize = Just filesize
		}

{- Extension preserving keys. -}
keyValueE :: SHASize -> KeySource -> Annex (Maybe Key)
keyValueE size source = keyValue size source >>= maybe (return Nothing) addE
  where
	addE k = return $ Just $ k
		{ keyName = keyName k ++ selectExtension (keyFilename source)
		, keyBackendName = shaNameE size
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
checkKeyChecksum :: SHASize -> Key -> FilePath -> Annex Bool
checkKeyChecksum size key file = do
	fast <- Annex.getState Annex.fast
	mstat <- liftIO $ catchMaybeIO $ getFileStatus file
	case (mstat, fast) of
		(Just stat, False) -> do
			let filesize = fromIntegral $ fileSize stat
			check <$> shaN size file filesize
		_ -> return True
  where
	sha = keySha key
	check s
		| s == sha = True
		{- A bug caused checksums to be prefixed with \ in some
		 - cases; still accept these as legal now that the bug has been
		 - fixed. -}
		| '\\' : s == sha = True
		| otherwise = False

keySha :: Key -> String
keySha key = dropExtensions (keyName key)

validExtension :: Char -> Bool
validExtension c
	| isAlphaNum c = True
	| c == '.' = True
	| otherwise = False

{- Upgrade keys that have the \ prefix on their sha due to a bug, or
 - that contain non-alphanumeric characters in their extension. -}
needsUpgrade :: Key -> Bool
needsUpgrade key = "\\" `isPrefixOf` keySha key ||
	any (not . validExtension) (takeExtensions $ keyName key)
