{- git-annex SHA backend
 -
 - Copyright 2011,2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Backend.SHA (backends) where

import Common.Annex
import qualified Annex
import Types.Backend
import Types.Key
import Types.KeySource

import qualified Build.SysConfig as SysConfig
import Data.Digest.Pure.SHA
import qualified Data.ByteString.Lazy as L

type SHASize = Int

-- order is slightly significant; want SHA256 first, and more general
-- sizes earlier
sizes :: [Int]
sizes = [256, 1, 512, 224, 384]

backends :: [Backend]
backends = catMaybes $ map genBackend sizes ++ map genBackendE sizes

genBackend :: SHASize -> Maybe Backend
genBackend size = Just $ Backend
	{ name = shaName size
	, getKey = keyValue size
	, fsckKey = Just $ checkKeyChecksum size
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
	case shaCommand shasize filesize of
		Left sha -> liftIO $ sha <$> L.readFile file
		Right command -> liftIO $ runcommand command
	where
		runcommand command =
			pOpen ReadFromPipe command (toCommand [File file]) $ \h -> do
				sha <- fst . separate (== ' ') <$> hGetLine h
				if null sha
					then error $ command ++ " parse error"
					else return sha

shaCommand :: SHASize -> Integer -> Either (L.ByteString -> String) String
shaCommand shasize filesize
	| shasize == 1 = use SysConfig.sha1 sha1
	| shasize == 256 = use SysConfig.sha256 sha256
	| shasize == 224 = use SysConfig.sha224 sha224
	| shasize == 384 = use SysConfig.sha384 sha384
	| shasize == 512 = use SysConfig.sha512 sha512
	| otherwise = error $ "bad sha size " ++ show shasize
	where
		use Nothing sha = Left $ showDigest . sha
		use (Just c) sha
			-- use builtin, but slower sha for small files
			-- benchmarking indicates it's faster up to
			-- and slightly beyond 50 kb files
			| filesize < 51200 = use Nothing sha
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
	| otherwise = join "." ("":es)
	where
		es = filter (not . null) $ reverse $
			take 2 $ takeWhile shortenough $
			reverse $ split "." $ takeExtensions f
		shortenough e
			| '\n' `elem` e = False -- newline in extension?!
			| otherwise = length e <= 4 -- long enough for "jpeg"

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
		check s
			| s == dropExtension (keyName key) = True
			| otherwise = False
