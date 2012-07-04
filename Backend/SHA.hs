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

shaCommand :: SHASize -> Either (L.ByteString -> String) String
shaCommand sz
	| sz == 1 = use SysConfig.sha1 sha1
	| sz == 256 = use SysConfig.sha256 sha256
	| sz == 224 = use SysConfig.sha224 sha224
	| sz == 384 = use SysConfig.sha384 sha384
	| sz == 512 = use SysConfig.sha512 sha512
	| otherwise = error $ "bad sha size " ++ show sz
	where
		use Nothing sha = Left $ showDigest . sha
		use (Just c) _ = Right c

shaName :: SHASize -> String
shaName size = "SHA" ++ show size

shaNameE :: SHASize -> String
shaNameE size = shaName size ++ "E"

shaN :: SHASize -> FilePath -> Annex String
shaN size file = do
	showAction "checksum"
	case shaCommand size of
		Left sha -> liftIO $ sha <$> L.readFile file
		Right command -> liftIO $ runcommand command
	where
		runcommand command =
			pOpen ReadFromPipe command (toCommand [File file]) $ \h -> do
				sha <- fst . separate (== ' ') <$> hGetLine h
				if null sha
					then error $ command ++ " parse error"
					else return sha

{- A key is a checksum of its contents. -}
keyValue :: SHASize -> KeySource -> Annex (Maybe Key)
keyValue size source = do
	let file = contentLocation source
	s <- shaN size file
	stat <- liftIO $ getFileStatus file
	return $ Just $ stubKey
		{ keyName = s
		, keyBackendName = shaName size
		, keySize = Just $ fromIntegral $ fileSize stat
		}

{- Extension preserving keys. -}
keyValueE :: SHASize -> KeySource -> Annex (Maybe Key)
keyValueE size source = keyValue size source >>= maybe (return Nothing) addE
	where
		addE k = return $ Just $ k
			{ keyName = keyName k ++ extension
			, keyBackendName = shaNameE size
			}
		naiveextension = takeExtension $ keyFilename source
		extension
			-- long or newline containing extensions are 
			-- probably not really an extension
			| length naiveextension > 6 ||
			  '\n' `elem` naiveextension = ""
			| otherwise = naiveextension

{- A key's checksum is checked during fsck. -}
checkKeyChecksum :: SHASize -> Key -> FilePath -> Annex Bool
checkKeyChecksum size key file = do
	fast <- Annex.getState Annex.fast
	present <- liftIO $ doesFileExist file
	if not present || fast
		then return True
		else check <$> shaN size file
	where
		check s
			| s == dropExtension (keyName key) = True
			| otherwise = False
