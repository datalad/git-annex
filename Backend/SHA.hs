{- git-annex SHA backend
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Backend.SHA (backends) where

import Common.Annex
import qualified Annex
import Types.Backend
import Types.Key
import qualified Build.SysConfig as SysConfig

type SHASize = Int

-- order is slightly significant; want SHA256 first, and more general
-- sizes earlier
sizes :: [Int]
sizes = [256, 1, 512, 224, 384]

backends :: [Backend]
backends = catMaybes $ map genBackend sizes ++ map genBackendE sizes

genBackend :: SHASize -> Maybe Backend
genBackend size
	| isNothing (shaCommand size) = Nothing
	| otherwise = Just b
	where
		b = Backend
			{ name = shaName size
			, getKey = keyValue size
			, fsckKey = Just $ checkKeyChecksum size
			}

genBackendE :: SHASize -> Maybe Backend
genBackendE size =
	case genBackend size of
		Nothing -> Nothing
		Just b -> Just $ b
			{ name = shaNameE size
			, getKey = keyValueE size
			}

shaCommand :: SHASize -> Maybe String
shaCommand 1 = SysConfig.sha1
shaCommand 256 = Just SysConfig.sha256
shaCommand 224 = SysConfig.sha224
shaCommand 384 = SysConfig.sha384
shaCommand 512 = SysConfig.sha512
shaCommand _ = Nothing

shaName :: SHASize -> String
shaName size = "SHA" ++ show size

shaNameE :: SHASize -> String
shaNameE size = shaName size ++ "E"

shaN :: SHASize -> FilePath -> Annex String
shaN size file = do
	showAction "checksum"
	liftIO $ pOpen ReadFromPipe command (toCommand [File file]) $ \h -> do
		sha <- fst . separate (== ' ') <$> hGetLine h
		if null sha
			then error $ command ++ " parse error"
			else return sha
	where
		command = fromJust $ shaCommand size

{- A key is a checksum of its contents. -}
keyValue :: SHASize -> FilePath -> Annex (Maybe Key)
keyValue size file = do
	s <- shaN size file	
	stat <- liftIO $ getFileStatus file
	return $ Just $ stubKey
		{ keyName = s
		, keyBackendName = shaName size
		, keySize = Just $ fromIntegral $ fileSize stat
		}

{- Extension preserving keys. -}
keyValueE :: SHASize -> FilePath -> Annex (Maybe Key)
keyValueE size file = keyValue size file >>= maybe (return Nothing) addE
	where
		addE k = return $ Just $ k
			{ keyName = keyName k ++ extension
			, keyBackendName = shaNameE size
			}
		naiveextension = takeExtension file
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
