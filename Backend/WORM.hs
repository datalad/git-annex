{- git-annex "WORM" backend -- Write Once, Read Many
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Backend.WORM (backends) where

import Common.Annex
import Types.Backend
import Types.Key
import Types.KeySource
import Backend.Utilities
import Git.FilePath

backends :: [Backend]
backends = [backend]

backend :: Backend
backend = Backend
	{ name = "WORM"
	, getKey = keyValue
	, fsckKey = Nothing
	, canUpgradeKey = Nothing
	, fastMigrate = Nothing
	, isStableKey = const True
	}

{- The key includes the file size, modification time, and the
 - original filename relative to the top of the git repository.
 -}
keyValue :: KeySource -> Annex (Maybe Key)
keyValue source = do
	stat <- liftIO $ getFileStatus $ contentLocation source
	relf <- getTopFilePath <$> inRepo (toTopFilePath $ keyFilename source)
	return $ Just $ stubKey
		{ keyName = genKeyName relf
		, keyBackendName = name backend
		, keySize = Just $ fromIntegral $ fileSize stat
		, keyMtime = Just $ modificationTime stat
		}
