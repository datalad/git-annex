{- git-annex "WORM" backend -- Write Once, Read Many
 -
 - Copyright 2010 Joey Hess <id@joeyh.name>
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
	, verifyKeyContent = Nothing
	, canUpgradeKey = Nothing
	, fastMigrate = Nothing
	, isStableKey = const True
	}

{- The key includes the file size, modification time, and the
 - original filename relative to the top of the git repository.
 -}
keyValue :: KeySource -> Annex (Maybe Key)
keyValue source = do
	let f = contentLocation source
	stat <- liftIO $ getFileStatus f
	sz <- liftIO $ getFileSize' f stat
	relf <- getTopFilePath <$> inRepo (toTopFilePath $ keyFilename source)
	return $ Just $ stubKey
		{ keyName = genKeyName relf
		, keyBackendName = name backend
		, keySize = Just sz
		, keyMtime = Just $ modificationTime stat
		}
