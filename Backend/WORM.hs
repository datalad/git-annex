{- git-annex "WORM" backend -- Write Once, Read Many
 -
 - Copyright 2010 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Backend.WORM (backends) where

import Annex.Common
import Types.Key
import Types.Backend
import Types.KeySource
import Backend.Utilities
import Git.FilePath

backends :: [Backend]
backends = [backend]

backend :: Backend
backend = Backend
	{ backendVariety = WORMKey
	, getKey = keyValue
	, verifyKeyContent = Nothing
	, canUpgradeKey = Just needsUpgrade
	, fastMigrate = Just removeSpaces
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
		, keyVariety = WORMKey
		, keySize = Just sz
		, keyMtime = Just $ modificationTime stat
		}

{- Old WORM keys could contain spaces, and can be upgraded to remove them. -}
needsUpgrade :: Key -> Bool
needsUpgrade key = ' ' `elem` keyName key

removeSpaces :: Key -> Backend -> AssociatedFile -> Maybe Key
removeSpaces oldkey newbackend _
	| migratable = Just $ oldkey
		{ keyName = reSanitizeKeyName (keyName oldkey) }
	| otherwise = Nothing
  where
	migratable = oldvariety == newvariety
	oldvariety = keyVariety oldkey
	newvariety = backendVariety newbackend
