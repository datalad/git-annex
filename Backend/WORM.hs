{- git-annex "WORM" backend -- Write Once, Read Many
 -
 - Copyright 2010 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Backend.WORM (backends) where

import Annex.Common
import Types.Key
import Types.Backend
import Types.KeySource
import Backend.Utilities
import Git.FilePath
import Utility.Metered

import qualified Data.ByteString.Char8 as S8
import qualified Utility.RawFilePath as R

backends :: [Backend]
backends = [backend]

backend :: Backend
backend = Backend
	{ backendVariety = WORMKey
	, genKey = Just keyValue
	, verifyKeyContent = Nothing
	, canUpgradeKey = Just needsUpgrade
	, fastMigrate = Just removeProblemChars
	, isStableKey = const True
	, isCryptographicallySecure = const False
	}

{- The key includes the file size, modification time, and the
 - original filename relative to the top of the git repository.
 -}
keyValue :: KeySource -> MeterUpdate -> Annex Key
keyValue source _ = do
	let f = contentLocation source
	stat <- liftIO $ R.getFileStatus f
	sz <- liftIO $ getFileSize' f stat
	relf <- fromRawFilePath . getTopFilePath
		<$> inRepo (toTopFilePath $ keyFilename source)
	return $ mkKey $ \k -> k
		{ keyName = genKeyName relf
		, keyVariety = WORMKey
		, keySize = Just sz
		, keyMtime = Just $ modificationTime stat
		}

{- Old WORM keys could contain spaces and carriage returns, 
 - and can be upgraded to remove them. -}
needsUpgrade :: Key -> Bool
needsUpgrade key = any (`S8.elem` fromKey keyName key) [' ', '\r']

removeProblemChars :: Key -> Backend -> AssociatedFile -> Annex (Maybe Key)
removeProblemChars oldkey newbackend _
	| migratable = return $ Just $ alterKey oldkey $ \d -> d
		{ keyName = encodeBS $ reSanitizeKeyName $ decodeBS $ keyName d }
	| otherwise = return Nothing
  where
	migratable = oldvariety == newvariety
	oldvariety = fromKey keyVariety oldkey
	newvariety = backendVariety newbackend
