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

backends :: [Backend]
backends = [backend]

backend :: Backend
backend = Backend {
	name = "WORM",
	getKey = keyValue,
	fsckKey = Nothing
}

{- The key includes the file size, modification time, and the
 - basename of the filename.
 -
 - That allows multiple files with the same names to have different keys,
 - while also allowing a file to be moved around while retaining the
 - same key.
 -}
keyValue :: FilePath -> Annex (Maybe Key)
keyValue file = do
	stat <- liftIO $ getFileStatus file
	return $ Just Key {
		keyName = takeFileName file,
		keyBackendName = name backend,
		keySize = Just $ fromIntegral $ fileSize stat,
		keyMtime = Just $ modificationTime stat
	}
