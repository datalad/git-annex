{- git-annex SHA backend
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Backend.SHA (backends) where

import Control.Monad.State
import Data.String.Utils
import System.Cmd.Utils
import System.IO
import System.Directory
import Data.Maybe
import System.Posix.Files
import System.FilePath

import Messages
import qualified Annex
import Locations
import Content
import Types
import Types.Backend
import Types.Key
import Utility
import qualified SysConfig

type SHASize = Int

sizes :: [Int]
sizes = [1, 256, 512, 224, 384]

backends :: [Backend Annex]
-- order is slightly significant; want sha1 first ,and more general
-- sizes earlier
backends = catMaybes $ map genBackend sizes ++ map genBackendE sizes

genBackend :: SHASize -> Maybe (Backend Annex)
genBackend size
	| shaCommand size == Nothing = Nothing
	| otherwise = Just b
	where
		b = Types.Backend.Backend
			{ name = shaName size
			, getKey = keyValue size
			, fsckKey = checkKeyChecksum size
			}

genBackendE :: SHASize -> Maybe (Backend Annex)
genBackendE size =
	case genBackend size of
		Nothing -> Nothing
		Just b -> Just $ b
			{ name = shaNameE size
			, getKey = keyValueE size
			}

shaCommand :: SHASize -> Maybe String
shaCommand 1 = SysConfig.sha1
shaCommand 256 = SysConfig.sha256
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
		line <- hGetLine h
		let bits = split " " line
		if null bits
			then error $ command ++ " parse error"
			else return $ head bits
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
		extension = 
			if length naiveextension > 6
				then "" -- probably not really an extension
				else naiveextension

-- A key's checksum is checked during fsck.
checkKeyChecksum :: SHASize -> Key -> Annex Bool
checkKeyChecksum size key = do
	g <- Annex.gitRepo
	fast <- Annex.getState Annex.fast
	let file = gitAnnexLocation g key
	present <- liftIO $ doesFileExist file
	if not present || fast
		then return True
		else do
			s <- shaN size file
			if s == dropExtension (keyName key)
				then return True
				else do
					dest <- moveBad key
					warning $ "Bad file content; moved to " ++ dest
					return False
