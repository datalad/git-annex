{- A "remote" that is just a filesystem directory.
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Remote.Directory (remote) where

import qualified Data.ByteString.Lazy.Char8 as L
import System.IO.Error
import Control.Exception.Extensible (IOException)
import qualified Data.Map as M
import Control.Monad (when)
import Control.Monad.State (liftIO)
import System.Directory hiding (copyFile)
import System.FilePath
import Data.Maybe

import Types
import Types.Remote
import qualified Git
import qualified Annex
import UUID
import Locations
import Utility.CopyFile
import Config
import Content
import Utility
import Remote.Special
import Remote.Encryptable
import Crypto

remote :: RemoteType Annex
remote = RemoteType {
	typename = "directory",
	enumerate = findSpecialRemotes "directory",
	generate = gen,
	setup = directorySetup
}

gen :: Git.Repo -> UUID -> Maybe RemoteConfig -> Annex (Remote Annex)
gen r u c = do
	dir <- getConfig r "directory" (error "missing directory")
	cst <- remoteCost r cheapRemoteCost
	return $ encryptableRemote c
		(storeEncrypted dir)
		(retrieveEncrypted dir)
		Remote {
			uuid = u,
			cost = cst,
			name = Git.repoDescribe r,
 			storeKey = store dir,
			retrieveKeyFile = retrieve dir,
			removeKey = remove dir,
			hasKey = checkPresent dir,
			hasKeyCheap = True,
			config = Nothing
		}

directorySetup :: UUID -> RemoteConfig -> Annex RemoteConfig
directorySetup u c = do
	-- verify configuration is sane
	let dir = fromMaybe (error "Specify directory=") $
		M.lookup "directory" c
	liftIO $ doesDirectoryExist dir
		>>! error $ "Directory does not exist: " ++ dir
	c' <- encryptionSetup c

	-- The directory is stored in git config, not in this remote's
	-- persistant state, so it can vary between hosts.
	gitConfigSpecialRemote u c' "directory" dir
	return $ M.delete "directory" c'

dirKey :: FilePath -> Key -> FilePath
dirKey d k = d </> hashDirMixed k </> f </> f
	where
		f = keyFile k

store :: FilePath -> Key -> Annex Bool
store d k = do
	g <- Annex.gitRepo
	let src = gitAnnexLocation g k
	let dest = dirKey d k
	liftIO $ catchBool $ storeHelper dest $ copyFile src dest

storeEncrypted :: FilePath -> (Cipher, Key) -> Key -> Annex Bool
storeEncrypted d (cipher, enck) k = do
	g <- Annex.gitRepo
	let src = gitAnnexLocation g k
	let dest = dirKey d enck
	liftIO $ catchBool $ storeHelper dest $ encrypt src dest
	where
		encrypt src dest = do
			withEncryptedContent cipher (L.readFile src) $ L.writeFile dest
			return True

storeHelper :: FilePath -> IO Bool -> IO Bool
storeHelper dest a = do
	let dir = parentDir dest
	createDirectoryIfMissing True dir
	allowWrite dir	
	ok <- a
	when ok $ do
		preventWrite dest
		preventWrite dir
	return ok

retrieve :: FilePath -> Key -> FilePath -> Annex Bool
retrieve d k f = liftIO $ copyFile (dirKey d k) f

retrieveEncrypted :: FilePath -> (Cipher, Key) -> FilePath -> Annex Bool
retrieveEncrypted d (cipher, enck) f =
	liftIO $ catchBool $ do
		withDecryptedContent cipher (L.readFile (dirKey d enck)) $ L.writeFile f
		return True

remove :: FilePath -> Key -> Annex Bool
remove d k = liftIO $ catchBool $ do
	allowWrite dir
	removeFile file
	removeDirectory dir
	return True
	where
		file = dirKey d k
		dir = parentDir file

checkPresent :: FilePath -> Key -> Annex (Either IOException Bool)
checkPresent d k = liftIO $ try $ doesFileExist (dirKey d k)
