{- A "remote" that is just a filesystem directory.
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Remote.Directory (remote) where

import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Map as M

import Common.Annex
import Utility.CopyFile
import Types.Remote
import qualified Git
import Config
import Utility.FileMode
import Remote.Helper.Special
import Remote.Helper.Encryptable
import Crypto

remote :: RemoteType
remote = RemoteType {
	typename = "directory",
	enumerate = findSpecialRemotes "directory",
	generate = gen,
	setup = directorySetup
}

gen :: Git.Repo -> UUID -> Maybe RemoteConfig -> Annex Remote
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
			retrieveKeyFileCheap = retrieveCheap dir,
			removeKey = remove dir,
			hasKey = checkPresent dir,
			hasKeyCheap = True,
			whereisKey = Nothing,
			config = Nothing,
			repo = r,
			remotetype = remote
		}

directorySetup :: UUID -> RemoteConfig -> Annex RemoteConfig
directorySetup u c = do
	-- verify configuration is sane
	let dir = fromMaybe (error "Specify directory=") $
		M.lookup "directory" c
	liftIO $ unlessM (doesDirectoryExist dir) $
		error $ "Directory does not exist: " ++ dir
	c' <- encryptionSetup c

	-- The directory is stored in git config, not in this remote's
	-- persistant state, so it can vary between hosts.
	gitConfigSpecialRemote u c' "directory" dir
	return $ M.delete "directory" c'

{- Locations to try to access a given Key in the Directory. -}
locations :: FilePath -> Key -> [FilePath]
locations d k = map (d </>) (keyPaths k)

withCheckedFile :: (FilePath -> IO Bool) -> FilePath -> Key -> (FilePath -> IO Bool) -> IO Bool
withCheckedFile _ [] _ _ = return False
withCheckedFile check d k a = go $ locations d k
	where
		go [] = return False
		go (f:fs) = do
			use <- check f
			if use
				then a f
				else go fs

withStoredFile :: FilePath -> Key -> (FilePath -> IO Bool) -> IO Bool
withStoredFile = withCheckedFile doesFileExist

store :: FilePath -> Key -> Annex Bool
store d k = do
	src <- inRepo $ gitAnnexLocation k
	liftIO $ catchBoolIO $ storeHelper d k $ copyFileExternal src

storeEncrypted :: FilePath -> (Cipher, Key) -> Key -> Annex Bool
storeEncrypted d (cipher, enck) k = do
	src <- inRepo $ gitAnnexLocation k
	liftIO $ catchBoolIO $ storeHelper d enck $ encrypt src
	where
		encrypt src dest = do
			withEncryptedContent cipher (L.readFile src) $ L.writeFile dest
			return True

storeHelper :: FilePath -> Key -> (FilePath -> IO Bool) -> IO Bool
storeHelper d key a = do
	let dest = Prelude.head $ locations d key
	let tmpdest = dest ++ ".tmp"
	let dir = parentDir dest
	createDirectoryIfMissing True dir
	allowWrite dir
	ok <- a tmpdest
	when ok $ do
		renameFile tmpdest dest
		preventWrite dest
		preventWrite dir
	return ok

retrieve :: FilePath -> Key -> FilePath -> Annex Bool
retrieve d k f = liftIO $ withStoredFile d k $ \file -> copyFileExternal file f

retrieveCheap :: FilePath -> Key -> FilePath -> Annex Bool
retrieveCheap d k f = liftIO $ withStoredFile d k $ \file ->
	catchBoolIO $ createSymbolicLink file f >> return True

retrieveEncrypted :: FilePath -> (Cipher, Key) -> FilePath -> Annex Bool
retrieveEncrypted d (cipher, enck) f =
	liftIO $ withStoredFile d enck $ \file -> catchBoolIO $ do
		withDecryptedContent cipher (L.readFile file) $ L.writeFile f
		return True

remove :: FilePath -> Key -> Annex Bool
remove d k = liftIO $ withStoredFile d k $ \file -> catchBoolIO $ do
	let dir = parentDir file
	allowWrite dir
	removeFile file
	removeDirectory dir
	return True

checkPresent :: FilePath -> Key -> Annex (Either String Bool)
checkPresent d k = liftIO $ catchMsgIO $ withStoredFile d k $
	const $ return True -- withStoredFile checked that it exists
