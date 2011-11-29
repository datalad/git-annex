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
			config = Nothing,
			repo = r
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

{- Where to store a given Key in the Directory.
 -
 - There are two possible locations to try; this had to be done because
 - on Linux, vfat filesystem mounted with shortname=mixed have a
 - variant of case insensativity that causes miserable failure when
 - hashDirMixed produces eg, "xx" and "XX". The first directory to be
 - created wins the namespace, and the second one cannot then be created.
 - But unlike behavior with shortname=lower, "XX/foo" won't look in
 - "xx/foo".
 -}
locations :: FilePath -> Key -> [FilePath]
locations d k = [using hashDirMixed, using hashDirLower]
	where
		using h = d </> h k </> f </> f
		f = keyFile k

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
storeHelper d key a = withCheckedFile check d key go
	where
		check dest = isJust <$> mkdir (parentDir dest)
		mkdir = catchMaybeIO . createDirectoryIfMissing True
		go dest = do
			let dir = parentDir dest
			allowWrite dir
			ok <- a dest
			when ok $ do
				preventWrite dest
				preventWrite dir
			return ok

retrieve :: FilePath -> Key -> FilePath -> Annex Bool
retrieve d k f = liftIO $ withStoredFile d k $ \file -> copyFileExternal file f

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
