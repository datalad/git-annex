{- Credentials storage
 -
 - Copyright 2012-2014 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Creds (
	module Types.Creds,
	CredPairStorage(..),
	setRemoteCredPair,
	getRemoteCredPair,
	getRemoteCredPairFor,
	warnMissingCredPairFor,
	getEnvCredPair,
	writeCacheCreds,
	readCacheCreds,
	removeCreds,
	includeCredsInfo,
) where

import Annex.Common
import qualified Annex
import Types.Creds
import Annex.Perms
import Utility.FileMode
import Crypto
import Types.Remote (RemoteConfig, RemoteConfigKey)
import Remote.Helper.Encryptable (remoteCipher, remoteCipher', embedCreds, EncryptionIsSetup, extractCipher)
import Utility.Env (getEnv)

import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Map as M
import Utility.Base64

{- A CredPair can be stored in a file, or in the environment, or perhaps
 - in a remote's configuration. -}
data CredPairStorage = CredPairStorage
	{ credPairFile :: FilePath
	, credPairEnvironment :: (String, String)
	, credPairRemoteKey :: Maybe RemoteConfigKey
	}

{- Stores creds in a remote's configuration, if the remote allows
 - that. Also caches them locally.
 -
 - The creds are found from the CredPairStorage storage if not provided,
 - so may be provided by an environment variable etc.
 -
 - The remote's configuration should have already had a cipher stored in it
 - if that's going to be done, so that the creds can be encrypted using the
 - cipher. The EncryptionIsSetup phantom type ensures that is the case.
 -}
setRemoteCredPair :: EncryptionIsSetup -> RemoteConfig -> RemoteGitConfig -> CredPairStorage -> Maybe CredPair -> Annex RemoteConfig
setRemoteCredPair encsetup c gc storage mcreds = case mcreds of
	Nothing -> maybe (return c) (setRemoteCredPair encsetup c gc storage . Just)
		=<< getRemoteCredPair c gc storage
	Just creds
		| embedCreds c -> case credPairRemoteKey storage of
			Nothing -> localcache creds
			Just key -> storeconfig creds key =<< remoteCipher =<< localcache creds
		| otherwise -> localcache creds
  where
	localcache creds = do
		writeCacheCredPair creds storage
		return c

	storeconfig creds key (Just cipher) = do
		cmd <- gpgCmd <$> Annex.getGitConfig
		s <- liftIO $ encrypt cmd (c, gc) cipher
			(feedBytes $ L.pack $ encodeCredPair creds)
			(readBytes $ return . L.unpack)
		return $ M.insert key (toB64 s) c
	storeconfig creds key Nothing =
		return $ M.insert key (toB64 $ encodeCredPair creds) c

{- Gets a remote's credpair, from the environment if set, otherwise
 - from the cache in gitAnnexCredsDir, or failing that, from the
 - value in RemoteConfig. -}
getRemoteCredPair :: RemoteConfig -> RemoteGitConfig -> CredPairStorage -> Annex (Maybe CredPair)
getRemoteCredPair c gc storage = maybe fromcache (return . Just) =<< fromenv
  where
	fromenv = liftIO $ getEnvCredPair storage
	fromcache = maybe fromconfig (return . Just) =<< readCacheCredPair storage
	fromconfig = case credPairRemoteKey storage of
		Just key -> do
			mcipher <- remoteCipher' c
			case (M.lookup key c, mcipher) of
				(Nothing, _) -> return Nothing
				(Just enccreds, Just (cipher, storablecipher)) ->
					fromenccreds enccreds cipher storablecipher
				(Just bcreds, Nothing) ->
					fromcreds $ fromB64 bcreds
		Nothing -> return Nothing
	fromenccreds enccreds cipher storablecipher = do
		cmd <- gpgCmd <$> Annex.getGitConfig
		mcreds <- liftIO $ catchMaybeIO $ decrypt cmd (c, gc) cipher
			(feedBytes $ L.pack $ fromB64 enccreds)
			(readBytes $ return . L.unpack)
		case mcreds of
			Just creds -> fromcreds creds
			Nothing -> do
				-- Work around un-encrypted creds storage
				-- bug in old S3 and glacier remotes.
				-- Not a problem for shared cipher.
				case storablecipher of
					SharedCipher {} -> showLongNote "gpg error above was caused by an old git-annex bug in credentials storage. Working around it.."
					_ -> error "*** Insecure credentials storage detected for this remote! See https://git-annex.branchable.com/upgrades/insecure_embedded_creds/"
				fromcreds $ fromB64 enccreds
	fromcreds creds = case decodeCredPair creds of
		Just credpair -> do
			writeCacheCredPair credpair storage

			return $ Just credpair
		_ -> error "bad creds"

getRemoteCredPairFor :: String -> RemoteConfig -> RemoteGitConfig -> CredPairStorage -> Annex (Maybe CredPair)
getRemoteCredPairFor this c gc storage = go =<< getRemoteCredPair c gc storage
  where
	go Nothing = do
		warnMissingCredPairFor this storage
		return Nothing
	go (Just credpair) = return $ Just credpair

warnMissingCredPairFor :: String -> CredPairStorage -> Annex ()
warnMissingCredPairFor this storage = warning $ unwords
	[ "Set both", loginvar
	, "and", passwordvar
	, "to use", this
	]
  where
	(loginvar, passwordvar) = credPairEnvironment storage

{- Gets a CredPair from the environment. -}
getEnvCredPair :: CredPairStorage -> IO (Maybe CredPair)
getEnvCredPair storage = liftM2 (,)
	<$> getEnv uenv
	<*> getEnv penv
  where
	(uenv, penv) = credPairEnvironment storage

writeCacheCredPair :: CredPair -> CredPairStorage -> Annex ()
writeCacheCredPair credpair storage =
	writeCacheCreds (encodeCredPair credpair) (credPairFile storage)

{- Stores the creds in a file inside gitAnnexCredsDir that only the user
 - can read. -}
writeCacheCreds :: Creds -> FilePath -> Annex ()
writeCacheCreds creds file = do
	d <- fromRepo gitAnnexCredsDir
	createAnnexDirectory d
	liftIO $ writeFileProtected (d </> file) creds

readCacheCredPair :: CredPairStorage -> Annex (Maybe CredPair)
readCacheCredPair storage = maybe Nothing decodeCredPair
	<$> readCacheCreds (credPairFile storage)

readCacheCreds :: FilePath -> Annex (Maybe Creds)
readCacheCreds f = liftIO . catchMaybeIO . readFile =<< cacheCredsFile f

cacheCredsFile :: FilePath -> Annex FilePath
cacheCredsFile basefile = do
	d <- fromRepo gitAnnexCredsDir
	return $ d </> basefile

existsCacheCredPair :: CredPairStorage -> Annex Bool
existsCacheCredPair storage = 
	liftIO . doesFileExist =<< cacheCredsFile (credPairFile storage)

encodeCredPair :: CredPair -> Creds
encodeCredPair (l, p) = unlines [l, p]

decodeCredPair :: Creds -> Maybe CredPair
decodeCredPair creds = case lines creds of
	l:p:[] -> Just (l, p)
	_ -> Nothing

removeCreds :: FilePath -> Annex ()
removeCreds file = do
	d <- fromRepo gitAnnexCredsDir
	let f = d </> file
	liftIO $ nukeFile f

includeCredsInfo :: RemoteConfig -> CredPairStorage -> [(String, String)] -> Annex [(String, String)]
includeCredsInfo c storage info = do
	v <- liftIO $ getEnvCredPair storage
	case v of
		Just _ -> do
			let (uenv, penv) = credPairEnvironment storage
			ret $ "from environment variables (" ++ unwords [uenv, penv] ++ ")"
		Nothing -> case (`M.lookup` c) =<< credPairRemoteKey storage of
			Nothing -> ifM (existsCacheCredPair storage)
				( ret "stored locally"
				, ret "not available"
				)
			Just _ -> case extractCipher c of
				Just (EncryptedCipher {}) -> ret "embedded in git repository (gpg encrypted)"
				_ -> ret "embedded in git repository (not encrypted)"
  where
	ret s = return $ ("creds", s) : info
