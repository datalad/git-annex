{- Credentials storage
 -
 - Copyright 2012-2014 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Creds (
	module Types.Creds,
	CredPairStorage(..),
	setRemoteCredPair,
	getRemoteCredPairFor,
	getRemoteCredPair,
	getEnvCredPair,
	writeCacheCreds,
	readCacheCreds,
) where

import Common.Annex
import Types.Creds
import Annex.Perms
import Utility.FileMode
import Crypto
import Types.Remote (RemoteConfig, RemoteConfigKey)
import Remote.Helper.Encryptable (remoteCipher, embedCreds)
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
 - that. Otherwise, caches them locally.
 - The creds are found in storage if not provided. -}
setRemoteCredPair :: RemoteConfig -> CredPairStorage -> Maybe CredPair -> Annex RemoteConfig
setRemoteCredPair c storage Nothing = 
	maybe (return c) (setRemoteCredPair c storage . Just)
		=<< getRemoteCredPair c storage
setRemoteCredPair c storage (Just creds)
	| embedCreds c = case credPairRemoteKey storage of
		Nothing -> localcache
		Just key -> storeconfig key =<< remoteCipher c
	| otherwise = localcache
  where
	localcache = do
		writeCacheCredPair creds storage
		return c

	storeconfig key (Just cipher) = do
		s <- liftIO $ encrypt (getGpgEncParams c) cipher
			(feedBytes $ L.pack $ encodeCredPair creds)
			(readBytes $ return . L.unpack)
		return $ M.insert key (toB64 s) c
	storeconfig key Nothing =
		return $ M.insert key (toB64 $ encodeCredPair creds) c

{- Gets a remote's credpair, from the environment if set, otherwise
 - from the cache in gitAnnexCredsDir, or failing that, from the
 - value in RemoteConfig. -}
getRemoteCredPairFor :: String -> RemoteConfig -> CredPairStorage -> Annex (Maybe CredPair)
getRemoteCredPairFor this c storage = maybe missing (return . Just) =<< getRemoteCredPair c storage
  where
	(loginvar, passwordvar) = credPairEnvironment storage
	missing = do
		warning $ unwords
			[ "Set both", loginvar
			, "and", passwordvar
			, "to use", this
			]
		return Nothing

getRemoteCredPair :: RemoteConfig -> CredPairStorage -> Annex (Maybe CredPair)
getRemoteCredPair c storage = maybe fromcache (return . Just) =<< fromenv
  where
	fromenv = liftIO $ getEnvCredPair storage
	fromcache = maybe fromconfig (return . Just) =<< readCacheCredPair storage
	fromconfig = case credPairRemoteKey storage of
		Just key -> do
			mcipher <- remoteCipher c
			case (M.lookup key c, mcipher) of
				(Nothing, _) -> return Nothing
				(Just enccreds, Just cipher) -> do
					creds <- liftIO $ decrypt cipher
						(feedBytes $ L.pack $ fromB64 enccreds)
						(readBytes $ return . L.unpack)
					fromcreds creds
				(Just bcreds, Nothing) ->
					fromcreds $ fromB64 bcreds
		Nothing -> return Nothing
	fromcreds creds = case decodeCredPair creds of
		Just credpair -> do
			writeCacheCredPair credpair storage
			return $ Just credpair
		_ -> error "bad creds"

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
readCacheCreds file = do
	d <- fromRepo gitAnnexCredsDir
	let f = d </> file
	liftIO $ catchMaybeIO $ readFile f

encodeCredPair :: CredPair -> Creds
encodeCredPair (l, p) = unlines [l, p]

decodeCredPair :: Creds -> Maybe CredPair
decodeCredPair creds = case lines creds of
	l:p:[] -> Just (l, p)
	_ -> Nothing
