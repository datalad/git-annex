{- Credentials storage
 -
 - Copyright 2012-2021 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Creds (
	module Types.Creds,
	CredPairStorage(..),
	setRemoteCredPair,
	setRemoteCredPair',
	getRemoteCredPair,
	getRemoteCredPairFor,
	missingCredPairFor,
	getEnvCredPair,
	writeCreds,
	readCreds,
	credsFile,
	removeCreds,
	includeCredsInfo,
) where

import Annex.Common
import qualified Annex
import Types.Creds
import Types.RemoteConfig
import Types.Remote (SetupStage(..))
import Annex.SpecialRemote.Config
import Annex.Perms
import Utility.FileMode
import Crypto
import Types.ProposedAccepted
import Remote.Helper.Encryptable (remoteCipher, remoteCipher', embedCreds, EncryptionIsSetup, extractCipher)
import Utility.Env (getEnv)
import Utility.Base64
import qualified Utility.RawFilePath as R
import qualified Utility.FileIO as F

import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Char8 as S8
import qualified Data.Map as M

{- A CredPair can be stored in a file, or in the environment, or
 - in a remote's configuration. -}
data CredPairStorage = CredPairStorage
	{ credPairFile :: OsPath
	, credPairEnvironment :: (String, String)
	, credPairRemoteField :: RemoteConfigField
	}

{- Stores creds in a remote's configuration, if the remote allows
 - that. Also caches them locally.
 -
 - The creds are found from the CredPairStorage storage if not provided,
 - so may be provided by an environment variable etc. When autoenabling,
 - the user would not expect to provide creds, so none are found.
 -
 - The remote's configuration should have already had a cipher stored in it
 - if that's going to be done, so that the creds can be encrypted using the
 - cipher. The EncryptionIsSetup is witness to that being the case.
 -}
setRemoteCredPair
	:: SetupStage
	-> EncryptionIsSetup
	-> ParsedRemoteConfig
	-> RemoteGitConfig
	-> CredPairStorage
	-> Maybe CredPair
	-> Annex RemoteConfig
setRemoteCredPair ss encsetup pc gc storage mcreds = case ss of
	AutoEnable _ -> pure (unparsedRemoteConfig pc)
	_ -> unparsedRemoteConfig <$>
		setRemoteCredPair' pc encsetup gc storage mcreds

setRemoteCredPair'
	:: ParsedRemoteConfig
	-> EncryptionIsSetup
	-> RemoteGitConfig
	-> CredPairStorage
	-> Maybe CredPair
	-> Annex ParsedRemoteConfig
setRemoteCredPair' pc encsetup gc storage mcreds = case mcreds of
	Nothing -> maybe (return pc) (setRemoteCredPair' pc encsetup gc storage . Just)
		=<< getRemoteCredPair pc gc storage
	Just creds
		| embedCreds pc -> do
			let key = credPairRemoteField storage
			localcache creds
			storeconfig creds key =<< remoteCipher pc gc
		| otherwise -> do
			localcache creds
			return pc
  where
	localcache creds = writeCacheCredPair creds storage

	storeconfig creds key (Just cipher) = do
		cmd <- gpgCmd <$> Annex.getGitConfig
		s <- liftIO $ encrypt cmd (pc, gc) cipher
			(feedBytes $ L8.pack $ encodeCredPair creds)
			(readBytesStrictly return)
		storeconfig' key (Accepted (decodeBS (toB64 s)))
	storeconfig creds key Nothing =
		storeconfig' key (Accepted (decodeBS $ toB64 $ encodeBS $ encodeCredPair creds))
	
	storeconfig' key val = return $ pc
		{ parsedRemoteConfigMap = M.insert key (RemoteConfigValue val) (parsedRemoteConfigMap pc)
		, unparsedRemoteConfig = M.insert key val (unparsedRemoteConfig pc)
		}

{- Gets a remote's credpair, from the environment if set, otherwise
 - from the cache in gitAnnexCredsDir, or failing that, from the
 - value in RemoteConfig. -}
getRemoteCredPair :: ParsedRemoteConfig -> RemoteGitConfig -> CredPairStorage -> Annex (Maybe CredPair)
getRemoteCredPair c gc storage = maybe fromcache (return . Just) =<< fromenv
  where
	fromenv = liftIO $ getEnvCredPair storage
	fromcache = maybe fromconfig (return . Just) =<< readCacheCredPair storage
	fromconfig = do
		let key = credPairRemoteField storage
		mcipher <- remoteCipher' c gc
		-- The RemoteConfig value may be passed through.
		-- Check for those first, because getRemoteConfigValue
		-- will throw an error if it does not find it.
		let getval = M.lookup key (getRemoteConfigPassedThrough c)
			<|> getRemoteConfigValue key c			
		case (getval, mcipher) of
			(Nothing, _) -> return Nothing
			(Just enccreds, Just (cipher, storablecipher)) ->
				fromenccreds (encodeBS enccreds) cipher storablecipher
			(Just bcreds, Nothing) ->
				fromcreds $ decodeBS $ fromB64 $ encodeBS bcreds
	fromenccreds enccreds cipher storablecipher = do
		cmd <- gpgCmd <$> Annex.getGitConfig
		mcreds <- liftIO $ catchMaybeIO $ decrypt cmd (c, gc) cipher
			(feedBytes $ L8.fromStrict $ fromB64 enccreds)
			(readBytesStrictly $ return . S8.unpack)
		case mcreds of
			Just creds -> fromcreds creds
			Nothing -> do
				-- Work around un-encrypted creds storage
				-- bug in old S3 and glacier remotes.
				-- Not a problem for shared cipher.
				case storablecipher of
					SharedCipher {} -> showLongNote "gpg error above was caused by an old git-annex bug in credentials storage. Working around it.."
					_ -> giveup "*** Insecure credentials storage detected for this remote! See https://git-annex.branchable.com/upgrades/insecure_embedded_creds/"
				fromcreds $ decodeBS $ fromB64 enccreds
	fromcreds creds = case decodeCredPair creds of
		Just credpair -> do
			writeCacheCredPair credpair storage

			return $ Just credpair
		_ -> giveup "bad creds"

getRemoteCredPairFor :: String -> ParsedRemoteConfig -> RemoteGitConfig -> CredPairStorage -> Annex (Maybe CredPair)
getRemoteCredPairFor this c gc storage = go =<< getRemoteCredPair c gc storage
  where
	go Nothing = do
		warning $ UnquotedString $ missingCredPairFor this storage
		return Nothing
	go (Just credpair) = return $ Just credpair

missingCredPairFor :: String -> CredPairStorage -> String
missingCredPairFor this storage = unwords
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

{- Writes a cred pair to local cache, unless prevented by configuration. -}
writeCacheCredPair :: CredPair -> CredPairStorage -> Annex ()
writeCacheCredPair credpair storage = 
	whenM (annexCacheCreds <$> Annex.getGitConfig) $
		writeCreds (encodeCredPair credpair) (credPairFile storage)

readCacheCredPair :: CredPairStorage -> Annex (Maybe CredPair)
readCacheCredPair storage = maybe Nothing decodeCredPair
	<$> readCreds (credPairFile storage)

existsCacheCredPair :: CredPairStorage -> Annex Bool
existsCacheCredPair storage = 
	liftIO . doesFileExist =<< credsFile (credPairFile storage)

{- Stores the creds in a file inside gitAnnexCredsDir that only the user
 - can read. -}
writeCreds :: Creds -> OsPath -> Annex ()
writeCreds creds file = do
	d <- fromRepo gitAnnexCredsDir
	createAnnexDirectory d
	liftIO $ writeFileProtected (d </> file) creds

readCreds :: OsPath -> Annex (Maybe Creds)
readCreds f = do
	f' <- credsFile f
	liftIO $ catchMaybeIO $ decodeBS . S8.unlines . fileLines'
		<$> F.readFile' f'

credsFile :: OsPath -> Annex OsPath
credsFile basefile = do
	d <- fromRepo gitAnnexCredsDir
	return $ d </> basefile

encodeCredPair :: CredPair -> Creds
encodeCredPair (l, p) = unlines [l, p]

decodeCredPair :: Creds -> Maybe CredPair
decodeCredPair creds = case lines creds of
	l:p:[] -> Just (l, p)
	_ -> Nothing

removeCreds :: OsPath -> Annex ()
removeCreds file = do
	d <- fromRepo gitAnnexCredsDir
	liftIO $ removeWhenExistsWith R.removeLink (fromOsPath (d </> file))

includeCredsInfo :: ParsedRemoteConfig -> CredPairStorage -> [(String, String)] -> Annex [(String, String)]
includeCredsInfo pc@(ParsedRemoteConfig cm _) storage info = do
	v <- liftIO $ getEnvCredPair storage
	case v of
		Just _ -> do
			let (uenv, penv) = credPairEnvironment storage
			ret $ "from environment variables (" ++ unwords [uenv, penv] ++ ")"
		Nothing -> case (`M.lookup` cm) (credPairRemoteField storage) of
			Nothing -> ifM (existsCacheCredPair storage)
				( ret "stored locally"
				, ret "not available"
				)
			Just _ -> case extractCipher pc of
				Just (EncryptedCipher {}) -> ret "embedded in git repository (gpg encrypted)"
				_ -> ret "embedded in git repository (not encrypted)"
  where
	ret s = return $ ("creds", s) : info
