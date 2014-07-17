{- Using ddar as a remote. Based on bup and rsync remotes.
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 - Copyright 2014 Robie Basak <robie@justgohome.co.uk>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Remote.Ddar (remote) where

import Control.Exception
import qualified Data.ByteString.Lazy as L
import qualified Data.Map as M
import System.IO.Error
import System.Process

import Data.String.Utils
import Common.Annex
import Types.Remote
import Types.Key
import Types.Creds
import qualified Git
import Config
import Config.Cost
import Remote.Helper.Special
import Remote.Helper.Encryptable
import Crypto
import Annex.Content
import Annex.Ssh
import Annex.UUID
import Utility.Metered

type DdarRepo = String

remote :: RemoteType
remote = RemoteType {
	typename = "ddar",
	enumerate = findSpecialRemotes "ddarrepo",
	generate = gen,
	setup = ddarSetup
}

gen :: Git.Repo -> UUID -> RemoteConfig -> RemoteGitConfig -> Annex (Maybe Remote)
gen r u c gc = do
	cst <- remoteCost gc $
		if ddarLocal ddarrepo
			then nearlyCheapRemoteCost
			else expensiveRemoteCost

	let new = Remote
		{ uuid = u
		, cost = cst
		, name = Git.repoDescribe r
		, storeKey = store ddarrepo
		, retrieveKeyFile = retrieve ddarrepo
		, retrieveKeyFileCheap = retrieveCheap
		, removeKey = remove ddarrepo
		, hasKey = checkPresent ddarrepo
		, hasKeyCheap = ddarLocal ddarrepo
		, whereisKey = Nothing
		, remoteFsck = Nothing
		, repairRepo = Nothing
		, config = c
		, repo = r
		, gitconfig = gc
		, localpath = if ddarLocal ddarrepo && not (null ddarrepo)
			then Just ddarrepo
			else Nothing
		, remotetype = remote
		, availability = if ddarLocal ddarrepo then LocallyAvailable else GloballyAvailable
		, readonly = False
		}
	return $ Just $ encryptableRemote c
		(storeEncrypted new ddarrepo)
		(retrieveEncrypted ddarrepo)
		new
  where
	ddarrepo = fromMaybe (error "missing ddarrepo") $ remoteAnnexDdarRepo gc

ddarSetup :: Maybe UUID -> Maybe CredPair -> RemoteConfig -> Annex (RemoteConfig, UUID)
ddarSetup mu _ c = do
	u <- maybe (liftIO genUUID) return mu

	-- verify configuration is sane
	let ddarrepo = fromMaybe (error "Specify ddarrepo=") $
		M.lookup "ddarrepo" c
	c' <- encryptionSetup c

	-- The ddarrepo is stored in git config, as well as this repo's
	-- persistant state, so it can vary between hosts.
	gitConfigSpecialRemote u c' "ddarrepo" ddarrepo

	return (c', u)

pipeDdar :: [CommandParam] -> Maybe Handle -> Maybe Handle -> IO Bool
pipeDdar params inh outh = do
	p <- runProcess "ddar" (toCommand params)
		Nothing Nothing inh outh Nothing
	ok <- waitForProcess p
	case ok of
		ExitSuccess -> return True
		_ -> return False

store :: DdarRepo -> Key -> AssociatedFile -> MeterUpdate -> Annex Bool
store ddarrepo k _f _p = sendAnnex k (void $ remove ddarrepo k) $ \src -> do
	let params =
		[ Param "c"
		, Param "-N"
		, Param $ key2file k
		, Param ddarrepo
		, File src
		]
	liftIO $ boolSystem "ddar" params

storeEncrypted :: Remote -> DdarRepo -> (Cipher, Key) -> Key -> MeterUpdate -> Annex Bool
storeEncrypted r ddarrepo (cipher, enck) k _p =
	sendAnnex k (void $ remove ddarrepo k) $ \src ->
		liftIO $ catchBoolIO $
			encrypt (getGpgEncParams r) cipher (feedFile src) $ \h ->
				pipeDdar params (Just h) Nothing
  where
	params =
		[ Param "c"
		, Param "-N"
		, Param $ key2file enck
		, Param ddarrepo
		, Param "-"
		]

{- Convert remote DdarRepo to host and path on remote end -}
splitRemoteDdarRepo :: DdarRepo -> (String, String)
splitRemoteDdarRepo ddarrepo =
	(host, ddarrepo')
  where
	(host, remainder) = span (/= ':') ddarrepo
	ddarrepo' = drop 1 remainder

{- Return the command and parameters to use for a ddar call that may need to be
 - made on a remote repository. This will call ssh if needed. -}

ddarRemoteCall :: DdarRepo -> Char -> [CommandParam] -> Annex (String, [CommandParam])
ddarRemoteCall ddarrepo cmd params
	| ddarLocal ddarrepo = return ("ddar", localParams)
	| otherwise = do
		remoteCachingParams <- sshCachingOptions (host, Nothing) []
		return ("ssh", remoteCachingParams ++ remoteParams)
  where
	(host, ddarrepo') = splitRemoteDdarRepo ddarrepo
	localParams = Param [cmd] : Param ddarrepo : params
	remoteParams = Param host : Param "ddar" : Param [cmd] : Param ddarrepo' : params

{- Specialized ddarRemoteCall that includes extraction command and flags -}

ddarExtractRemoteCall :: DdarRepo -> Key -> Annex (String, [CommandParam])
ddarExtractRemoteCall ddarrepo k =
	ddarRemoteCall ddarrepo 'x' [Param "--force-stdout", Param $ key2file k]

retrieve :: DdarRepo -> Key -> AssociatedFile -> FilePath -> MeterUpdate -> Annex Bool
retrieve ddarrepo k _f d _p = do
	(cmd, params) <- ddarExtractRemoteCall ddarrepo k
	liftIO $ catchBoolIO $ withFile d WriteMode $ \h -> do
		let p = (proc cmd $ toCommand params){ std_out = UseHandle h }
		(_, _, _, pid) <- Common.Annex.createProcess p
		forceSuccessProcess p pid
		return True

retrieveCheap :: Key -> FilePath -> Annex Bool
retrieveCheap _ _ = return False

retrieveEncrypted :: DdarRepo -> (Cipher, Key) -> Key -> FilePath -> MeterUpdate -> Annex Bool
retrieveEncrypted ddarrepo (cipher, enck) _ f _p = do
	(cmd, params) <- ddarExtractRemoteCall ddarrepo enck
	let p = proc cmd $ toCommand params
	liftIO $ catchBoolIO $ withHandle StdoutHandle createProcessSuccess p $ \h -> do
		decrypt cipher (\toh -> L.hPut toh =<< L.hGetContents h) $
			readBytes $ L.writeFile f
		return True

remove :: DdarRepo -> Key -> Annex Bool
remove ddarrepo key = do
	(cmd, params) <- ddarRemoteCall ddarrepo 'd' [Param $ key2file key]
	liftIO $ boolSystem cmd params

ddarDirectoryExists :: DdarRepo -> Annex (Either String Bool)
ddarDirectoryExists ddarrepo
	| ddarLocal ddarrepo = do
		maybeStatus <- liftIO $ tryJust (guard . isDoesNotExistError) $ getFileStatus ddarrepo
		return $ case maybeStatus of
			Left _ -> Right False
			Right status -> Right $ isDirectory status
	| otherwise = do
		sshCachingParams <- sshCachingOptions (host, Nothing) []
		exitCode <- liftIO $ safeSystem "ssh" $ sshCachingParams ++ params
		case exitCode of
			ExitSuccess -> return $ Right True
			ExitFailure 1 -> return $ Right False
			ExitFailure code -> return $ Left $ "ssh call " ++
				show (Data.String.Utils.join " " $ toCommand params) ++
				" failed with status " ++ show code
  where
	(host, ddarrepo') = splitRemoteDdarRepo ddarrepo
	params =
		[ Param host
		, Param "test"
		, Param "-d"
		, Param ddarrepo'
		]

{- Use "ddar t" to determine if a given key is present in a ddar archive -}
inDdarManifest :: DdarRepo -> Key -> Annex (Either String Bool)
inDdarManifest ddarrepo k = do
	(cmd, params) <- ddarRemoteCall ddarrepo 't' []
	let p = proc cmd $ toCommand params
	liftIO $ catchMsgIO $ withHandle StdoutHandle createProcessSuccess p $ \h -> do
		contents <- hGetContents h
		return $ elem k' $ lines contents
  where
	k' = key2file k

checkPresent :: DdarRepo -> Key -> Annex (Either String Bool)
checkPresent ddarrepo key = do
	directoryExists <- ddarDirectoryExists ddarrepo
	case directoryExists of
		Left e -> return $ Left e
		Right True -> inDdarManifest ddarrepo key
		Right False -> return $ Right False

ddarLocal :: DdarRepo -> Bool
ddarLocal = notElem ':'
