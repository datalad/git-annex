{- A remote that provides hooks to run shell commands.
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Remote.Hook (remote) where

import qualified Data.ByteString.Lazy as L
import qualified Data.Map as M
import System.Environment

import Common.Annex
import Types.Remote
import Types.Key
import qualified Git
import Config
import Config.Cost
import Annex.Content
import Remote.Helper.Special
import Remote.Helper.Encryptable
import Crypto
import Utility.Metered

remote :: RemoteType
remote = RemoteType {
	typename = "hook",
	enumerate = findSpecialRemotes "hooktype",
	generate = gen,
	setup = hookSetup
}

gen :: Git.Repo -> UUID -> RemoteConfig -> RemoteGitConfig -> Annex Remote
gen r u c gc = do
	cst <- remoteCost gc expensiveRemoteCost
	return $ encryptableRemote c
		(storeEncrypted hooktype $ getGpgOpts gc)
		(retrieveEncrypted hooktype)
		Remote {
			uuid = u,
			cost = cst,
			name = Git.repoDescribe r,
			storeKey = store hooktype,
			retrieveKeyFile = retrieve hooktype,
			retrieveKeyFileCheap = retrieveCheap hooktype,
			removeKey = remove hooktype,
			hasKey = checkPresent r hooktype,
			hasKeyCheap = False,
			whereisKey = Nothing,
			config = M.empty,
			localpath = Nothing,
			repo = r,
			gitconfig = gc,
			readonly = False,
			globallyAvailable = False,
			remotetype = remote
		}
  where
	hooktype = fromMaybe (error "missing hooktype") $ remoteAnnexHookType gc	

hookSetup :: UUID -> RemoteConfig -> Annex RemoteConfig
hookSetup u c = do
	let hooktype = fromMaybe (error "Specify hooktype=") $
		M.lookup "hooktype" c
	c' <- encryptionSetup c
	gitConfigSpecialRemote u c' "hooktype" hooktype
	return c'

hookEnv :: Key -> Maybe FilePath -> IO (Maybe [(String, String)])
hookEnv k f = Just <$> mergeenv (fileenv f ++ keyenv)
  where
	mergeenv l = M.toList . M.union (M.fromList l) 
		<$> M.fromList <$> getEnvironment
	env s v = ("ANNEX_" ++ s, v)
	keyenv = catMaybes
		[ Just $ env "KEY" (key2file k)
		, env "HASH_1" <$> headMaybe hashbits
		, env "HASH_2" <$> headMaybe (drop 1 hashbits)
		]
	fileenv Nothing = []
	fileenv (Just file) =  [env "FILE" file]
	hashbits = map takeDirectory $ splitPath $ hashDirMixed k

lookupHook :: String -> String -> Annex (Maybe String)
lookupHook hooktype hook =do
	command <- getConfig (annexConfig hookname) ""
	if null command
		then do
			warning $ "missing configuration for " ++ hookname
			return Nothing
		else return $ Just command
  where
	hookname = hooktype ++ "-" ++ hook ++ "-hook"

runHook :: String -> String -> Key -> Maybe FilePath -> Annex Bool -> Annex Bool
runHook hooktype hook k f a = maybe (return False) run =<< lookupHook hooktype hook
  where
	run command = do
		showOutput -- make way for hook output
		ifM (liftIO $ boolSystemEnv "sh" [Param "-c", Param command] =<< hookEnv k f)
			( a
			, do
				warning $ hook ++ " hook exited nonzero!"
				return False
			)

store :: String -> Key -> AssociatedFile -> MeterUpdate -> Annex Bool
store h k _f _p = sendAnnex k (void $ remove h k) $ \src ->
	runHook h "store" k (Just src) $ return True

storeEncrypted :: String -> GpgOpts -> (Cipher, Key) -> Key -> MeterUpdate -> Annex Bool
storeEncrypted h gpgOpts (cipher, enck) k _p = withTmp enck $ \tmp ->
	sendAnnex k (void $ remove h enck) $ \src -> do
		liftIO $ encrypt gpgOpts cipher (feedFile src) $
			readBytes $ L.writeFile tmp
		runHook h "store" enck (Just tmp) $ return True

retrieve :: String -> Key -> AssociatedFile -> FilePath -> MeterUpdate -> Annex Bool
retrieve h k _f d _p = runHook h "retrieve" k (Just d) $ return True

retrieveCheap :: String -> Key -> FilePath -> Annex Bool
retrieveCheap _ _ _ = return False

retrieveEncrypted :: String -> (Cipher, Key) -> Key -> FilePath -> MeterUpdate -> Annex Bool
retrieveEncrypted h (cipher, enck) _ f _p = withTmp enck $ \tmp ->
	runHook h "retrieve" enck (Just tmp) $ liftIO $ catchBoolIO $ do
		decrypt cipher (feedFile tmp) $
			readBytes $ L.writeFile f
		return True

remove :: String -> Key -> Annex Bool
remove h k = runHook h "remove" k Nothing $ return True

checkPresent :: Git.Repo -> String -> Key -> Annex (Either String Bool)
checkPresent r h k = do
	showAction $ "checking " ++ Git.repoDescribe r
	v <- lookupHook h "checkpresent"
	liftIO $ catchMsgIO $ check v
  where
	findkey s = key2file k `elem` lines s
	check Nothing = error "checkpresent hook misconfigured"
	check (Just hook) = do
		env <- hookEnv k Nothing
		findkey <$> readProcessEnv "sh" ["-c", hook] env
