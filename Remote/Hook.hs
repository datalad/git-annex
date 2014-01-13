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
import Annex.UUID
import Remote.Helper.Special
import Remote.Helper.Encryptable
import Crypto
import Utility.Metered

type Action = String
type HookName = String

remote :: RemoteType
remote = RemoteType {
	typename = "hook",
	enumerate = findSpecialRemotes "hooktype",
	generate = gen,
	setup = hookSetup
}

gen :: Git.Repo -> UUID -> RemoteConfig -> RemoteGitConfig -> Annex (Maybe Remote)
gen r u c gc = do
	cst <- remoteCost gc expensiveRemoteCost
	return $ Just $ encryptableRemote c
		(storeEncrypted hooktype $ getGpgEncParams (c,gc))
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
			remoteFsck = Nothing,
			repairRepo = Nothing,
			config = c,
			localpath = Nothing,
			repo = r,
			gitconfig = gc,
			readonly = False,
			availability = GloballyAvailable,
			remotetype = remote
		}
  where
	hooktype = fromMaybe (error "missing hooktype") $ remoteAnnexHookType gc

hookSetup :: Maybe UUID -> RemoteConfig -> Annex (RemoteConfig, UUID)
hookSetup mu c = do
	u <- maybe (liftIO genUUID) return mu
	let hooktype = fromMaybe (error "Specify hooktype=") $
		M.lookup "hooktype" c
	c' <- encryptionSetup c
	gitConfigSpecialRemote u c' "hooktype" hooktype
	return (c', u)

hookEnv :: Action -> Key -> Maybe FilePath -> IO (Maybe [(String, String)])
hookEnv action k f = Just <$> mergeenv (fileenv f ++ keyenv)
  where
	mergeenv l = M.toList . M.union (M.fromList l) 
		<$> M.fromList <$> getEnvironment
	env s v = ("ANNEX_" ++ s, v)
	keyenv = catMaybes
		[ Just $ env "KEY" (key2file k)
		, Just $ env "ACTION" action
		, env "HASH_1" <$> headMaybe hashbits
		, env "HASH_2" <$> headMaybe (drop 1 hashbits)
		]
	fileenv Nothing = []
	fileenv (Just file) =  [env "FILE" file]
	hashbits = map takeDirectory $ splitPath $ hashDirMixed k

lookupHook :: HookName -> Action -> Annex (Maybe String)
lookupHook hookname action = do
	command <- getConfig (annexConfig hook) ""
	if null command
		then do
			fallback <- getConfig (annexConfig hookfallback) ""
			if null fallback
				then do
					warning $ "missing configuration for " ++ hook ++ " or " ++ hookfallback
					return Nothing
				else return $ Just fallback
		else return $ Just command
  where
	hook = hookname ++ "-" ++ action ++ "-hook"
	hookfallback = hookname ++ "-hook"

runHook :: HookName -> Action -> Key -> Maybe FilePath -> Annex Bool -> Annex Bool
runHook hook action k f a = maybe (return False) run =<< lookupHook hook action
  where
	run command = do
		showOutput -- make way for hook output
		ifM (liftIO $ boolSystemEnv "sh" [Param "-c", Param command] =<< hookEnv action k f)
			( a
			, do
				warning $ hook ++ " hook exited nonzero!"
				return False
			)

store :: HookName -> Key -> AssociatedFile -> MeterUpdate -> Annex Bool
store h k _f _p = sendAnnex k (void $ remove h k) $ \src ->
	runHook h "store" k (Just src) $ return True

storeEncrypted :: HookName -> [CommandParam] -> (Cipher, Key) -> Key -> MeterUpdate -> Annex Bool
storeEncrypted h gpgOpts (cipher, enck) k _p = withTmp enck $ \tmp ->
	sendAnnex k (void $ remove h enck) $ \src -> do
		liftIO $ encrypt gpgOpts cipher (feedFile src) $
			readBytes $ L.writeFile tmp
		runHook h "store" enck (Just tmp) $ return True

retrieve :: HookName -> Key -> AssociatedFile -> FilePath -> MeterUpdate -> Annex Bool
retrieve h k _f d _p = runHook h "retrieve" k (Just d) $ return True

retrieveCheap :: HookName -> Key -> FilePath -> Annex Bool
retrieveCheap _ _ _ = return False

retrieveEncrypted :: HookName -> (Cipher, Key) -> Key -> FilePath -> MeterUpdate -> Annex Bool
retrieveEncrypted h (cipher, enck) _ f _p = withTmp enck $ \tmp ->
	runHook h "retrieve" enck (Just tmp) $ liftIO $ catchBoolIO $ do
		decrypt cipher (feedFile tmp) $
			readBytes $ L.writeFile f
		return True

remove :: HookName -> Key -> Annex Bool
remove h k = runHook h "remove" k Nothing $ return True

checkPresent :: Git.Repo -> HookName -> Key -> Annex (Either String Bool)
checkPresent r h k = do
	showAction $ "checking " ++ Git.repoDescribe r
	v <- lookupHook h action
	liftIO $ catchMsgIO $ check v
  where
  	action = "checkpresent"
	findkey s = key2file k `elem` lines s
	check Nothing = error $ action ++ " hook misconfigured"
	check (Just hook) = do
		env <- hookEnv action k Nothing
		findkey <$> readProcessEnv "sh" ["-c", hook] env
