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
import qualified Git
import Config
import Annex.Content
import Remote.Helper.Special
import Remote.Helper.Encryptable
import Crypto

remote :: RemoteType
remote = RemoteType {
	typename = "hook",
	enumerate = findSpecialRemotes "hooktype",
	generate = gen,
	setup = hookSetup
}

gen :: Git.Repo -> UUID -> Maybe RemoteConfig -> Annex Remote
gen r u c = do
	hooktype <- getRemoteConfig r "hooktype" (error "missing hooktype")
	cst <- remoteCost r expensiveRemoteCost
	return $ encryptableRemote c
		(storeEncrypted hooktype)
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
			config = Nothing,
			repo = r,
			remotetype = remote
		}

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
		mergeenv l = M.toList .
			M.union (M.fromList l) 
				<$> M.fromList <$> getEnvironment
		env s v = ("ANNEX_" ++ s, v)
		keyenv =
			[ env "KEY" (show k)
			, env "HASH_1" (hashbits !! 0)
			, env "HASH_2" (hashbits !! 1)
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
			ifM (liftIO $
				boolSystemEnv "sh" [Param "-c", Param command]
					=<< hookEnv k f)
				( a
				, do
					warning $ hook ++ " hook exited nonzero!"
					return False
				)

store :: String -> Key -> AssociatedFile -> Annex Bool
store h k _f = do
	src <- inRepo $ gitAnnexLocation k
	runHook h "store" k (Just src) $ return True

storeEncrypted :: String -> (Cipher, Key) -> Key -> Annex Bool
storeEncrypted h (cipher, enck) k = withTmp enck $ \tmp -> do
	src <- inRepo $ gitAnnexLocation k
	liftIO $ withEncryptedContent cipher (L.readFile src) $ L.writeFile tmp
	runHook h "store" enck (Just tmp) $ return True

retrieve :: String -> Key -> AssociatedFile -> FilePath -> Annex Bool
retrieve h k _f d = runHook h "retrieve" k (Just d) $ return True

retrieveCheap :: String -> Key -> FilePath -> Annex Bool
retrieveCheap _ _ _ = return False

retrieveEncrypted :: String -> (Cipher, Key) -> Key -> FilePath -> Annex Bool
retrieveEncrypted h (cipher, enck) _ f = withTmp enck $ \tmp ->
	runHook h "retrieve" enck (Just tmp) $ liftIO $ catchBoolIO $ do
		withDecryptedContent cipher (L.readFile tmp) $ L.writeFile f
		return True

remove :: String -> Key -> Annex Bool
remove h k = runHook h "remove" k Nothing $ return True

checkPresent :: Git.Repo -> String -> Key -> Annex (Either String Bool)
checkPresent r h k = do
	showAction $ "checking " ++ Git.repoDescribe r
	v <- lookupHook h "checkpresent"
	liftIO $ catchMsgIO $ check v
	where
		findkey s = show k `elem` lines s
		check Nothing = error "checkpresent hook misconfigured"
		check (Just hook) = do
			env <- hookEnv k Nothing
			findkey <$> readProcessEnv "sh" ["-c", hook] env
