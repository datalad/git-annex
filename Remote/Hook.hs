{- A remote that provides hooks to run shell commands.
 -
 - Copyright 2011 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Remote.Hook (remote) where

import Annex.Common
import Types.Remote
import Types.Creds
import qualified Git
import Config
import Config.Cost
import Annex.UUID
import Remote.Helper.Special
import Remote.Helper.Messages
import Utility.Env
import Messages.Progress

import qualified Data.Map as M

type Action = String
type HookName = String

remote :: RemoteType
remote = RemoteType {
	typename = "hook",
	enumerate = const (findSpecialRemotes "hooktype"),
	generate = gen,
	setup = hookSetup
}

gen :: Git.Repo -> UUID -> RemoteConfig -> RemoteGitConfig -> Annex (Maybe Remote)
gen r u c gc = do
	cst <- remoteCost gc expensiveRemoteCost
	return $ Just $ specialRemote c
		(simplyPrepare $ store hooktype)
		(simplyPrepare $ retrieve hooktype)
		(simplyPrepare $ remove hooktype)
		(simplyPrepare $ checkKey r hooktype)
		Remote
			{ uuid = u
			, cost = cst
			, name = Git.repoDescribe r
			, storeKey = storeKeyDummy
			, retrieveKeyFile = retreiveKeyFileDummy
			, retrieveKeyFileCheap = retrieveCheap hooktype
			, removeKey = removeKeyDummy
			, lockContent = Nothing
			, checkPresent = checkPresentDummy
			, checkPresentCheap = False
			, whereisKey = Nothing
			, remoteFsck = Nothing
			, repairRepo = Nothing
			, config = c
			, localpath = Nothing
			, repo = r
			, gitconfig = gc
			, readonly = False
			, availability = GloballyAvailable
			, remotetype = remote
			, mkUnavailable = gen r u c $
				gc { remoteAnnexHookType = Just "!dne!" }
			, getInfo = return [("hooktype", hooktype)]
			, claimUrl = Nothing
			, checkUrl = Nothing
			}
  where
	hooktype = fromMaybe (giveup "missing hooktype") $ remoteAnnexHookType gc

hookSetup :: Maybe UUID -> Maybe CredPair -> RemoteConfig -> RemoteGitConfig -> Annex (RemoteConfig, UUID)
hookSetup mu _ c gc = do
	u <- maybe (liftIO genUUID) return mu
	let hooktype = fromMaybe (giveup "Specify hooktype=") $
		M.lookup "hooktype" c
	(c', _encsetup) <- encryptionSetup c gc
	gitConfigSpecialRemote u c' "hooktype" hooktype
	return (c', u)

hookEnv :: Action -> Key -> Maybe FilePath -> IO (Maybe [(String, String)])
hookEnv action k f = Just <$> mergeenv (fileenv f ++ keyenv)
  where
	mergeenv l = addEntries l <$> getEnvironment
	envvar s v = ("ANNEX_" ++ s, v)
	keyenv = catMaybes
		[ Just $ envvar "KEY" (key2file k)
		, Just $ envvar "ACTION" action
		, envvar "HASH_1" <$> headMaybe hashbits
		, envvar "HASH_2" <$> headMaybe (drop 1 hashbits)
		]
	fileenv Nothing = []
	fileenv (Just file) =  [envvar "FILE" file]
	hashbits = map takeDirectory $ splitPath $ hashDirMixed def k

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
		ifM (progressCommandEnv "sh" [Param "-c", Param command] =<< liftIO (hookEnv action k f))
			( a
			, do
				warning $ hook ++ " hook exited nonzero!"
				return False
			)

store :: HookName -> Storer
store h = fileStorer $ \k src _p ->
	runHook h "store" k (Just src) $ return True

retrieve :: HookName -> Retriever
retrieve h = fileRetriever $ \d k _p ->
	unlessM (runHook h "retrieve" k (Just d) $ return True) $
		giveup "failed to retrieve content"

retrieveCheap :: HookName -> Key -> AssociatedFile -> FilePath -> Annex Bool
retrieveCheap _ _ _ _ = return False

remove :: HookName -> Remover
remove h k = runHook h "remove" k Nothing $ return True

checkKey :: Git.Repo -> HookName -> CheckPresent
checkKey r h k = do
	showChecking r
	v <- lookupHook h action
	liftIO $ check v
  where
	action = "checkpresent"
	findkey s = key2file k `elem` lines s
	check Nothing = giveup $ action ++ " hook misconfigured"
	check (Just hook) = do
		environ <- hookEnv action k Nothing
		findkey <$> readProcessEnv "sh" ["-c", hook] environ
