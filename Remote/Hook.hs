{- A remote that provides hooks to run shell commands.
 -
 - Copyright 2011-2020 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE RankNTypes #-}

module Remote.Hook (remote) where

import Annex.Common
import Types.Remote
import Types.Creds
import qualified Git
import Git.Types (fromConfigKey, fromConfigValue)
import Config
import Config.Cost
import Annex.UUID
import Annex.SpecialRemote.Config
import Remote.Helper.Special
import Remote.Helper.ExportImport
import Utility.Env
import Messages.Progress
import Types.ProposedAccepted

import qualified Data.Map as M

type Action = String
type HookName = String

remote :: RemoteType
remote = specialRemoteType $ RemoteType
	{ typename = "hook"
	, enumerate = const (findSpecialRemotes "hooktype")
	, generate = gen
	, configParser = mkRemoteConfigParser
		[ optionalStringParser hooktypeField
			(FieldDesc "(required) specify collection of hooks to use")
		]
	, setup = hookSetup
	, exportSupported = exportUnsupported
	, importSupported = importUnsupported
	, thirdPartyPopulated = False
	}

hooktypeField :: RemoteConfigField
hooktypeField = Accepted "hooktype"

gen :: Git.Repo -> UUID -> RemoteConfig -> RemoteGitConfig -> RemoteStateHandle -> Annex (Maybe Remote)
gen r u rc gc rs = do
	c <- parsedRemoteConfig remote rc
	cst <- remoteCost gc c expensiveRemoteCost
	return $ Just $ specialRemote c
		(store hooktype)
		(retrieve hooktype)
		(remove hooktype)
		(checkKey hooktype)
		Remote
			{ uuid = u
			, cost = cst
			, name = Git.repoDescribe r
			, storeKey = storeKeyDummy
			, retrieveKeyFile = retrieveKeyFileDummy
			, retrieveKeyFileCheap = Nothing
			-- A hook could use http and be vulnerable to
			-- redirect to file:// attacks, etc.
			, retrievalSecurityPolicy = mkRetrievalVerifiableKeysSecure gc
			, removeKey = removeKeyDummy
			, lockContent = Nothing
			, checkPresent = checkPresentDummy
			, checkPresentCheap = False
			, exportActions = exportUnsupported
			, importActions = importUnsupported
			, whereisKey = Nothing
			, remoteFsck = Nothing
			, repairRepo = Nothing
			, config = c
			, localpath = Nothing
			, getRepo = return r
			, gitconfig = gc
			, readonly = False
			, appendonly = False
			, untrustworthy = False
			, availability = pure GloballyAvailable
			, remotetype = remote
			, mkUnavailable = gen r u rc
				(gc { remoteAnnexHookType = Just "!dne!" })
				rs
			, getInfo = return [("hooktype", hooktype)]
			, claimUrl = Nothing
			, checkUrl = Nothing
			, remoteStateHandle = rs
			}
  where
	hooktype = fromMaybe (giveup "missing hooktype") $ remoteAnnexHookType gc

hookSetup :: SetupStage -> Maybe UUID -> Maybe CredPair -> RemoteConfig -> RemoteGitConfig -> Annex (RemoteConfig, UUID)
hookSetup _ mu _ c gc = do
	u <- maybe (liftIO genUUID) return mu
	let hooktype = maybe (giveup "Specify hooktype=") fromProposedAccepted $
		M.lookup hooktypeField c
	(c', _encsetup) <- encryptionSetup c gc
	gitConfigSpecialRemote u c' [("hooktype", hooktype)]
	return (c', u)

hookEnv :: Action -> Key -> Maybe FilePath -> IO (Maybe [(String, String)])
hookEnv action k f = Just <$> mergeenv (fileenv f ++ keyenv)
  where
	mergeenv l = addEntries l <$> getEnvironment
	envvar s v = ("ANNEX_" ++ s, v)
	keyenv = catMaybes
		[ Just $ envvar "KEY" (serializeKey k)
		, Just $ envvar "ACTION" action
		, envvar "HASH_1" <$> headMaybe hashbits
		, envvar "HASH_2" <$> headMaybe (drop 1 hashbits)
		]
	fileenv Nothing = []
	fileenv (Just file) =  [envvar "FILE" file]
	hashbits = map takeDirectory $ splitPath $
		fromRawFilePath $ hashDirMixed def k

lookupHook :: HookName -> Action -> Annex (Maybe String)
lookupHook hookname action = do
	command <- fromConfigValue <$> getConfig hook mempty
	if null command
		then do
			fallback <- fromConfigValue <$> getConfig hookfallback mempty
			if null fallback
				then do
					warning $ UnquotedString $ "missing configuration for " ++ fromConfigKey hook ++ " or " ++ fromConfigKey hookfallback
					return Nothing
				else return $ Just fallback
		else return $ Just command
  where
	hook = annexConfig $ encodeBS $ hookname ++ "-" ++ action ++ "-hook"
	hookfallback = annexConfig $ encodeBS $ hookname ++ "-hook"

runHook :: HookName -> Action -> Key -> Maybe FilePath -> Annex ()
runHook hook action k f = lookupHook hook action >>= \case
	Just command -> do
		showOutput -- make way for hook output
		environ <- liftIO (hookEnv action k f)
		unlessM (progressCommandEnv "sh" [Param "-c", Param command] environ) $
			giveup $ hook ++ " hook exited nonzero!"
	Nothing -> giveup $ action ++ " hook misconfigured"

runHook' :: HookName -> Action -> Key -> Maybe FilePath -> Annex Bool -> Annex Bool
runHook' hook action k f a = maybe (return False) run =<< lookupHook hook action
  where
	run command = do
		showOutput -- make way for hook output
		ifM (progressCommandEnv "sh" [Param "-c", Param command] =<< liftIO (hookEnv action k f))
			( a
			, do
				warning $ UnquotedString $ hook ++ " hook exited nonzero!"
				return False
			)

store :: HookName -> Storer
store h = fileStorer $ \k src _p -> runHook h "store" k (Just src)

retrieve :: HookName -> Retriever
retrieve h = fileRetriever $ \d k _p ->
	unlessM (runHook' h "retrieve" k (Just (fromRawFilePath d)) $ return True) $
		giveup "failed to retrieve content"

remove :: HookName -> Remover
remove h _proof k = 
	unlessM (runHook' h "remove" k Nothing $ return True) $
		giveup "failed to remove content"

checkKey :: HookName -> CheckPresent
checkKey h k = do
	v <- lookupHook h action
	liftIO $ check v
  where
	action = "checkpresent"
	findkey s = serializeKey k `elem` lines s
	check Nothing = giveup $ action ++ " hook misconfigured"
	check (Just hook) = do
		environ <- hookEnv action k Nothing
		findkey <$> readProcessEnv "sh" ["-c", hook] environ
