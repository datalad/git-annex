{- git-annex command
 -
 - Copyright 2013-2020 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Command.EnableRemote where

import Command
import qualified Annex
import qualified Logs.Remote
import qualified Types.Remote as R
import qualified Git
import qualified Git.Types as Git
import qualified Annex.SpecialRemote as SpecialRemote
import qualified Remote
import qualified Types.Remote as Remote
import qualified Remote.Git
import Logs.UUID
import Annex.UUID
import Config
import Config.DynamicConfig
import Types.GitConfig
import Types.ProposedAccepted
import Git.Config

import qualified Data.Map as M

cmd :: Command
cmd = command "enableremote" SectionSetup
	"enables git-annex to use a remote"
	(paramPair paramName $ paramOptional $ paramRepeating paramParamValue)
	(withParams seek)

seek :: CmdParams -> CommandSeek
seek = withWords (commandAction . start)

start :: [String] -> CommandStart
start [] = unknownNameError "Specify the remote to enable."
start (name:rest) = go =<< filter matchingname <$> Annex.getGitRemotes
  where
	matchingname r = Git.remoteName r == Just name
	go [] = 
		let use = startSpecialRemote name (Logs.Remote.keyValToConfig Proposed rest)
		in SpecialRemote.findExisting' name >>= \case
			-- enable dead remote only when there is no
			-- other remote with the same name
			([], l) -> use l
			(l, _) -> use l
	go (r:_) = do
		-- This could be either a normal git remote or a special
		-- remote that has an url (eg gcrypt).
		rs <- Remote.remoteList
		case filter (\rmt -> Remote.name rmt == name) rs of
			(rmt:_) | Remote.remotetype rmt == Remote.Git.remote ->
				startNormalRemote name rest r
			_  -> go []

-- Normal git remotes are special-cased; enableremote retries probing
-- the remote uuid.
startNormalRemote :: Git.RemoteName -> [String] -> Git.Repo -> CommandStart
startNormalRemote name restparams r
	| null restparams = starting "enableremote" ai si $ do
		setRemoteIgnore r False
		r' <- Remote.Git.configRead False r
		u <- getRepoUUID r'
		next $ return $ u /= NoUUID
	| otherwise = giveup $
		"That is a normal git remote; passing these parameters does not make sense: " ++ unwords restparams
  where
	ai = ActionItemOther (Just (UnquotedString name))
	si = SeekInput [name]

startSpecialRemote :: Git.RemoteName -> Remote.RemoteConfig -> [(UUID, Remote.RemoteConfig, Maybe (SpecialRemote.ConfigFrom UUID))] -> CommandStart
startSpecialRemote name config [] = do
	m <- SpecialRemote.specialRemoteMap
	confm <- Logs.Remote.remoteConfigMap
	Remote.nameToUUID' name >>= \case
		([u], _) | u `M.member` m ->
			startSpecialRemote name config $
				[(u, fromMaybe M.empty (M.lookup u confm), Nothing)]
		(_, msg) -> unknownNameError msg
startSpecialRemote name config ((u, c, mcu):[]) =
	starting "enableremote" ai si $ do
		let fullconfig = config `M.union` c	
		t <- either giveup return (SpecialRemote.findType fullconfig)
		gc <- maybe (liftIO dummyRemoteGitConfig) 
			(return . Remote.gitconfig)
			=<< Remote.byUUID u
		performSpecialRemote t u c fullconfig gc mcu
  where
	ai = ActionItemOther (Just (UnquotedString name))
	si = SeekInput [name]
startSpecialRemote _ _ _ =
	giveup "Multiple remotes have that name. Either use git-annex renameremote to rename them, or specify the uuid of the remote to enable."

performSpecialRemote :: RemoteType -> UUID -> R.RemoteConfig -> R.RemoteConfig -> RemoteGitConfig -> Maybe (SpecialRemote.ConfigFrom UUID) -> CommandPerform
performSpecialRemote t u oldc c gc mcu = do
	(c', u') <- R.setup t (R.Enable oldc) (Just u) Nothing c gc
	next $ cleanupSpecialRemote t u' c' mcu

cleanupSpecialRemote :: RemoteType -> UUID -> R.RemoteConfig -> Maybe (SpecialRemote.ConfigFrom UUID) -> CommandCleanup
cleanupSpecialRemote t u c mcu = do
	case mcu of
		Nothing -> 
			Logs.Remote.configSet u c
		Just (SpecialRemote.ConfigFrom cu) -> do
			setConfig (remoteAnnexConfig c "config-uuid") (fromUUID cu)
			Logs.Remote.configSet cu c
	Remote.byUUID u >>= \case
		Nothing -> noop
		Just r -> do
			repo <- R.getRepo r
			setRemoteIgnore repo False
	unless (Remote.gitSyncableRemoteType t) $
		setConfig (remoteConfig c "skipFetchAll") (boolConfig True)
	return True

unknownNameError :: String -> Annex a
unknownNameError prefix = do
	m <- SpecialRemote.specialRemoteMap
	descm <- M.unionWith Remote.addName
		<$> uuidDescMap
		<*> pure (M.map toUUIDDesc m)
	specialmsg <- if M.null m
			then pure "(No special remotes are currently known; perhaps use initremote instead?)"
			else Remote.prettyPrintUUIDsDescs
				"known special remotes"
				descm (M.keys m)
	disabledremotes <- filterM isdisabled =<< Annex.getGitRemotes
	let remotesmsg = unlines $ map ("\t" ++) $
		mapMaybe Git.remoteName disabledremotes
	giveup $ concat $ filter (not . null) [prefix ++ "\n", remotesmsg, specialmsg]
  where
	isdisabled r = anyM id
		[ (==) NoUUID <$> getRepoUUID r
		, liftIO . getDynamicConfig . remoteAnnexIgnore
			=<< Annex.getRemoteGitConfig r
		]
