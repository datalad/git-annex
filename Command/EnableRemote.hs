{- git-annex command
 -
 - Copyright 2013-2016 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.EnableRemote where

import Command
import qualified Annex
import qualified Logs.Remote
import qualified Types.Remote as R
import qualified Git
import qualified Git.Types as Git
import qualified Annex.SpecialRemote
import qualified Remote
import qualified Types.Remote as Remote
import qualified Remote.Git
import Logs.UUID
import Annex.UUID
import Config
import Config.DynamicConfig
import Types.GitConfig

import qualified Data.Map as M

cmd :: Command
cmd = command "enableremote" SectionSetup
	"enables git-annex to use a remote"
	(paramPair paramName $ paramOptional $ paramRepeating paramKeyValue)
	(withParams seek)

seek :: CmdParams -> CommandSeek
seek = withWords start

start :: [String] -> CommandStart
start [] = unknownNameError "Specify the remote to enable."
start (name:rest) = go =<< filter matchingname <$> Annex.fromRepo Git.remotes
  where
	matchingname r = Git.remoteName r == Just name
	go [] = startSpecialRemote name (Logs.Remote.keyValToConfig rest)
		=<< Annex.SpecialRemote.findExisting name
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
	| null restparams = do
		showStart "enableremote" name
		next $ next $ do
			setRemoteIgnore r False
			r' <- Remote.Git.configRead False r
			u <- getRepoUUID r'
			return $ u /= NoUUID
	| otherwise = giveup $
		"That is a normal git remote; passing these parameters does not make sense: " ++ unwords restparams

startSpecialRemote :: Git.RemoteName -> Remote.RemoteConfig -> Maybe (UUID, Remote.RemoteConfig) -> CommandStart
startSpecialRemote name config Nothing = do
	m <- Annex.SpecialRemote.specialRemoteMap
	confm <- Logs.Remote.readRemoteLog
	v <- Remote.nameToUUID' name
	case v of
		Right u | u `M.member` m ->
			startSpecialRemote name config $
				Just (u, fromMaybe M.empty (M.lookup u confm))
		_ -> unknownNameError "Unknown remote name."
startSpecialRemote name config (Just (u, c)) = do
	let fullconfig = config `M.union` c	
	t <- either giveup return (Annex.SpecialRemote.findType fullconfig)
	showStart "enableremote" name
	gc <- maybe (liftIO dummyRemoteGitConfig) 
		(return . Remote.gitconfig)
		=<< Remote.byUUID u
	next $ performSpecialRemote t u c fullconfig gc

performSpecialRemote :: RemoteType -> UUID -> R.RemoteConfig -> R.RemoteConfig -> RemoteGitConfig -> CommandPerform
performSpecialRemote t u oldc c gc = do
	(c', u') <- R.setup t (R.Enable oldc) (Just u) Nothing c gc
	next $ cleanupSpecialRemote u' c'

cleanupSpecialRemote :: UUID -> R.RemoteConfig -> CommandCleanup
cleanupSpecialRemote u c = do
	Logs.Remote.configSet u c
	mr <- Remote.byUUID u
	case mr of
		Nothing -> noop
		Just r -> setRemoteIgnore (R.repo r) False
	return True

unknownNameError :: String -> Annex a
unknownNameError prefix = do
	m <- Annex.SpecialRemote.specialRemoteMap
	descm <- M.unionWith Remote.addName <$> uuidMap <*> pure m
	specialmsg <- if M.null m
			then pure "(No special remotes are currently known; perhaps use initremote instead?)"
			else Remote.prettyPrintUUIDsDescs
				"known special remotes"
				descm (M.keys m)
	disabledremotes <- filterM isdisabled =<< Annex.fromRepo Git.remotes
	let remotesmsg = unlines $ map ("\t" ++) $
		mapMaybe Git.remoteName disabledremotes
	giveup $ concat $ filter (not . null) [prefix ++ "\n", remotesmsg, specialmsg]
  where
	isdisabled r = anyM id
		[ (==) NoUUID <$> getRepoUUID r
		, liftIO . getDynamicConfig . remoteAnnexIgnore
			=<< Annex.getRemoteGitConfig r
		]
