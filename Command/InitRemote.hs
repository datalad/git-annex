{- git-annex command
 -
 - Copyright 2011,2013 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.InitRemote where

import qualified Data.Map as M

import Common.Annex
import Command
import qualified Remote
import qualified Logs.Remote
import qualified Types.Remote as R
import Logs.UUID
import Logs.Trust

import Data.Ord

cmd :: Command
cmd = command "initremote" SectionSetup
	"creates a special (non-git) remote"
	(paramPair paramName $ paramOptional $ paramRepeating paramKeyValue)
	(withParams seek)

seek :: CmdParams -> CommandSeek
seek = withWords start

start :: [String] -> CommandStart
start [] = error "Specify a name for the remote."
start (name:ws) = ifM (isJust <$> findExisting name)
	( error $ "There is already a special remote named \"" ++ name ++
		"\". (Use enableremote to enable an existing special remote.)"
	, do
		ifM (isJust <$> Remote.byNameOnly name)
			( error $ "There is already a remote named \"" ++ name ++ "\""
			, do
				let c = newConfig name
				t <- findType config

				showStart "initremote" name
				next $ perform t name $ M.union config c
			)
	)
  where
	config = Logs.Remote.keyValToConfig ws

perform :: RemoteType -> String -> R.RemoteConfig -> CommandPerform
perform t name c = do
	(c', u) <- R.setup t Nothing Nothing c
	next $ cleanup u name c'

cleanup :: UUID -> String -> R.RemoteConfig -> CommandCleanup
cleanup u name c = do
	describeUUID u name
	Logs.Remote.configSet u c
	return True

{- See if there's an existing special remote with this name. -}
findExisting :: String -> Annex (Maybe (UUID, R.RemoteConfig))
findExisting name = do
	t <- trustMap
	matches <- sortBy (comparing $ \(u, _c) -> M.lookup u t )
		. findByName name
		<$> Logs.Remote.readRemoteLog
	return $ headMaybe matches

newConfig :: String -> R.RemoteConfig
newConfig = M.singleton nameKey

findByName :: String ->  M.Map UUID R.RemoteConfig -> [(UUID, R.RemoteConfig)]
findByName n = filter (matching . snd) . M.toList
  where
	matching c = case M.lookup nameKey c of
		Nothing -> False
		Just n'
			| n' == n -> True
			| otherwise -> False

remoteNames :: Annex [String]
remoteNames = do
	m <- Logs.Remote.readRemoteLog
	return $ mapMaybe (M.lookup nameKey . snd) $ M.toList m

{- find the specified remote type -}
findType :: R.RemoteConfig -> Annex RemoteType
findType config = maybe unspecified specified $ M.lookup typeKey config
  where
	unspecified = error "Specify the type of remote with type="
	specified s = case filter (findtype s) Remote.remoteTypes of
		[] -> error $ "Unknown remote type " ++ s
		(t:_) -> return t
	findtype s i = R.typename i == s

{- The name of a configured remote is stored in its config using this key. -}
nameKey :: String
nameKey = "name"

{- The type of a remote is stored in its config using this key. -}
typeKey :: String
typeKey = "type"
