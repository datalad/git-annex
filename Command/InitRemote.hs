{- git-annex command
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
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
import Annex.UUID
import Logs.UUID
import Logs.Trust

import Data.Ord

def :: [Command]
def = [command "initremote"
	(paramPair paramName $ paramOptional $ paramRepeating paramKeyValue)
	seek SectionSetup "sets up a special (non-git) remote"]

seek :: [CommandSeek]
seek = [withWords start]

start :: [String] -> CommandStart
start [] = do
	names <- remoteNames
	error $ "Specify a name for the remote. " ++
		if null names
			then ""
			else "Either a new name, or one of these existing special remotes: " ++ intercalate " " names
start (name:ws) = do
	(u, c) <- findByName name
	let fullconfig = config `M.union` c	
	t <- findType fullconfig

	showStart "initremote" name
	next $ perform t u name $ M.union config c

  where
	config = Logs.Remote.keyValToConfig ws

perform :: RemoteType -> UUID -> String -> R.RemoteConfig -> CommandPerform
perform t u name c = do
	c' <- R.setup t u c
	next $ cleanup u name c'

cleanup :: UUID -> String -> R.RemoteConfig -> CommandCleanup
cleanup u name c = do
	describeUUID u name
	Logs.Remote.configSet u c
	return True

{- Look up existing remote's UUID and config by name, or generate a new one -}
findByName :: String -> Annex (UUID, R.RemoteConfig)
findByName name = do
	t <- trustMap
	matches <- sortBy (comparing $ \(u, _c) -> M.lookup u t )
		. findByName' name
		<$> Logs.Remote.readRemoteLog
	maybe generate return $ headMaybe matches
  where
	generate = do
		uuid <- liftIO genUUID
		return (uuid, M.insert nameKey name M.empty)

findByName' :: String ->  M.Map UUID R.RemoteConfig -> [(UUID, R.RemoteConfig)]
findByName' n = filter (matching . snd) . M.toList
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
