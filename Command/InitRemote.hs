{- git-annex command
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.InitRemote where

import qualified Data.Map as M
import Control.Monad (when)
import Control.Monad.State (liftIO)
import Data.Maybe
import Data.String.Utils

import Command
import qualified Remote
import qualified RemoteLog
import qualified Types.Remote as R
import Types
import UUID
import Messages

command :: [Command]
command = [repoCommand "initremote"
	(paramPair paramName $
		paramOptional $ paramRepeating paramKeyValue) seek
	"sets up a special (non-git) remote"]

seek :: [CommandSeek]
seek = [withWords start]

start :: CommandStartWords
start ws = do
	when (null ws) needname

	(u, c) <- findByName name
	let fullconfig = M.union config c	
	t <- findType fullconfig

	showStart "initremote" name
	next $ perform t u $ M.union config c

	where
		name = head ws
		config = RemoteLog.keyValToConfig $ tail ws
		needname = do
			let err s = error $ "Specify a name for the remote. " ++ s
			names <- remoteNames
			if null names
				then err ""
				else err $ "Either a new name, or one of these existing special remotes: " ++ join " " names
			

perform :: R.RemoteType Annex -> UUID -> R.RemoteConfig -> CommandPerform
perform t u c = do
	c' <- R.setup t u c
	next $ cleanup u c'

cleanup :: UUID -> R.RemoteConfig -> CommandCleanup
cleanup u c = do
	RemoteLog.configSet u c
        return True

{- Look up existing remote's UUID and config by name, or generate a new one -}
findByName :: String -> Annex (UUID, R.RemoteConfig)
findByName name = do
	m <- RemoteLog.readRemoteLog
	maybe generate return $ findByName' name m
	where
		generate = do
			uuid <- liftIO genUUID
			return (uuid, M.insert nameKey name M.empty)

findByName' :: String ->  M.Map UUID R.RemoteConfig -> Maybe (UUID, R.RemoteConfig)
findByName' n m = if null matches then Nothing else Just $ head matches
	where
		matches = filter (matching . snd) $ M.toList m
		matching c = case M.lookup nameKey c of
			Nothing -> False
			Just n'
				| n' == n -> True
				| otherwise -> False

remoteNames :: Annex [String]
remoteNames = do
	m <- RemoteLog.readRemoteLog
	return $ mapMaybe (M.lookup nameKey . snd) $ M.toList m

{- find the specified remote type -}
findType :: R.RemoteConfig -> Annex (R.RemoteType Annex)
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
