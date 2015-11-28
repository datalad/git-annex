{- git-annex command
 -
 - Copyright 2013 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.EnableRemote where

import Common.Annex
import Command
import qualified Logs.Remote
import qualified Types.Remote as R
import qualified Annex.SpecialRemote
import qualified Remote
import Logs.UUID

import qualified Data.Map as M

cmd :: Command
cmd = command "enableremote" SectionSetup
	"enables use of an existing special remote"
	(paramPair paramName $ paramOptional $ paramRepeating paramKeyValue)
	(withParams seek)

seek :: CmdParams -> CommandSeek
seek = withWords start

start :: [String] -> CommandStart
start [] = unknownNameError "Specify the special remote to enable."
start (name:ws) = go =<< Annex.SpecialRemote.findExisting name
  where
	config = Logs.Remote.keyValToConfig ws
	
	go Nothing = do
		m <- Annex.SpecialRemote.specialRemoteMap
		confm <- Logs.Remote.readRemoteLog
		v <- Remote.nameToUUID' name
		case v of
			Right u | u `M.member` m ->
				go (Just (u, fromMaybe M.empty (M.lookup u confm)))
			_ -> unknownNameError "Unknown special remote."
	go (Just (u, c)) = do
		let fullconfig = config `M.union` c	
		t <- either error return (Annex.SpecialRemote.findType fullconfig)
		showStart "enableremote" name
		next $ perform t u fullconfig

unknownNameError :: String -> Annex a
unknownNameError prefix = do
	m <- Annex.SpecialRemote.specialRemoteMap
	descm <- M.unionWith Remote.addName <$> uuidMap <*> pure m
	msg <- if M.null m
			then pure "(No special remotes are currently known; perhaps use initremote instead?)"
			else Remote.prettyPrintUUIDsDescs
				"known special remotes"
				descm (M.keys m)
	error $ prefix ++ "\n" ++ msg

perform :: RemoteType -> UUID -> R.RemoteConfig -> CommandPerform
perform t u c = do
	(c', u') <- R.setup t (Just u) Nothing c
	next $ cleanup u' c'

cleanup :: UUID -> R.RemoteConfig -> CommandCleanup
cleanup u c = do
	Logs.Remote.configSet u c
	return True
