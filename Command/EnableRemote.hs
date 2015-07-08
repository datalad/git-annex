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
import qualified Command.InitRemote as InitRemote

import qualified Data.Map as M

cmd :: Command
cmd = command "enableremote" SectionSetup
	"enables use of an existing special remote"
	(paramPair paramName $ paramOptional $ paramRepeating paramKeyValue)
	(withParams seek)

seek :: CmdParams -> CommandSeek
seek = withWords start

start :: [String] -> CommandStart
start [] = unknownNameError "Specify the name of the special remote to enable."
start (name:ws) = go =<< InitRemote.findExisting name
  where
	config = Logs.Remote.keyValToConfig ws
	
	go Nothing = unknownNameError "Unknown special remote name."
	go (Just (u, c)) = do
		let fullconfig = config `M.union` c	
		t <- InitRemote.findType fullconfig

		showStart "enableremote" name
		next $ perform t u fullconfig

unknownNameError :: String -> Annex a
unknownNameError prefix = do
	names <- InitRemote.remoteNames
	error $ prefix ++ "\n" ++
		if null names
			then "(No special remotes are currently known; perhaps use initremote instead?)"
			else "Known special remotes: " ++ unwords names

perform :: RemoteType -> UUID -> R.RemoteConfig -> CommandPerform
perform t u c = do
	(c', u') <- R.setup t (Just u) Nothing c
	next $ cleanup u' c'

cleanup :: UUID -> R.RemoteConfig -> CommandCleanup
cleanup u c = do
	Logs.Remote.configSet u c
	return True
