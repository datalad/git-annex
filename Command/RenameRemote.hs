{- git-annex command
 -
 - Copyright 2019 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Command.RenameRemote where

import Command
import qualified Logs.Remote
import qualified Types.Remote as R
import qualified Remote

import qualified Data.Map as M

cmd :: Command
cmd = command "renameremote" SectionSetup
	"changes name of special remote"
	(paramPair paramName paramName)
	(withParams seek)

seek :: CmdParams -> CommandSeek
seek = withWords (commandAction . start)

start :: [String] -> CommandStart
start (oldname:newname:[]) = do
	m <- Logs.Remote.readRemoteLog
	Remote.nameToUUID' oldname >>= \case
		Left e -> giveup e
		Right u -> case M.lookup u m of
			Nothing -> giveup "That is not a special remote."
			Just cfg -> do
				showStart' "rename" Nothing
				next $ perform u cfg newname
start _ = giveup "Specify an old name (or uuid or description) and a new name."

perform :: UUID -> R.RemoteConfig -> String -> CommandPerform
perform u cfg newname = do
	Logs.Remote.configSet u (M.insert "name" newname cfg)
	next $ return True
