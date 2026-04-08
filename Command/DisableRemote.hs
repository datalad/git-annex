{- git-annex command
 -
 - Copyright 2026 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Command.DisableRemote where

import Command
import Remote
import Annex.DisableRemote

cmd :: Command
cmd = withAnnexOptions [jsonOptions] $
	command "disableremote" SectionSetup
		"stop using a remote"
		paramName
		(withParams seek)

seek :: CmdParams -> CommandSeek
seek = withWords (commandAction . start)

start :: [String] -> CommandStart
start (remotename:[]) = byName' remotename >>= \case
	Left err -> giveup err
	Right r -> starting "disableremote" ai si $ do
		disableRemote r remotename =<< remoteList
		next $ return True
  where
	ai = ActionItemOther (Just (UnquotedString remotename))
	si = SeekInput [remotename]
start _ = giveup "Specify the remote's name."
