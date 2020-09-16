{- git-annex command
 -
 - Copyright 2010 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Command.InAnnex where

import Command
import Annex.Content

cmd :: Command
cmd = noCommit $ 
	command "inannex" SectionPlumbing 
		"checks if keys are present in the annex"
		(paramRepeating paramKey)
		(withParams seek)

seek :: CmdParams -> CommandSeek
seek = withKeys (commandAction . start)

start :: (SeekInput, Key) -> CommandStart
start (_, key) = inAnnexSafe key >>= dispatch
  where
	dispatch (Just True) = stop
	dispatch (Just False) = exit 1
	dispatch Nothing = exit 100
	exit n = liftIO $ exitWith $ ExitFailure n
