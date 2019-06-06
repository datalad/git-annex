{- git-annex command
 -
 - Copyright 2011 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Command.Describe where

import Command
import qualified Remote
import Logs.UUID

cmd :: Command
cmd = command "describe" SectionSetup
	"change description of a repository"
	(paramPair paramRemote paramDesc)
	(withParams seek)

seek :: CmdParams -> CommandSeek
seek = withWords (commandAction . start)

start :: [String] -> CommandStart
start (name:description) | not (null description) = do
	u <- Remote.nameToUUID name
	starting "describe" (ActionItemOther (Just name)) $
		perform u $ unwords description
start _ = giveup "Specify a repository and a description."	

perform :: UUID -> String -> CommandPerform
perform u description = do
	describeUUID u (toUUIDDesc description)
	next $ return True
