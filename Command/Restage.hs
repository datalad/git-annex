{- git-annex command
 -
 - Copyright 2022 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Command.Restage where

import Command
import qualified Annex
import Annex.Link

cmd :: Command
cmd = command "restage" SectionPlumbing 
	"estages unlocked files in the git index"
	paramNothing (withParams seek)

seek :: CmdParams -> CommandSeek
seek = withNothing (commandAction start)

start :: CommandStart
start = starting "restage" (ActionItemOther Nothing) (SeekInput []) $ do
	restagePointerFiles =<< Annex.gitRepo
	next $ return True
