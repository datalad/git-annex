{- git-annex command
 -
 - Copyright 2010 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Init where

import Common.Annex
import Command
import Annex.Init
	
cmd :: Command
cmd = dontCheck repoExists $
	command "init" SectionSetup "initialize git-annex"
		paramDesc (withParams seek)

seek :: CmdParams -> CommandSeek
seek = withWords start

start :: [String] -> CommandStart
start ws = do
	showStart "init" description
	next $ perform description
  where
	description = unwords ws

perform :: String -> CommandPerform
perform description = do
	initialize $ if null description then Nothing else Just description
	next $ return True
