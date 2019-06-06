{- git-annex command
 -
 - Copyright 2012 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Command.Ungroup where

import Command
import qualified Remote
import Logs.Group
import Types.Group

import qualified Data.Set as S

cmd :: Command
cmd = command "ungroup" SectionSetup "remove a repository from a group"
	(paramPair paramRemote paramDesc) (withParams seek)

seek :: CmdParams -> CommandSeek
seek = withWords (commandAction . start)

start :: [String] -> CommandStart
start (name:g:[]) = do
	u <- Remote.nameToUUID name
	starting "ungroup" (ActionItemOther (Just name)) $
		perform u (toGroup g)
start _ = giveup "Specify a repository and a group."

perform :: UUID -> Group -> CommandPerform
perform uuid g = do
	groupChange uuid (S.delete g) 
	next $ return True
