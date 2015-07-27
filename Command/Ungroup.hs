{- git-annex command
 -
 - Copyright 2012 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Ungroup where

import Common.Annex
import Command
import qualified Remote
import Logs.Group
import Types.Group

import qualified Data.Set as S

cmd :: Command
cmd = command "ungroup" SectionSetup "remove a repository from a group"
	(paramPair paramRemote paramDesc) (withParams seek)

seek :: CmdParams -> CommandSeek
seek = withWords start

start :: [String] -> CommandStart
start (name:g:[]) = do
	showStart "ungroup" name
	u <- Remote.nameToUUID name
	next $ perform u g
start _ = error "Specify a repository and a group."

perform :: UUID -> Group -> CommandPerform
perform uuid g = do
	groupChange uuid (S.delete g) 
	next $ return True
