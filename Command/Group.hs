{- git-annex command
 -
 - Copyright 2012 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Group where

import Command
import qualified Remote
import Logs.Group
import Types.Group

import qualified Data.Set as S

cmd :: Command
cmd = command "group" SectionSetup "add a repository to a group"
	(paramPair paramRemote paramDesc) (withParams seek)

seek :: CmdParams -> CommandSeek
seek = withWords start

start :: [String] -> CommandStart
start (name:g:[]) = do
	showStart "group" name
	u <- Remote.nameToUUID name
	next $ setGroup u g
start (name:[]) = do
	u <- Remote.nameToUUID name
	showRaw . unwords . S.toList =<< lookupGroups u
	stop
start _ = error "Specify a repository and a group."

setGroup :: UUID -> Group -> CommandPerform
setGroup uuid g = do
	groupChange uuid (S.insert g) 
	next $ return True
