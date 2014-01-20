{- git-annex command
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Group where

import Common.Annex
import Command
import qualified Remote
import Logs.Group
import Types.Group

import qualified Data.Set as S

def :: [Command]
def = [command "group" (paramPair paramRemote paramDesc) seek
	SectionSetup "add a repository to a group"]

seek :: CommandSeek
seek = withWords start

start :: [String] -> CommandStart
start (name:g:[]) = do
	showStart "group" name
	u <- Remote.nameToUUID name
	next $ perform u g
start _ = error "Specify a repository and a group."

perform :: UUID -> Group -> CommandPerform
perform uuid g = do
	groupChange uuid (S.insert g) 
	next $ return True
