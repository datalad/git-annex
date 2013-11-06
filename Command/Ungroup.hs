{- git-annex command
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
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

def :: [Command]
def = [command "ungroup" (paramPair paramRemote paramDesc) seek
	SectionSetup "remove a repository from a group"]

seek :: [CommandSeek]
seek = [withWords start]

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
