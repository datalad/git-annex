{- git-annex command
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Dead where

import Common.Annex
import Command
import qualified Remote
import Logs.Trust
import Logs.Group

import qualified Data.Set as S

def :: [Command]
def = [command "dead" (paramRepeating paramRemote) seek
	SectionSetup "hide a lost repository"]

seek :: CommandSeek
seek = withWords start

start :: [String] -> CommandStart
start ws = do
	let name = unwords ws
	showStart "dead" name
	u <- Remote.nameToUUID name
	next $ perform u

perform :: UUID -> CommandPerform
perform uuid = do
	markDead uuid
	next $ return True

markDead :: UUID -> Annex ()
markDead uuid = do
	trustSet uuid DeadTrusted
	groupSet uuid S.empty
