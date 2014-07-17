{- git-annex command
 -
 - Copyright 2014 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Reinit where

import Common.Annex
import Command
import Annex.Init
import Annex.UUID
import Types.UUID
import qualified Remote
	
def :: [Command]
def = [dontCheck repoExists $
	command "reinit" (paramUUID ++ " or " ++ paramDesc) seek SectionUtility ""]

seek :: CommandSeek
seek = withWords start

start :: [String] -> CommandStart
start ws = do
	showStart "reinit" s
	next $ perform s
  where
	s = unwords ws

perform :: String -> CommandPerform
perform s = do
	u <- if isUUID s
		then return $ toUUID s
		else Remote.nameToUUID s
	storeUUID u
	initialize'
	next $ return True
