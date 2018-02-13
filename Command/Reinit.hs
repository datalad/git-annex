{- git-annex command
 -
 - Copyright 2014 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Reinit where

import Command
import Annex.Init
import Annex.UUID
import qualified Remote
import qualified Annex.SpecialRemote
	
cmd :: Command
cmd = dontCheck repoExists $
	command "reinit" SectionUtility 
		"initialize repository, reusing old UUID"
		(paramUUID ++ "|" ++ paramDesc)
		(withParams seek)

seek :: CmdParams -> CommandSeek
seek = withWords start

start :: [String] -> CommandStart
start ws = do
	showStart' "reinit" (Just s)
	next $ perform s
  where
	s = unwords ws

perform :: String -> CommandPerform
perform s = do
	u <- if isUUID s
		then return $ toUUID s
		else Remote.nameToUUID s
	storeUUID u
	initialize' (AutoInit False) Nothing
	Annex.SpecialRemote.autoEnable
	next $ return True
