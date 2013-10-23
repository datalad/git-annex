{- git-annex command
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Repair where

import Common.Annex
import Command
import qualified Annex
import Git.RecoverRepository (runRecovery)

def :: [Command]
def = [noCommit $ dontCheck repoExists $
	command "repair" paramNothing seek SectionMaintenance "recover broken git repository"]

seek :: [CommandSeek]
seek = [withNothing start]

start :: CommandStart
start = next $ next $ do
	force <- Annex.getState Annex.force
	inRepo $ runRecovery force
