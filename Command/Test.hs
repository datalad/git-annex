{- git-annex command
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Test where

import Common.Annex
import Command
import qualified Command.Init
import qualified Command.Add
import qualified Command.Drop
import qualified Command.Get
import qualified Command.Move
import qualified Command.Copy
import qualified Command.Sync
import qualified Command.Whereis
import qualified Command.Fsck
import qualified Test

def :: [Command]
def = [noCommit $ noRepo showHelp $ dontCheck repoExists $
	command "test" paramNothing seek "run built-in test suite"]

seek :: [CommandSeek]
seek = [withWords start]

start :: [String] -> CommandStart
start _ = do
	liftIO $ Test.main
	stop
