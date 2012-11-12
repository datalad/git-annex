{- git-annex command
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Help where

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

def :: [Command]
def = [noCommit $ noRepo showHelp $ dontCheck repoExists $
	command "help" paramNothing seek "display help"]

seek :: [CommandSeek]
seek = [withWords start]

start :: [String] -> CommandStart
start _ = do
	liftIO showHelp
	stop

showHelp :: IO ()
showHelp = liftIO $ putStrLn $ unlines
	[ "The most commonly used git-annex commands are:"
	, unlines $ map cmdline $ concat
		[ Command.Init.def
		, Command.Add.def
		, Command.Drop.def
		, Command.Get.def
		, Command.Move.def
		, Command.Copy.def
		, Command.Sync.def
		, Command.Whereis.def
		, Command.Fsck.def
		]
	, "Run git-annex without any options for a complete command and option list."
	]
  where
	cmdline c = "\t" ++ cmdname c ++ "\t" ++ cmddesc c
