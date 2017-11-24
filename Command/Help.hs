{- git-annex command
 -
 - Copyright 2012 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.Help where

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

cmd :: Command
cmd = noCommit $ dontCheck repoExists $
	noRepo (parseparams startNoRepo) $ 
		command "help" SectionCommon "display help"
			"COMMAND" (parseparams seek)
  where
	parseparams = withParams

seek :: CmdParams -> CommandSeek
seek = withWords start

start :: [String] -> CommandStart
start params = do
	liftIO $ start' params
	stop

startNoRepo :: CmdParams -> IO ()
startNoRepo = start'

start' :: [String] -> IO ()
start' [c] = showGitHelp c
start' _ = showGeneralHelp

showGeneralHelp :: IO ()
showGeneralHelp = putStrLn $ unlines
	[ "The most frequently used git-annex commands are:"
	, unlines $ map cmdline $
		[ Command.Init.cmd
		, Command.Add.cmd
		, Command.Drop.cmd
		, Command.Get.cmd
		, Command.Move.cmd
		, Command.Copy.cmd
		, Command.Sync.cmd
		, Command.Whereis.cmd
		, Command.Fsck.cmd
		]
	, "For a complete command list, run: git-annex"
	, "For help on a specific command, run: git-annex help COMMAND"
	]
  where
	cmdline c = "\t" ++ cmdname c ++ "\t" ++ cmddesc c

showGitHelp :: String -> IO ()
showGitHelp c = 
	unlessM (githelp) $
		putStrLn $ "View online help at " ++ url
  where
	githelp = boolSystem "git" [Param "help", Param fullc]
	fullc = "git-annex-" ++ c
	url = "https://git-annex.branchable.com/" ++ fullc ++ "/"
