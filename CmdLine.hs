{- git-annex command line parsing and dispatch
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module CmdLine (
	dispatch,
	usage,
	shutdown
) where

import System.IO.Error (try)
import System.Console.GetOpt

import Common.Annex
import qualified Annex
import qualified Annex.Queue
import qualified Git
import Annex.Content
import Command

{- Runs the passed command line. -}
dispatch :: [String] -> [Command] -> [Option] -> String -> Git.Repo -> IO ()
dispatch args cmds options header gitrepo = do
	setupConsole
	state <- Annex.new gitrepo
	((cmd, actions), state') <- Annex.run state $ parseCmd args header cmds options
	tryRun state' cmd $ [startup] ++ actions ++ [shutdown]

{- Parses command line, stores configure flags, and returns a 
 - list of actions to be run in the Annex monad and the Command
 - being run. -}
parseCmd :: [String] -> String -> [Command] -> [Option] -> Annex (Command, [Annex Bool])
parseCmd argv header cmds options = do
	(flags, params) <- liftIO getopt
	when (null params) $ error $ "missing command" ++ usagemsg
	let (c:rest) = params
	case lookupCmd c of
		[] -> error $ "unknown command " ++ c ++ " " ++ usagemsg
		[cmd] -> do
			_ <- sequence flags
			checkCommand cmd
			as <- prepCommand cmd rest
			return (cmd, as)
		_ -> error $ "internal error: multiple matching commands for " ++ c
	where
		getopt = case getOpt Permute options argv of
			(flags, params, []) ->
				return (flags, params)
			(_, _, errs) ->
				ioError (userError (concat errs ++ usagemsg))
		lookupCmd cmd = filter (\c -> cmd  == cmdname c) cmds
		usagemsg = "\n\n" ++ usage header cmds options

{- Usage message with lists of commands and options. -}
usage :: String -> [Command] -> [Option] -> String
usage header cmds options =
	usageInfo (header ++ "\n\nOptions:") options ++
		"\nCommands:\n" ++ cmddescs
	where
		cmddescs = unlines $ map (indent . showcmd) cmds
		showcmd c =
			cmdname c ++
			pad (longest cmdname + 1) (cmdname c) ++
			cmdparams c ++
			pad (longest cmdparams + 2) (cmdparams c) ++
			cmddesc c
		pad n s = replicate (n - length s) ' '
		longest f = foldl max 0 $ map (length . f) cmds

{- Runs a list of Annex actions. Catches IO errors and continues
 - (but explicitly thrown errors terminate the whole command).
 -}
tryRun :: Annex.AnnexState -> Command -> [Annex Bool] -> IO ()
tryRun = tryRun' 0
tryRun' :: Integer -> Annex.AnnexState -> Command -> [Annex Bool] -> IO ()
tryRun' errnum state cmd (a:as) = do
	result <- try $ Annex.run state $ do
		Annex.Queue.flushWhenFull
		a
	case result of
		Left err -> do
			Annex.eval state $ do
				showErr err
				showEndFail
			tryRun' (errnum + 1) state cmd as
		Right (True,state') -> tryRun' errnum state' cmd as
		Right (False,state') -> tryRun' (errnum + 1) state' cmd as
tryRun' errnum _ cmd [] = when (errnum > 0) $
		error $ cmdname cmd ++ ": " ++ show errnum ++ " failed"

{- Actions to perform each time ran. -}
startup :: Annex Bool
startup = return True

{- Cleanup actions. -}
shutdown :: Annex Bool
shutdown = do
	saveState
	liftIO Git.reap -- zombies from long-running git processes
	return True
