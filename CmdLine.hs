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
import Control.Monad.State (liftIO)
import Control.Monad (when)

import qualified Annex
import qualified AnnexQueue
import qualified Git
import Content
import Types
import Command
import Version
import Options
import Messages
import UUID

{- Runs the passed command line. -}
dispatch :: [String] -> [Command] -> [Option] -> String -> Git.Repo -> IO ()
dispatch args cmds options header gitrepo = do
	setupConsole
	state <- Annex.new gitrepo
	(actions, state') <- Annex.run state $ parseCmd args header cmds options
	tryRun state' $ [startup] ++ actions ++ [shutdown]

{- Parses command line, stores configure flags, and returns a 
 - list of actions to be run in the Annex monad. -}
parseCmd :: [String] -> String -> [Command] -> [Option] -> Annex [Annex Bool]
parseCmd argv header cmds options = do
	(flags, params) <- liftIO getopt
	when (null params) $ error $ "missing command" ++ usagemsg
	case lookupCmd (head params) of
		[] -> error $ "unknown command" ++ usagemsg
		[command] -> do
			_ <- sequence flags
			when (cmdusesrepo command) checkVersion
			prepCommand command (drop 1 params)
		_ -> error "internal error: multiple matching commands"
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
tryRun :: Annex.AnnexState -> [Annex Bool] -> IO ()
tryRun = tryRun' 0
tryRun' :: Integer -> Annex.AnnexState -> [Annex Bool] -> IO ()
tryRun' errnum state (a:as) = do
	result <- try $ Annex.run state $ do
		AnnexQueue.flushWhenFull
		a
	case result of
		Left err -> do
			Annex.eval state $ do
				showEndFail
				showErr err
			tryRun' (errnum + 1) state as
		Right (True,state') -> tryRun' errnum state' as
		Right (False,state') -> tryRun' (errnum + 1) state' as
tryRun' errnum _ [] = when (errnum > 0) $ error $ show errnum ++ " failed"

{- Actions to perform each time ran. -}
startup :: Annex Bool
startup = do
	prepUUID
	return True

{- Cleanup actions. -}
shutdown :: Annex Bool
shutdown = do
	saveState
	liftIO Git.reap
	return True
