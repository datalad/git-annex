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

import qualified System.IO.Error as IO
import qualified Control.Exception as E
import Control.Exception (throw)
import System.Console.GetOpt

import Common.Annex
import qualified Annex
import qualified Annex.Queue
import qualified Git
import qualified Git.Command
import Annex.Content
import Command

type Params = [String]
type Flags = [Annex ()]

{- Runs the passed command line. -}
dispatch :: Params -> [Command] -> [Option] -> String -> IO Git.Repo -> IO ()
dispatch args cmds commonoptions header getgitrepo = do
	setupConsole
	r <- E.try getgitrepo :: IO (Either E.SomeException Git.Repo)
	case r of
		Left e -> fromMaybe (throw e) (cmdnorepo cmd)
		Right g -> do
			state <- Annex.new g
			(actions, state') <- Annex.run state $ do
				sequence_ flags
				prepCommand cmd params
			tryRun state' cmd $ [startup] ++ actions ++ [shutdown]
	where
		(flags, cmd, params) = parseCmd args cmds commonoptions header

{- Parses command line, and returns actions to run to configure flags,
 - the Command being run, and the remaining parameters for the command. -} 
parseCmd :: Params -> [Command] -> [Option] -> String -> (Flags, Command, Params)
parseCmd argv cmds commonoptions header
	| name == Nothing = err "missing command"
	| null matches = err $ "unknown command " ++ fromJust name
	| otherwise = check $ getOpt Permute (commonoptions ++ cmdoptions cmd) args
	where
		(name, args) = findname argv []
		findname [] c = (Nothing, reverse c)
		findname (a:as) c
			| "-" `isPrefixOf` a = findname as (a:c)
			| otherwise = (Just a, reverse c ++ as)
		matches = filter (\c -> name == Just (cmdname c)) cmds
		cmd = Prelude.head matches
		check (flags, rest, []) = (flags, cmd, rest)
		check (_, _, errs) = err $ concat errs
		err msg = error $ msg ++ "\n\n" ++ usage header cmds commonoptions

{- Runs a list of Annex actions. Catches IO errors and continues
 - (but explicitly thrown errors terminate the whole command).
 -}
tryRun :: Annex.AnnexState -> Command -> [CommandCleanup] -> IO ()
tryRun = tryRun' 0
tryRun' :: Integer -> Annex.AnnexState -> Command -> [CommandCleanup] -> IO ()
tryRun' errnum _ cmd []
	| errnum > 0 = error $ cmdname cmd ++ ": " ++ show errnum ++ " failed"
	| otherwise = return ()
tryRun' errnum state cmd (a:as) = run >>= handle
	where
		run = IO.try $ Annex.run state $ do
			Annex.Queue.flushWhenFull
			a
		handle (Left err) = showerr err >> cont False state
		handle (Right (success, state')) = cont success state'
		cont success s = tryRun' (if success then errnum else errnum + 1) s cmd as
		showerr err = Annex.eval state $ do
			showErr err
			showEndFail

{- Actions to perform each time ran. -}
startup :: Annex Bool
startup = return True

{- Cleanup actions. -}
shutdown :: Annex Bool
shutdown = do
	saveState
	liftIO Git.Command.reap -- zombies from long-running git processes
	return True
