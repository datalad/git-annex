{- git-annex command line parsing and dispatch
 -
 - Copyright 2010-2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module CmdLine (
	dispatch,
	usage,
	shutdown
) where

import qualified Control.Exception as E
import qualified Data.Map as M
import Control.Exception (throw)
import System.Console.GetOpt

import Common.Annex
import qualified Annex
import qualified Annex.Queue
import qualified Git
import qualified Git.Command
import qualified Git.AutoCorrect
import Annex.Content
import Annex.Ssh
import Command

type Params = [String]
type Flags = [Annex ()]

{- Runs the passed command line. -}
dispatch :: Bool -> Params -> [Command] -> [Option] -> String -> IO Git.Repo -> IO ()
dispatch fuzzyok allargs allcmds commonoptions header getgitrepo = do
	setupConsole
	r <- E.try getgitrepo :: IO (Either E.SomeException Git.Repo)
	case r of
		Left e -> fromMaybe (throw e) (cmdnorepo cmd)
		Right g -> do
			state <- Annex.new g
			(actions, state') <- Annex.run state $ do
				checkfuzzy
				sequence_ flags
				prepCommand cmd params
		 	tryRun state' cmd $ [startup] ++ actions ++ [shutdown $ cmdoneshot cmd]
	where
		err msg = msg ++ "\n\n" ++ usage header allcmds commonoptions
		cmd = Prelude.head cmds
		(fuzzy, cmds, name, args) = findCmd fuzzyok allargs allcmds err
		(flags, params) = getOptCmd args cmd commonoptions err
		checkfuzzy = when fuzzy $
			inRepo $ Git.AutoCorrect.prepare name cmdname cmds

{- Parses command line params far enough to find the Command to run, and
 - returns the remaining params.
 - Does fuzzy matching if necessary, which may result in multiple Commands. -}
findCmd :: Bool -> Params -> [Command] -> (String -> String) -> (Bool, [Command], String, Params)
findCmd fuzzyok argv cmds err
	| isNothing name = error $ err "missing command"
	| not (null exactcmds) = (False, exactcmds, fromJust name, args)
	| fuzzyok && not (null inexactcmds) = (True, inexactcmds, fromJust name, args)
	| otherwise = error $ err $ "unknown command " ++ fromJust name
	where
		(name, args) = findname argv []
		findname [] c = (Nothing, reverse c)
		findname (a:as) c
			| "-" `isPrefixOf` a = findname as (a:c)
			| otherwise = (Just a, reverse c ++ as)
		exactcmds = filter (\c -> name == Just (cmdname c)) cmds
		inexactcmds = case name of
			Nothing -> []
			Just n -> Git.AutoCorrect.fuzzymatches n cmdname cmds

{- Parses command line options, and returns actions to run to configure flags
 - and the remaining parameters for the command. -}
getOptCmd :: Params -> Command -> [Option] -> (String -> String) -> (Flags, Params)
getOptCmd argv cmd commonoptions err = check $
	getOpt Permute (commonoptions ++ cmdoptions cmd) argv
	where
		check (flags, rest, []) = (flags, rest)
		check (_, _, errs) = error $ err $ concat errs

{- Runs a list of Annex actions. Catches IO errors and continues
 - (but explicitly thrown errors terminate the whole command).
 -}
tryRun :: Annex.AnnexState -> Command -> [CommandCleanup] -> IO ()
tryRun = tryRun' 0
tryRun' :: Integer -> Annex.AnnexState -> Command -> [CommandCleanup] -> IO ()
tryRun' errnum _ cmd []
	| errnum > 0 = error $ cmdname cmd ++ ": " ++ show errnum ++ " failed"
	| otherwise = return ()
tryRun' errnum state cmd (a:as) = do
	r <- run
	handle $! r
	where
		run = tryIO $ Annex.run state $ do
			Annex.Queue.flushWhenFull
			a
		handle (Left err) = showerr err >> cont False state
		handle (Right (success, state')) = cont success state'
		cont success s = do
			let errnum' = if success then errnum else errnum + 1
			(tryRun' $! errnum') s cmd as
		showerr err = Annex.eval state $ do
			showErr err
			showEndFail

{- Actions to perform each time ran. -}
startup :: Annex Bool
startup = return True

{- Cleanup actions. -}
shutdown :: Bool -> Annex Bool
shutdown oneshot = do
	saveState oneshot
	sequence_ =<< M.elems <$> Annex.getState Annex.cleanup
	liftIO Git.Command.reap -- zombies from long-running git processes
	sshCleanup -- ssh connection caching
	return True
