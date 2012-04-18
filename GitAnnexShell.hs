{- git-annex-shell main program
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module GitAnnexShell where

import System.Environment
import System.Console.GetOpt

import Common.Annex
import qualified Git.Construct
import CmdLine
import Command
import Annex.UUID
import qualified Option

import qualified Command.ConfigList
import qualified Command.InAnnex
import qualified Command.DropKey
import qualified Command.RecvKey
import qualified Command.SendKey
import qualified Command.Commit

cmds_readonly :: [Command]
cmds_readonly = concat
	[ Command.ConfigList.def
	, Command.InAnnex.def
	, Command.SendKey.def
	]

cmds_notreadonly :: [Command]
cmds_notreadonly = concat
	[ Command.RecvKey.def
	, Command.DropKey.def
	, Command.Commit.def
	]

cmds :: [Command]
cmds = map adddirparam $ cmds_readonly ++ cmds_notreadonly
	where
		adddirparam c = c
			{ cmdparamdesc = "DIRECTORY " ++ cmdparamdesc c
			}

options :: [OptDescr (Annex ())]
options = Option.common ++
	[ Option [] ["uuid"] (ReqArg checkuuid paramUUID) "repository uuid"
	]
	where
		checkuuid expected = getUUID >>= check
			where
				check u | u == toUUID expected = return ()
				check NoUUID = unexpected "uninitialized repository"
				check u = unexpected $ "UUID " ++ fromUUID u
				unexpected s = error $
					"expected repository UUID " ++
					expected ++ " but found " ++ s

header :: String
header = "Usage: git-annex-shell [-c] command [parameters ...] [option ..]"

run :: [String] -> IO ()
run [] = failure
-- skip leading -c options, passed by eg, ssh
run ("-c":p) = run p
-- a command can be either a builtin or something to pass to git-shell
run c@(cmd:dir:params)
	| cmd `elem` builtins = builtin cmd dir params
	| otherwise = external c
run c@(cmd:_)
	-- Handle the case of being the user's login shell. It will be passed
	-- a single string containing all the real parameters.
	| "git-annex-shell " `isPrefixOf` cmd = run $ drop 1 $ shellUnEscape cmd
	| cmd `elem` builtins = failure
	| otherwise = external c

builtins :: [String]
builtins = map cmdname cmds

builtin :: String -> String -> [String] -> IO ()
builtin cmd dir params = do
	checkNotReadOnly cmd
	dispatch False (cmd : filterparams params) cmds options header $
		Git.Construct.repoAbsPath dir >>= Git.Construct.fromAbsPath

external :: [String] -> IO ()
external params = do
	checkNotLimited
	unlessM (boolSystem "git-shell" $ map Param $ "-c":filterparams params) $
		error "git-shell failed"

-- Drop all args after "--".
-- These tend to be passed by rsync and not useful.
filterparams :: [String] -> [String]
filterparams [] = []
filterparams ("--":_) = []
filterparams (a:as) = a:filterparams as

failure :: IO ()
failure = error $ "bad parameters\n\n" ++ usage header cmds options

checkNotLimited :: IO ()
checkNotLimited = checkEnv "GIT_ANNEX_SHELL_LIMITED"

checkNotReadOnly :: String -> IO ()
checkNotReadOnly cmd
	| cmd `elem` map cmdname cmds_readonly = return ()
	| otherwise = checkEnv "GIT_ANNEX_SHELL_READONLY"

checkEnv :: String -> IO ()
checkEnv var =
	whenM (not . null <$> catchDefaultIO (getEnv var) "") $
		error $ "Action blocked by " ++ var
