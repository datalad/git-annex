{- git-annex-shell main program
 -
 - Copyright 2010 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

import System.Environment
import Data.List

import qualified Git
import CmdLine
import Command
import Utility
import Options

import qualified Command.ConfigList
import qualified Command.InAnnex
import qualified Command.DropKey
import qualified Command.RecvKey
import qualified Command.SendKey

cmds :: [Command]
cmds = map adddirparam $ concat
	[ Command.ConfigList.command
	, Command.InAnnex.command
	, Command.DropKey.command
	, Command.RecvKey.command
	, Command.SendKey.command
	]
	where
		adddirparam c = c { cmdparams = "DIRECTORY " ++ cmdparams c }

header :: String
header = "Usage: git-annex-shell [-c] command [parameters ...] [option ..]"

main :: IO ()
main = main' =<< getArgs

main' :: [String] -> IO ()
main' [] = failure
-- skip leading -c options, passed by eg, ssh
main' ("-c":p) = main' p
-- a command can be either a builtin or something to pass to git-shell
main' c@(cmd:dir:params)
	| cmd `elem` builtins = builtin cmd dir params
	| otherwise = external c
main' c@(cmd:_)
	-- Handle the case of being the user's login shell. It will be passed
	-- a single string containing all the real parameters.
	| "git-annex-shell " `isPrefixOf` cmd = main' $ drop 1 $ shellUnEscape cmd
	| cmd `elem` builtins = failure
	| otherwise = external c

builtins :: [String]
builtins = map cmdname cmds

builtin :: String -> String -> [String] -> IO ()
builtin cmd dir params =
	Git.repoAbsPath dir >>= Git.repoFromAbsPath >>=
		dispatch (cmd : filterparams params) cmds commonOptions header

external :: [String] -> IO ()
external params =
	unlessM (boolSystem "git-shell" $ map Param $ "-c":filterparams params) $
		error "git-shell failed"

-- Drop all args after "--".
-- These tend to be passed by rsync and not useful.
filterparams :: [String] -> [String]
filterparams [] = []
filterparams ("--":_) = []
filterparams (a:as) = a:filterparams as

failure :: IO ()
failure = error $ "bad parameters\n\n" ++ usage header cmds commonOptions
