{- git-annex-shell main program
 -
 - Copyright 2010-2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module CmdLine.GitAnnexShell where

import System.Environment
import System.Console.GetOpt

import Common.Annex
import qualified Git.Construct
import CmdLine
import Command
import Annex.UUID
import Annex (setField)
import Fields
import Utility.UserInfo
import Remote.GCrypt (getGCryptUUID)
import qualified Annex
import Init

import qualified Command.ConfigList
import qualified Command.InAnnex
import qualified Command.DropKey
import qualified Command.RecvKey
import qualified Command.SendKey
import qualified Command.TransferInfo
import qualified Command.Commit
import qualified Command.GCryptSetup

cmds_readonly :: [Command]
cmds_readonly = concat
	[ gitAnnexShellCheck Command.ConfigList.def
	, gitAnnexShellCheck Command.InAnnex.def
	, gitAnnexShellCheck Command.SendKey.def
	, gitAnnexShellCheck Command.TransferInfo.def
	]

cmds_notreadonly :: [Command]
cmds_notreadonly = concat
	[ gitAnnexShellCheck Command.RecvKey.def
	, gitAnnexShellCheck Command.DropKey.def
	, gitAnnexShellCheck Command.Commit.def
	, Command.GCryptSetup.def
	]

cmds :: [Command]
cmds = map adddirparam $ cmds_readonly ++ cmds_notreadonly
  where
	adddirparam c = c { cmdparamdesc = "DIRECTORY " ++ cmdparamdesc c }

options :: [OptDescr (Annex ())]
options = commonOptions ++
	[ Option [] ["uuid"] (ReqArg checkUUID paramUUID) "local repository uuid"
	]
  where
	checkUUID expected = getUUID >>= check
	  where
		check u | u == toUUID expected = noop
		check NoUUID = checkGCryptUUID expected
		check u = unexpectedUUID expected u
	checkGCryptUUID expected = check =<< getGCryptUUID True =<< gitRepo
	  where
	  	check (Just u) | u == toUUID expected = noop
		check Nothing = unexpected expected "uninitialized repository"
		check (Just u) = unexpectedUUID expected u
	unexpectedUUID expected u = unexpected expected $ "UUID " ++ fromUUID u
	unexpected expected s = error $
		"expected repository UUID " ++ expected ++ " but found " ++ s

header :: String
header = "git-annex-shell [-c] command [parameters ...] [option ...]"

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
	checkDirectory $ Just dir
	let (params', fieldparams, opts) = partitionParams params
	    fields = filter checkField $ parseFields fieldparams
	    cmds' = map (newcmd $ unwords opts) cmds
	dispatch False (cmd : params') cmds' options fields header $
		Git.Construct.repoAbsPath dir >>= Git.Construct.fromAbsPath
  where
	addrsyncopts opts seek k = setField "RsyncOptions" opts >> seek k
	newcmd opts c = c { cmdseek = addrsyncopts opts (cmdseek c) }

external :: [String] -> IO ()
external params = do
	{- Normal git-shell commands all have the directory as their last
	 - parameter. -}
	let lastparam = lastMaybe =<< shellUnEscape <$> lastMaybe params
	    (params', _, _) = partitionParams params
	checkDirectory lastparam
	checkNotLimited
	unlessM (boolSystem "git-shell" $ map Param $ "-c":params') $
		error "git-shell failed"

{- Split the input list into 3 groups separated with a double dash --.
 - Parameters between two -- markers are field settings, in the form:
 - field=value field=value
 -
 - Parameters after the last -- are the command itself and its arguments e.g.,
 - rsync --bandwidth=100.
 -}
partitionParams :: [String] -> ([String], [String], [String])
partitionParams ps = case segment (== "--") ps of
	params:fieldparams:rest -> ( params, fieldparams, intercalate ["--"] rest )
	[params] -> (params, [], [])
	_ -> ([], [], [])

parseFields :: [String] -> [(String, String)]
parseFields = map (separate (== '='))

{- Only allow known fields to be set, ignore others.
 - Make sure that field values make sense. -}
checkField :: (String, String) -> Bool
checkField (field, value)
	| field == fieldName remoteUUID = fieldCheck remoteUUID value
	| field == fieldName associatedFile = fieldCheck associatedFile value
	| field == fieldName direct = fieldCheck direct value
	| otherwise = False

failure :: IO ()
failure = error $ "bad parameters\n\n" ++ usage header cmds

checkNotLimited :: IO ()
checkNotLimited = checkEnv "GIT_ANNEX_SHELL_LIMITED"

checkNotReadOnly :: String -> IO ()
checkNotReadOnly cmd
	| cmd `elem` map cmdname cmds_readonly = noop
	| otherwise = checkEnv "GIT_ANNEX_SHELL_READONLY"

checkDirectory :: Maybe FilePath -> IO ()
checkDirectory mdir = do
	v <- catchMaybeIO $ getEnv "GIT_ANNEX_SHELL_DIRECTORY"
	case (v, mdir) of
		(Nothing, _) -> noop
		(Just d, Nothing) -> req d Nothing
		(Just d, Just dir)
			|  d `equalFilePath` dir -> noop
			| otherwise -> do
				home <- myHomeDir
				d' <- canondir home d
				dir' <- canondir home dir
				if d' `equalFilePath` dir'
					then noop
					else req d' (Just dir')
  where
	req d mdir' = error $ unwords 
		[ "Only allowed to access"
		, d
		, maybe "and could not determine directory from command line" ("not " ++) mdir'
		]

	{- A directory may start with ~/ or in some cases, even /~/,
	 - or could just be relative to home, or of course could
	 - be absolute. -}
	canondir home d
		| "~/" `isPrefixOf` d = return d
		| "/~/" `isPrefixOf` d = return $ drop 1 d
		| otherwise = relHome $ absPathFrom home d

checkEnv :: String -> IO ()
checkEnv var = do
	v <- catchMaybeIO $ getEnv var
	case v of
		Nothing -> noop
		Just "" -> noop
		Just _ -> error $ "Action blocked by " ++ var

{- Modifies a Command to check that it is run in either a git-annex
 - repository, or a repository with a gcrypt-id set. -}
gitAnnexShellCheck :: [Command] -> [Command]
gitAnnexShellCheck = map $ addCheck okforshell . dontCheck repoExists
  where
	okforshell = unlessM (isInitialized <||> isJust . gcryptId <$> Annex.getGitConfig) $
		error "Not a git-annex or gcrypt repository."
