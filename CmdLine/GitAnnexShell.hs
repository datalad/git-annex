{- git-annex-shell main program
 -
 - Copyright 2010-2012 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module CmdLine.GitAnnexShell where

import System.Environment

import Common.Annex
import qualified Git.Construct
import qualified Git.Config
import CmdLine
import CmdLine.GlobalSetter
import Command
import Annex.UUID
import CmdLine.GitAnnexShell.Fields
import Utility.UserInfo
import Remote.GCrypt (getGCryptUUID)
import qualified Annex
import Annex.Init

import qualified Command.ConfigList
import qualified Command.InAnnex
import qualified Command.DropKey
import qualified Command.RecvKey
import qualified Command.SendKey
import qualified Command.TransferInfo
import qualified Command.Commit
import qualified Command.NotifyChanges
import qualified Command.GCryptSetup

cmds_readonly :: [Command]
cmds_readonly =
	[ gitAnnexShellCheck Command.ConfigList.cmd
	, gitAnnexShellCheck Command.InAnnex.cmd
	, gitAnnexShellCheck Command.SendKey.cmd
	, gitAnnexShellCheck Command.TransferInfo.cmd
	, gitAnnexShellCheck Command.NotifyChanges.cmd
	]

cmds_notreadonly :: [Command]
cmds_notreadonly =
	[ gitAnnexShellCheck Command.RecvKey.cmd
	, gitAnnexShellCheck Command.DropKey.cmd
	, gitAnnexShellCheck Command.Commit.cmd
	, Command.GCryptSetup.cmd
	]

cmds :: [Command]
cmds = map adddirparam $ cmds_readonly ++ cmds_notreadonly
  where
	adddirparam c = c { cmdparamdesc = "DIRECTORY " ++ cmdparamdesc c }

globalOptions :: [GlobalOption]
globalOptions = 
	globalSetter checkUUID (strOption
		( long "uuid" <> metavar paramUUID
		<> help "local repository uuid"
		))
	: commonGlobalOptions
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
	    rsyncopts = ("RsyncOptions", unwords opts)
	    fields = rsyncopts : filter checkField (parseFields fieldparams)
	dispatch False (cmd : params') cmds globalOptions fields mkrepo
		"git-annex-shell"
		"Restricted login shell for git-annex only SSH access"
  where
	mkrepo = do
		r <- Git.Construct.repoAbsPath dir >>= Git.Construct.fromAbsPath
		Git.Config.read r
			`catchIO` \_ -> do
				hn <- fromMaybe "unknown" <$> getHostname
				error $ "failed to read git config of git repository in " ++ hn ++ " on " ++ dir ++ "; perhaps this repository is not set up correctly or has moved"

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
checkField (field, val)
	| field == fieldName remoteUUID = fieldCheck remoteUUID val
	| field == fieldName associatedFile = fieldCheck associatedFile val
	| field == fieldName direct = fieldCheck direct val
	| otherwise = False

failure :: IO ()
failure = error $ "bad parameters\n\n" ++ usage h cmds
  where
	h = "git-annex-shell [-c] command [parameters ...] [option ...]"

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
gitAnnexShellCheck :: Command -> Command
gitAnnexShellCheck = addCheck okforshell . dontCheck repoExists
  where
	okforshell = unlessM (isInitialized <||> isJust . gcryptId <$> Annex.getGitConfig) $
		error "Not a git-annex or gcrypt repository."
