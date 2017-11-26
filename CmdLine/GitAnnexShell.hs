{- git-annex-shell main program
 -
 - Copyright 2010-2012 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module CmdLine.GitAnnexShell where

import Annex.Common
import qualified Git.Construct
import qualified Git.Config
import CmdLine
import CmdLine.GlobalSetter
import Command
import Annex.UUID
import CmdLine.GitAnnexShell.Checks
import CmdLine.GitAnnexShell.Fields
import Remote.GCrypt (getGCryptUUID)

import qualified Command.ConfigList
import qualified Command.InAnnex
import qualified Command.LockContent
import qualified Command.DropKey
import qualified Command.RecvKey
import qualified Command.SendKey
import qualified Command.TransferInfo
import qualified Command.Commit
import qualified Command.NotifyChanges
import qualified Command.GCryptSetup

cmds_readonly :: [Command]
cmds_readonly =
	[ Command.ConfigList.cmd
	, gitAnnexShellCheck Command.InAnnex.cmd
	, gitAnnexShellCheck Command.LockContent.cmd
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
cmds = map (adddirparam . noMessages) (cmds_readonly ++ cmds_notreadonly)
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
	unexpected expected s = giveup $
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
	unless (cmd `elem` map cmdname cmds_readonly)
		checkNotReadOnly
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
				giveup $ "failed to read git config of git repository in " ++ hn ++ " on " ++ dir ++ "; perhaps this repository is not set up correctly or has moved"

external :: [String] -> IO ()
external params = do
	{- Normal git-shell commands all have the directory as their last
	 - parameter. -}
	let lastparam = lastMaybe =<< shellUnEscape <$> lastMaybe params
	    (params', _, _) = partitionParams params
	checkDirectory lastparam
	checkNotLimited
	unlessM (boolSystem "git-shell" $ map Param $ "-c":params') $
		giveup "git-shell failed"

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
	| field == fieldName unlocked = fieldCheck unlocked val
	| field == fieldName direct = fieldCheck direct val
	| field == fieldName autoInit = fieldCheck autoInit val
	| otherwise = False

failure :: IO ()
failure = giveup $ "bad parameters\n\n" ++ usage h cmds
  where
	h = "git-annex-shell [-c] command [parameters ...] [option ...]"
