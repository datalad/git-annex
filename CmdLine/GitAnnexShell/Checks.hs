{- git-annex-shell checks
 -
 - Copyright 2012 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module CmdLine.GitAnnexShell.Checks where

import Annex.Common
import Command
import qualified Annex
import Annex.Init
import Utility.UserInfo
import Utility.Env

limitedEnv :: String
limitedEnv = "GIT_ANNEX_SHELL_LIMITED"

checkNotLimited :: IO ()
checkNotLimited = checkEnv limitedEnv

readOnlyEnv :: String
readOnlyEnv = "GIT_ANNEX_SHELL_READONLY"

checkNotReadOnly :: IO ()
checkNotReadOnly = checkEnv readOnlyEnv

appendOnlyEnv :: String
appendOnlyEnv = "GIT_ANNEX_SHELL_APPENDONLY"

checkNotAppendOnly :: IO ()
checkNotAppendOnly = checkEnv appendOnlyEnv

checkEnv :: String -> IO ()
checkEnv var = checkEnvSet var >>= \case
	False -> noop
	True -> giveup $ "Action blocked by " ++ var

checkEnvSet :: String -> IO Bool
checkEnvSet var = getEnv var >>= return . \case
	Nothing -> False
	Just "" -> False
	Just _ -> True

checkDirectory :: Maybe FilePath -> IO ()
checkDirectory mdir = do
	v <- getEnv "GIT_ANNEX_SHELL_DIRECTORY"
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
	req d mdir' = giveup $ unwords 
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
		| otherwise = relHome $ fromRawFilePath $ absPathFrom 
			(toRawFilePath home)
			(toRawFilePath d)

{- Modifies a Command to check that it is run in either a git-annex
 - repository, or a repository with a gcrypt-id set. -}
gitAnnexShellCheck :: Command -> Command
gitAnnexShellCheck = addCheck okforshell . dontCheck repoExists
  where
	okforshell = unlessM (isInitialized <||> isJust . gcryptId <$> Annex.getGitConfig) $
		giveup "Not a git-annex or gcrypt repository."
