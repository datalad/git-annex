{- GIT_SSH and GIT_SSH_COMMAND support
 -
 - Copyright 2017 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Git.Ssh where

import Common
import Utility.Env

import Data.Char

gitSshEnv :: String
gitSshEnv = "GIT_SSH"

gitSshCommandEnv :: String
gitSshCommandEnv = "GIT_SSH_COMMAND"

gitSshEnvSet :: IO Bool
gitSshEnvSet = anyM (isJust <$$> getEnv) [gitSshEnv, gitSshCommandEnv]

-- Either a hostname, or user@host
type SshHost = String

type SshPort = Integer

-- Command to run on the remote host. It is run by the shell
-- there, so any necessary shell escaping of parameters in it should
-- already be done.
type SshCommand = String

-- | Checks for GIT_SSH and GIT_SSH_COMMAND and if set, returns
-- a command and parameters to run to ssh.
gitSsh :: SshHost -> Maybe SshPort -> SshCommand -> IO (Maybe (FilePath, [CommandParam]))
gitSsh host mp cmd = gitSsh' host mp cmd []

gitSsh' :: SshHost -> Maybe SshPort -> SshCommand -> [CommandParam] -> IO (Maybe (FilePath, [CommandParam]))
gitSsh' host mp cmd extrasshparams = do
	gsc <- getEnv gitSshCommandEnv
	case gsc of
		Just c
			-- git only runs the command with the shell
			-- when it contains spaces; otherwise it's
			-- treated the same as GIT_SSH
			| any isSpace c -> ret "sh"
				[ Param "-c"
				, Param (shellcmd c sshps)
				]
			| otherwise -> ret c sshps
		Nothing -> do
			gs <- getEnv gitSshEnv
			case gs of
				Just c -> ret c sshps
				Nothing -> return Nothing
 where
	ret c l = return $ Just (c, l)

	-- Git passes exactly these parameters to the ssh command.
	gitps = map Param $ case mp of
		Nothing -> [host, cmd]
		Just p -> [host, "-p", show p, cmd]

	-- Passing any extra parameters to the ssh command may
	-- break some commands.
	sshps = extrasshparams ++ gitps

	-- The shell command to run with sh -c is constructed
	-- this way, rather than using "$@" because there could be some
	-- unwanted parameters passed to the command, and this way they
	-- are ignored. For example, when Utility.Rsync.rsyncShell is 
	-- used, rsync adds some parameters after the command.
	shellcmd c ps = c ++ " " ++ unwords (map shellEscape (toCommand ps))
