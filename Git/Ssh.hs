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
gitSsh host mp cmd = do
	gsc <- getEnv gitSshCommandEnv
	case gsc of
		Just c
			-- git only runs the command with the shell
			-- when it contains spaces; otherwise it's
			-- treated the same as GIT_SSH
			| any isSpace c -> ret "sh"
				[ [ Param "-c"
				  , Param (c ++ " \"$@\"")
				  , Param c
				  ]
				, gitps
				-- cmd is already shell escaped
				-- for the remote side, but needs to be
				-- shell-escaped once more since it's
				-- passed through the local shell.
				, [ Param $ shellEscape $ cmd ]
				]
			| otherwise -> ret c [ gitps, [Param cmd]]
		Nothing -> do
			gs <- getEnv gitSshEnv
			case gs of
				Just c -> ret c [ gitps, [Param cmd]]
				Nothing -> return Nothing
 where
	-- git passes exactly these parameters, followed by another
	-- parameter containing the remote command.
	gitps = map Param $ case mp of
		Nothing -> [host]
		Just p -> [host, "-p", show p]
	ret c ll = return $ Just (c, concat ll)
