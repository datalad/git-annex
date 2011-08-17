{- git-annex remote access with ssh
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Remote.Ssh where

import Control.Monad.State (liftIO)

import qualified Git
import Utility
import Types
import Config

{- Generates parameters to ssh to a repository's host and run a command.
 - Caller is responsible for doing any neccessary shellEscaping of the
 - passed command. -}
sshToRepo :: Git.Repo -> [CommandParam] -> Annex [CommandParam]
sshToRepo repo sshcmd = do
	s <- getConfig repo "ssh-options" ""
	let sshoptions = map Param (words s)
	let sshport = case Git.urlPort repo of
		Nothing -> []
		Just p -> [Param "-p", Param (show p)]
	let sshhost = Param $ Git.urlHostUser repo
	return $ sshoptions ++ sshport ++ [sshhost] ++ sshcmd

{- Generates parameters to run a git-annex-shell command on a remote
 - repository. -}
git_annex_shell :: Git.Repo -> String -> [CommandParam] -> Annex (Maybe (FilePath, [CommandParam]))
git_annex_shell r command params
	| not $ Git.repoIsUrl r = return $ Just (shellcmd, shellopts)
	| Git.repoIsSsh r = do
		sshparams <- sshToRepo r [Param sshcmd]
		return $ Just ("ssh", sshparams)
	| otherwise = return Nothing
	where
		dir = Git.workTree r
		shellcmd = "git-annex-shell"
		shellopts = Param command : File dir : params
		sshcmd = shellcmd ++ " " ++ 
			unwords (map shellEscape $ toCommand shellopts)

{- Uses a supplied function (such as boolSystem) to run a git-annex-shell
 - command on a remote.
 -
 - Or, if the remote does not support running remote commands, returns
 - a specified error value. -}
onRemote 
	:: Git.Repo
	-> (FilePath -> [CommandParam] -> IO a, a)
	-> String
	-> [CommandParam]
	-> Annex a
onRemote r (with, errorval) command params = do
	s <- git_annex_shell r command params
	case s of
		Just (c, ps) -> liftIO $ with c ps
		Nothing -> return errorval
