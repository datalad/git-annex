{- git-annex remote access with ssh
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Remote.Helper.Ssh where

import Common.Annex
import qualified Git
import qualified Git.Url
import Config
import Annex.UUID
import Annex.Ssh

{- Generates parameters to ssh to a repository's host and run a command.
 - Caller is responsible for doing any neccessary shellEscaping of the
 - passed command. -}
sshToRepo :: Git.Repo -> [CommandParam] -> Annex [CommandParam]
sshToRepo repo sshcmd = do
	opts <- map Param . words <$> getRemoteConfig repo "ssh-options" ""
	params <- sshParams (Git.Url.hostuser repo, Git.Url.port repo) opts
	return $ params ++ sshcmd

{- Generates parameters to run a git-annex-shell command on a remote
 - repository. -}
git_annex_shell :: Git.Repo -> String -> [CommandParam] -> Annex (Maybe (FilePath, [CommandParam]))
git_annex_shell r command params
	| not $ Git.repoIsUrl r = return $ Just (shellcmd, shellopts)
	| Git.repoIsSsh r = do
		uuid <- getRepoUUID r
		sshparams <- sshToRepo r [Param $ sshcmd uuid ]
		return $ Just ("ssh", sshparams)
	| otherwise = return Nothing
	where
		dir = Git.workTree r
		shellcmd = "git-annex-shell"
		shellopts = Param command : File dir : params
		sshcmd uuid = unwords $
			shellcmd : map shellEscape (toCommand shellopts) ++
			uuidcheck uuid
		uuidcheck NoUUID = []
		uuidcheck (UUID u) = ["--uuid", u]

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
