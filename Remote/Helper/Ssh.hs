{- git-annex remote access with ssh and git-annex-shell
 -
 - Copyright 2011-2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Remote.Helper.Ssh where

import Common.Annex
import qualified Git
import qualified Git.Url
import Annex.UUID
import Annex.Ssh
import CmdLine.GitAnnexShell.Fields (Field, fieldName)
import qualified CmdLine.GitAnnexShell.Fields as Fields
import Types.GitConfig
import Types.Key
import Remote.Helper.Messages
import Utility.Metered
import Utility.Rsync
import Types.Remote
import Logs.Transfer

{- Generates parameters to ssh to a repository's host and run a command.
 - Caller is responsible for doing any neccessary shellEscaping of the
 - passed command. -}
toRepo :: Git.Repo -> [CommandParam] -> Annex [CommandParam]
toRepo r sshcmd = do
	g <- fromRepo id
	let c = extractRemoteGitConfig g (Git.repoDescribe r)
	let opts = map Param $ remoteAnnexSshOptions c
	let host = fromMaybe (error "bad ssh url") $ Git.Url.hostuser r
	params <- sshCachingOptions (host, Git.Url.port r) opts
	return $ params ++ Param host : sshcmd

{- Generates parameters to run a git-annex-shell command on a remote
 - repository. -}
git_annex_shell :: Git.Repo -> String -> [CommandParam] -> [(Field, String)] -> Annex (Maybe (FilePath, [CommandParam]))
git_annex_shell r command params fields
	| not $ Git.repoIsUrl r = return $ Just (shellcmd, shellopts ++ fieldopts)
	| Git.repoIsSsh r = do
		u <- getRepoUUID r
		sshparams <- toRepo r [Param $ sshcmd u ]
		return $ Just ("ssh", sshparams)
	| otherwise = return Nothing
  where
	dir = Git.repoPath r
	shellcmd = "git-annex-shell"
	shellopts = Param command : File dir : params
	sshcmd u = unwords $
		shellcmd : map shellEscape (toCommand shellopts) ++
		uuidcheck u ++
		map shellEscape (toCommand fieldopts)
	uuidcheck NoUUID = []
	uuidcheck (UUID u) = ["--uuid", u]
	fieldopts
		| null fields = []
		| otherwise = fieldsep : map fieldopt fields ++ [fieldsep]
	fieldsep = Param "--"
	fieldopt (field, value) = Param $
		fieldName field ++ "=" ++ value

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
	-> [(Field, String)]
	-> Annex a
onRemote r (with, errorval) command params fields = do
	s <- git_annex_shell r command params fields
	case s of
		Just (c, ps) -> liftIO $ with c ps
		Nothing -> return errorval

{- Checks if a remote contains a key. -}
inAnnex :: Git.Repo -> Key -> Annex (Either String Bool)
inAnnex r k = do
	showChecking r
	onRemote r (check, cantCheck r) "inannex" [Param $ key2file k] []
  where
	check c p = dispatch <$> safeSystem c p
	dispatch ExitSuccess = Right True
	dispatch (ExitFailure 1) = Right False
	dispatch _ = cantCheck r

{- Removes a key from a remote. -}
dropKey :: Git.Repo -> Key -> Annex Bool
dropKey r key = onRemote r (boolSystem, False) "dropkey"
	[ Params "--quiet --force"
	, Param $ key2file key
	]
	[]

rsyncHelper :: Maybe MeterUpdate -> [CommandParam] -> Annex Bool
rsyncHelper callback params = do
	showOutput -- make way for progress bar
	ifM (liftIO $ (maybe rsync rsyncProgress callback) params)
		( return True
		, do
			showLongNote "rsync failed -- run git annex again to resume file transfer"
			return False
		)

{- Generates rsync parameters that ssh to the remote and asks it
 - to either receive or send the key's content. -}
rsyncParamsRemote :: Bool -> Remote -> Direction -> Key -> FilePath -> AssociatedFile -> Annex [CommandParam]
rsyncParamsRemote direct r direction key file afile = do
	u <- getUUID
	let fields = (Fields.remoteUUID, fromUUID u)
		: (Fields.direct, if direct then "1" else "")
		: maybe [] (\f -> [(Fields.associatedFile, f)]) afile
	Just (shellcmd, shellparams) <- git_annex_shell (repo r)
		(if direction == Download then "sendkey" else "recvkey")
		[ Param $ key2file key ]
		fields
	-- Convert the ssh command into rsync command line.
	let eparam = rsyncShell (Param shellcmd:shellparams)
	let o = rsyncParams r
	return $ if direction == Download
		then o ++ rsyncopts eparam dummy (File file)
		else o ++ rsyncopts eparam (File file) dummy
  where
	rsyncopts ps source dest
		| end ps == [dashdash] = ps ++ [source, dest]
		| otherwise = ps ++ [dashdash, source, dest]
	dashdash = Param "--"
	{- The rsync shell parameter controls where rsync
	 - goes, so the source/dest parameter can be a dummy value,
	 - that just enables remote rsync mode.
	 - For maximum compatability with some patched rsyncs,
	 - the dummy value needs to still contain a hostname,
	 - even though this hostname will never be used. -}
	dummy = Param "dummy:"

-- --inplace to resume partial files
rsyncParams :: Remote -> [CommandParam]
rsyncParams r = Params "--progress --inplace" :
	map Param (remoteAnnexRsyncOptions $ gitconfig r)

