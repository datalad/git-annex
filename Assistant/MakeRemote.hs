{- git-annex assistant remote creation utilities
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.MakeRemote where

import Assistant.Common
import Assistant.ThreadedMonad
import Assistant.DaemonStatus
import Assistant.ScanRemotes
import Assistant.Ssh
import Assistant.Sync
import qualified Types.Remote as R
import qualified Remote
import Remote.List
import qualified Remote.Rsync as Rsync
import qualified Git
import qualified Git.Command
import qualified Command.InitRemote
import Logs.UUID
import Logs.Remote

import qualified Data.Text as T
import qualified Data.Map as M
import Data.Char

{- Sets up and begins syncing with a new ssh or rsync remote. -}
makeSshRemote :: ThreadState -> DaemonStatusHandle -> ScanRemoteMap -> Bool -> SshData -> IO Remote
makeSshRemote st dstatus scanremotes forcersync sshdata = do
	r <- runThreadState st $
		addRemote $ maker (sshRepoName sshdata) sshurl
	syncNewRemote st dstatus scanremotes r
	return r
	where
		rsync = forcersync || rsyncOnly sshdata
		maker
			| rsync = makeRsyncRemote
			| otherwise = makeGitRemote
		sshurl = T.unpack $ T.concat $
			if rsync
				then [u, h, T.pack ":", sshDirectory sshdata, T.pack "/"]
				else [T.pack "ssh://", u, h, d, T.pack "/"]
			where
				u = maybe (T.pack "") (\v -> T.concat [v, T.pack "@"]) $ sshUserName sshdata
				h = sshHostName sshdata
				d
					| T.pack "/" `T.isPrefixOf` sshDirectory sshdata = d
					| otherwise = T.concat [T.pack "/~/", sshDirectory sshdata]
	
{- Runs an action that returns a name of the remote, and finishes adding it. -}
addRemote :: Annex String -> Annex Remote
addRemote a = do
	name <- a
	void remoteListRefresh
	maybe (error "failed to add remote") return =<< Remote.byName (Just name)

{- Inits a rsync special remote, and returns its name. -}
makeRsyncRemote :: String -> String -> Annex String
makeRsyncRemote name location = makeRemote name location $
	const $ makeSpecialRemote name Rsync.remote config
	where
		config = M.fromList
			[ ("encryption", "shared")
			, ("rsyncurl", location)
			, ("type", "rsync")
			]

{- Inits a special remote. -}
makeSpecialRemote :: String -> RemoteType -> R.RemoteConfig -> Annex ()
makeSpecialRemote name remotetype config = do
	(u, c) <- Command.InitRemote.findByName name
	c' <- R.setup remotetype u $ M.union config c
	describeUUID u name
	configSet u c'

{- Returns the name of the git remote it created. If there's already a
 - remote at the location, returns its name. -}
makeGitRemote :: String -> String -> Annex String
makeGitRemote basename location = makeRemote basename location $ \name ->
	void $ inRepo $
		Git.Command.runBool "remote"
			[Param "add", Param name, Param location]

{- If there's not already a remote at the location, adds it using the
 - action, which is passed the name of the remote to make.
 -
 - Returns the name of the remote. -}
makeRemote :: String -> String -> (String -> Annex ()) -> Annex String
makeRemote basename location a = do
	r <- fromRepo id
	if not (any samelocation $ Git.remotes r)
		then do
			let name = uniqueRemoteName basename 0 r
			a name
			return name
		else return basename
	where
		samelocation x = Git.repoLocation x == location

{- Generate an unused name for a remote, adding a number if
 - necessary.
 -
 - Ensures that the returned name is a legal git remote name. -}
uniqueRemoteName :: String -> Int -> Git.Repo -> String
uniqueRemoteName basename n r
	| null namecollision = name
	| otherwise = uniqueRemoteName legalbasename (succ n) r
	where
		namecollision = filter samename (Git.remotes r)
		samename x = Git.remoteName x == Just name
		name
			| n == 0 = legalbasename
			| otherwise = legalbasename ++ show n
		legalbasename = filter legal basename
		legal '_' = True
		legal c = isAlphaNum c
