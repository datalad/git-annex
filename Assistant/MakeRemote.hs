{- git-annex assistant remote creation utilities
 -
 - Copyright 2012, 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.MakeRemote where

import Assistant.Common
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
import Git.Remote
import Config
import Config.Cost
import Creds

import qualified Data.Text as T
import qualified Data.Map as M

{- Sets up and begins syncing with a new ssh or rsync remote. -}
makeSshRemote :: Bool -> SshData -> Maybe Cost -> Assistant Remote
makeSshRemote forcersync sshdata mcost = do
	r <- liftAnnex $
		addRemote $ maker (sshRepoName sshdata) sshurl
	liftAnnex $ maybe noop (setRemoteCost r) mcost
	syncRemote r
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
			| T.pack "/" `T.isPrefixOf` sshDirectory sshdata = sshDirectory sshdata
			| T.pack "~/" `T.isPrefixOf` sshDirectory sshdata = T.concat [T.pack "/", sshDirectory sshdata]
			| otherwise = T.concat [T.pack "/~/", sshDirectory sshdata]
	
{- Runs an action that returns a name of the remote, and finishes adding it. -}
addRemote :: Annex String -> Annex Remote
addRemote a = do
	name <- a
	void remoteListRefresh
	maybe (error "failed to add remote") return
		=<< Remote.byName (Just name)

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

{- Inits a new special remote, or enables an existing one.
 -
 - Currently, only 'weak' ciphers can be generated from the assistant,
 - because otherwise GnuPG may block once the entropy pool is drained,
 - and as of now there's no way to tell the user to perform IO actions
 - to refill the pool. -}
makeSpecialRemote :: String -> RemoteType -> R.RemoteConfig -> Annex ()
makeSpecialRemote name remotetype config =
	go =<< Command.InitRemote.findExisting name
  where
  	go Nothing = go =<< Just <$> Command.InitRemote.generateNew name
	go (Just (u, c)) = do
		c' <- R.setup remotetype u $
			M.insert "highRandomQuality" "false" $ M.union config c
		describeUUID u name
		configSet u c'

{- Returns the name of the git remote it created. If there's already a
 - remote at the location, returns its name. -}
makeGitRemote :: String -> String -> Annex String
makeGitRemote basename location = makeRemote basename location $ \name ->
	void $ inRepo $ Git.Command.runBool
		[Param "remote", Param "add", Param name, Param location]

{- If there's not already a remote at the location, adds it using the
 - action, which is passed the name of the remote to make.
 -
 - Returns the name of the remote. -}
makeRemote :: String -> String -> (String -> Annex ()) -> Annex String
makeRemote basename location a = do
	g <- gitRepo
	if not (any samelocation $ Git.remotes g)
		then do
			
			let name = uniqueRemoteName basename 0 g
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
	legalbasename = makeLegalName basename

{- Finds a CredPair belonging to any Remote that is of a given type
 - and matches some other criteria.
 -
 - This can be used as a default when another repository is being set up
 - using the same service.
 -
 - A function must be provided that returns the CredPairStorage
 - to use for a particular Remote's uuid.
 -}
previouslyUsedCredPair
	:: (UUID -> CredPairStorage)
	-> RemoteType
	-> (Remote -> Bool)
	-> Annex (Maybe CredPair)
previouslyUsedCredPair getstorage remotetype criteria =
	getM fromstorage =<< filter criteria . filter sametype <$> remoteList
  where
	sametype r = R.typename (R.remotetype r) == R.typename remotetype
	fromstorage r = do
		let storage = getstorage (R.uuid r)
		getRemoteCredPair (R.config r) storage
