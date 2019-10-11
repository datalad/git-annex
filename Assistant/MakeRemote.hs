{- git-annex assistant remote creation utilities
 -
 - Copyright 2012, 2013 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Assistant.MakeRemote where

import Assistant.Common
import Assistant.Ssh
import qualified Types.Remote as R
import qualified Remote
import Remote.List
import qualified Remote.Rsync as Rsync
import qualified Remote.GCrypt as GCrypt
import qualified Git
import qualified Git.Command
import qualified Annex
import qualified Annex.SpecialRemote
import Annex.SpecialRemote.Config
import Logs.UUID
import Logs.Remote
import Git.Remote
import Git.Types (RemoteName)
import Creds
import Assistant.Gpg
import Utility.Gpg (KeyId)
import Types.GitConfig

import qualified Data.Map as M

{- Sets up a new git or rsync remote, accessed over ssh. -}
makeSshRemote :: SshData -> Annex RemoteName
makeSshRemote sshdata = maker (sshRepoName sshdata) (genSshUrl sshdata)
  where
	maker
		| onlyCapability sshdata RsyncCapable = makeRsyncRemote
		| otherwise = makeGitRemote

{- Runs an action that returns a name of the remote, and finishes adding it. -}
addRemote :: Annex RemoteName -> Annex Remote
addRemote a = do
	name <- a
	void remoteListRefresh
	maybe (error "failed to add remote") return
		=<< Remote.byName (Just name)

{- Inits a rsync special remote, and returns its name. -}
makeRsyncRemote :: RemoteName -> String -> Annex String
makeRsyncRemote name location = makeRemote name location $ const $ void $
	go =<< Annex.SpecialRemote.findExisting name
  where
	go Nothing = setupSpecialRemote name Rsync.remote config Nothing
		(Nothing, R.Init, Annex.SpecialRemote.newConfig name Nothing mempty mempty)
	go (Just (u, c)) = setupSpecialRemote name Rsync.remote config Nothing
		(Just u, R.Enable c, c)
	config = M.fromList
		[ (encryptionField, "shared")
		, ("rsyncurl", location)
		, ("type", "rsync")
		]

{- Inits a gcrypt special remote, and returns its name. -}
makeGCryptRemote :: RemoteName -> String -> KeyId -> Annex RemoteName
makeGCryptRemote remotename location keyid = 
	initSpecialRemote remotename GCrypt.remote Nothing $ M.fromList
		[ ("type", "gcrypt")
		, ("gitrepo", location)
		, configureEncryption HybridEncryption
		, ("keyid", keyid)
		]

type SpecialRemoteMaker = RemoteName -> RemoteType -> Maybe CredPair -> R.RemoteConfig -> Annex RemoteName

{- Inits a new special remote. The name is used as a suggestion, but
 - will be changed if there is already a special remote with that name. -}
initSpecialRemote :: SpecialRemoteMaker
initSpecialRemote name remotetype mcreds config = go 0
  where
	go :: Int -> Annex RemoteName
	go n = do
		let fullname = if n == 0  then name else name ++ show n
		Annex.SpecialRemote.findExisting fullname >>= \case
			Nothing -> setupSpecialRemote fullname remotetype config mcreds
				(Nothing, R.Init, Annex.SpecialRemote.newConfig fullname Nothing mempty mempty)
			Just _ -> go (n + 1)

{- Enables an existing special remote. -}
enableSpecialRemote :: SpecialRemoteMaker
enableSpecialRemote name remotetype mcreds config =
	Annex.SpecialRemote.findExisting name >>= \case
		Nothing -> error $ "Cannot find a special remote named " ++ name
		Just (u, c) -> setupSpecialRemote' False name remotetype config mcreds (Just u, R.Enable c, c)

setupSpecialRemote :: RemoteName -> RemoteType -> R.RemoteConfig -> Maybe CredPair -> (Maybe UUID, R.SetupStage, R.RemoteConfig) -> Annex RemoteName
setupSpecialRemote = setupSpecialRemote' True

setupSpecialRemote' :: Bool -> RemoteName -> RemoteType -> R.RemoteConfig -> Maybe CredPair -> (Maybe UUID, R.SetupStage, R.RemoteConfig) -> Annex RemoteName
setupSpecialRemote' setdesc name remotetype config mcreds (mu, ss, c) = do
	{- Currently, only 'weak' ciphers can be generated from the
	 - assistant, because otherwise GnuPG may block once the entropy
	 - pool is drained, and as of now there's no way to tell the user
	 - to perform IO actions to refill the pool. -}
	let weakc = M.insert "highRandomQuality" "false" $ M.union config c
	dummycfg <- liftIO dummyRemoteGitConfig
	(c', u) <- R.setup remotetype ss mu mcreds weakc dummycfg
	configSet u c'
	when setdesc $
		whenM (isNothing . M.lookup u <$> uuidDescMap) $
			describeUUID u (toUUIDDesc name)
	return name

{- Returns the name of the git remote it created. If there's already a
 - remote at the location, returns its name. -}
makeGitRemote :: String -> String -> Annex RemoteName
makeGitRemote basename location = makeRemote basename location $ \name ->
	void $ inRepo $ Git.Command.runBool
		[Param "remote", Param "add", Param name, Param location]

{- If there's not already a remote at the location, adds it using the
 - action, which is passed the name of the remote to make.
 -
 - Returns the name of the remote. -}
makeRemote :: String -> String -> (RemoteName -> Annex ()) -> Annex RemoteName
makeRemote basename location a = do
	rs <- Annex.getGitRemotes
	if not (any samelocation rs)
		then do
			let name = uniqueRemoteName basename 0 rs
			a name
			return name
		else return basename
  where
	samelocation x = Git.repoLocation x == location

{- Given a list of all remotes, generate an unused name for a new
 - remote, adding a number if necessary.
 -
 - Ensures that the returned name is a legal git remote name. -}
uniqueRemoteName :: String -> Int -> [Git.Repo] -> RemoteName
uniqueRemoteName basename n rs
	| null namecollision = name
	| otherwise = uniqueRemoteName legalbasename (succ n) rs
  where
	namecollision = filter samename rs
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
		getRemoteCredPair (R.config r) (R.gitconfig r) storage
