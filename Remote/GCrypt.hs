{- git remotes encrypted using git-remote-gcrypt
 -
 - Copyright 2013 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Remote.GCrypt (
	remote,
	chainGen,
	getGCryptUUID,
	coreGCryptId,
	setupRepo,
	accessShellConfig,
) where

import qualified Data.Map as M
import qualified Data.ByteString.Lazy as L
import Control.Exception
import Data.Default

import Common.Annex
import qualified Annex
import Types.Remote
import Types.GitConfig
import Types.Crypto
import Types.Creds
import qualified Git
import qualified Git.Command
import qualified Git.Config
import qualified Git.GCrypt
import qualified Git.Construct
import qualified Git.Types as Git ()
import qualified Annex.Branch
import Config
import Config.Cost
import Remote.Helper.Git
import Remote.Helper.Encryptable
import Remote.Helper.Special
import Remote.Helper.Messages
import qualified Remote.Helper.Ssh as Ssh
import Utility.Metered
import Annex.UUID
import Annex.Ssh
import qualified Remote.Rsync
import qualified Remote.Directory
import Utility.Rsync
import Utility.Tmp
import Logs.Remote
import Logs.Transfer
import Utility.Gpg

remote :: RemoteType
remote = RemoteType {
	typename = "gcrypt",
	-- Remote.Git takes care of enumerating gcrypt remotes too,
	-- and will call our gen on them.
	enumerate = const (return []),
	generate = gen,
	setup = gCryptSetup
}

chainGen :: Git.Repo -> UUID -> RemoteConfig -> RemoteGitConfig -> Annex (Maybe Remote)
chainGen gcryptr u c gc = do
	g <- gitRepo
	-- get underlying git repo with real path, not gcrypt path
	r <- liftIO $ Git.GCrypt.encryptedRemote g gcryptr
	let r' = r { Git.remoteName = Git.remoteName gcryptr }
	gen r' u c gc

gen :: Git.Repo -> UUID -> RemoteConfig -> RemoteGitConfig -> Annex (Maybe Remote)
gen baser u c gc = do
	-- doublecheck that cache matches underlying repo's gcrypt-id
	-- (which might not be set), only for local repos
	(mgcryptid, r) <- getGCryptId True baser gc
	g <- gitRepo
	case (mgcryptid, Git.GCrypt.remoteRepoId g (Git.remoteName baser)) of
		(Just gcryptid, Just cachedgcryptid)
			| gcryptid /= cachedgcryptid -> resetup gcryptid r
		_ -> gen' r u c gc
  where
	-- A different drive may have been mounted, making a different
	-- gcrypt remote available. So need to set the cached
	-- gcrypt-id and annex-uuid of the remote to match the remote
	-- that is now available. Also need to set the gcrypt particiants
	-- correctly.
	resetup gcryptid r = do
		let u' = genUUIDInNameSpace gCryptNameSpace gcryptid
		v <- M.lookup u' <$> readRemoteLog
		case (Git.remoteName baser, v) of
			(Just remotename, Just c') -> do
				setGcryptEncryption c' remotename
				setConfig (remoteConfig baser "uuid") (fromUUID u')
				setConfig (ConfigKey $ Git.GCrypt.remoteConfigKey "gcrypt-id" remotename) gcryptid
				gen' r u' c' gc
			_ -> do
				warning $ "not using unknown gcrypt repository pointed to by remote " ++ Git.repoDescribe r
				return Nothing

gen' :: Git.Repo -> UUID -> RemoteConfig -> RemoteGitConfig -> Annex (Maybe Remote)
gen' r u c gc = do
	cst <- remoteCost gc $
		if repoCheap r then nearlyCheapRemoteCost else expensiveRemoteCost
	(rsynctransport, rsyncurl) <- rsyncTransportToObjects r gc
	let rsyncopts = Remote.Rsync.genRsyncOpts c gc rsynctransport rsyncurl
	let this = Remote 
		{ uuid = u
		, cost = cst
		, name = Git.repoDescribe r
		, storeKey = storeKeyDummy
		, retrieveKeyFile = retreiveKeyFileDummy
		, retrieveKeyFileCheap = \_ _ _ -> return False
		, removeKey = removeKeyDummy
		, lockContent = Nothing
		, checkPresent = checkPresentDummy
		, checkPresentCheap = repoCheap r
		, whereisKey = Nothing
		, remoteFsck = Nothing
		, repairRepo = Nothing
		, config = c
		, localpath = localpathCalc r
		, repo = r
		, gitconfig = gc { remoteGitConfig = Just $ extractGitConfig r }
		, readonly = Git.repoIsHttp r
		, availability = availabilityCalc r
		, remotetype = remote
		, mkUnavailable = return Nothing
		, getInfo = gitRepoInfo this
		, claimUrl = Nothing
		, checkUrl = Nothing
	}
	return $ Just $ specialRemote' specialcfg c
		(simplyPrepare $ store this rsyncopts)
		(simplyPrepare $ retrieve this rsyncopts)
		(simplyPrepare $ remove this rsyncopts)
		(simplyPrepare $ checkKey this rsyncopts)
		this
  where
	specialcfg
		| Git.repoIsUrl r = (specialRemoteCfg c)
			-- Rsync displays its own progress.
			{ displayProgress = False }
		| otherwise = specialRemoteCfg c

rsyncTransportToObjects :: Git.Repo -> RemoteGitConfig -> Annex ([CommandParam], String)
rsyncTransportToObjects r gc = do
	(rsynctransport, rsyncurl, _) <- rsyncTransport r gc
	return (rsynctransport, rsyncurl ++ "/annex/objects")

rsyncTransport :: Git.Repo -> RemoteGitConfig -> Annex ([CommandParam], String, AccessMethod)
rsyncTransport r gc
	| "ssh://" `isPrefixOf` loc = sshtransport $ break (== '/') $ drop (length "ssh://") loc
	| "//:" `isInfixOf` loc = othertransport
	| ":" `isInfixOf` loc = sshtransport $ separate (== ':') loc
	| otherwise = othertransport
  where
	loc = Git.repoLocation r
	sshtransport (host, path) = do
		let rsyncpath = if "/~/" `isPrefixOf` path
			then drop 3 path
			else path
		opts <- sshOptions (host, Nothing) gc []
		return (rsyncShell $ Param "ssh" : opts, host ++ ":" ++ rsyncpath, AccessShell)
	othertransport = return ([], loc, AccessDirect)

noCrypto :: Annex a
noCrypto = error "cannot use gcrypt remote without encryption enabled"

unsupportedUrl :: a
unsupportedUrl = error "using non-ssh remote repo url with gcrypt is not supported"

gCryptSetup :: Maybe UUID -> Maybe CredPair -> RemoteConfig -> Annex (RemoteConfig, UUID)
gCryptSetup mu _ c = go $ M.lookup "gitrepo" c
  where
	remotename = fromJust (M.lookup "name" c)
	go Nothing = error "Specify gitrepo="
	go (Just gitrepo) = do
		(c', _encsetup) <- encryptionSetup c
		inRepo $ Git.Command.run 
			[ Param "remote", Param "add"
			, Param remotename
			, Param $ Git.GCrypt.urlPrefix ++ gitrepo
			]

		setGcryptEncryption c' remotename

		{- Run a git fetch and a push to the git repo in order to get
		 - its gcrypt-id set up, so that later git annex commands
		 - will use the remote as a gcrypt remote. The fetch is
		 - needed if the repo already exists; the push is needed
		 - if the repo has not yet been initialized by gcrypt. -}
		void $ inRepo $ Git.Command.runBool
			[ Param "fetch"
			, Param remotename
			]
		void $ inRepo $ Git.Command.runBool
			[ Param "push"
			, Param remotename
			, Param $ Git.fromRef Annex.Branch.fullname
			]
		g <- inRepo Git.Config.reRead
		case Git.GCrypt.remoteRepoId g (Just remotename) of
			Nothing -> error "unable to determine gcrypt-id of remote"
			Just gcryptid -> do
				let u = genUUIDInNameSpace gCryptNameSpace gcryptid
				if Just u == mu || isNothing mu
					then do
						method <- setupRepo gcryptid =<< inRepo (Git.Construct.fromRemoteLocation gitrepo)
						gitConfigSpecialRemote u c' "gcrypt" (fromAccessMethod method)
						return (c', u)
					else error $ "uuid mismatch; expected " ++ show mu ++ " but remote gitrepo has " ++ show u ++ " (" ++ show gcryptid ++ ")"

{- Sets up the gcrypt repository. The repository is either a local
 - repo, or it is accessed via rsync directly, or it is accessed over ssh
 - and git-annex-shell is available to manage it.
 -
 - The GCryptID is recorded in the repository's git config for later use.
 - Also, if the git config has receive.denyNonFastForwards set, disable
 - it; gcrypt relies on being able to fast-forward branches.
 -}
setupRepo :: Git.GCrypt.GCryptId -> Git.Repo -> Annex AccessMethod
setupRepo gcryptid r
	| Git.repoIsUrl r = do
		(_, _, accessmethod) <- rsyncTransport r def
		case accessmethod of
			AccessDirect -> rsyncsetup
			AccessShell -> ifM gitannexshellsetup
				( return AccessShell
				, rsyncsetup
				)
	| Git.repoIsLocalUnknown r = localsetup =<< liftIO (Git.Config.read r)
	| otherwise = localsetup r
  where
	localsetup r' = do
		let setconfig k v = liftIO $ Git.Command.run [Param "config", Param k, Param v] r'
		setconfig coreGCryptId gcryptid
		setconfig denyNonFastForwards (Git.Config.boolConfig False)
		return AccessDirect

	{- As well as modifying the remote's git config, 
	 - create the objectDir on the remote,
	 - which is needed for direct rsync of objects to work.
	 -}
	rsyncsetup = Remote.Rsync.withRsyncScratchDir $ \tmp -> do
		liftIO $ createDirectoryIfMissing True $ tmp </> objectDir
		(rsynctransport, rsyncurl, _) <- rsyncTransport r def
		let tmpconfig = tmp </> "config"
		void $ liftIO $ rsync $ rsynctransport ++
			[ Param $ rsyncurl ++ "/config"
			, Param tmpconfig
			]
		liftIO $ do
			void $ Git.Config.changeFile tmpconfig coreGCryptId gcryptid
			void $ Git.Config.changeFile tmpconfig denyNonFastForwards (Git.Config.boolConfig False)
		ok <- liftIO $ rsync $ rsynctransport ++
			[ Param "--recursive"
			, Param $ tmp ++ "/"
			, Param rsyncurl
			]
		unless ok $
			error "Failed to connect to remote to set it up."
		return AccessDirect

	{-  Ask git-annex-shell to configure the repository as a gcrypt
	 -  repository. May fail if it is too old. -}
	gitannexshellsetup = Ssh.onRemote r (boolSystem, return False)
		"gcryptsetup" [ Param gcryptid ] []

	denyNonFastForwards = "receive.denyNonFastForwards"

accessShell :: Remote -> Bool
accessShell = accessShellConfig . gitconfig

accessShellConfig :: RemoteGitConfig -> Bool
accessShellConfig c = case method of
	AccessShell -> True
	_ -> False
  where
	method = toAccessMethod $ fromMaybe "" $ remoteAnnexGCrypt c

shellOrRsync :: Remote -> Annex a -> Annex a -> Annex a
shellOrRsync r ashell arsync
	| accessShell r = ashell
	| otherwise = arsync

{- Configure gcrypt to use the same list of keyids that
 - were passed to initremote as its participants.
 - Also, configure it to use a signing key that is in the list of
 - participants, which gcrypt requires is the case, and may not be
 - depending on system configuration.
 -
 - (For shared encryption, gcrypt's default behavior is used.)
 -
 - Also, sets gcrypt-publish-participants to avoid unncessary gpg
 - passphrase prompts.
 -}
setGcryptEncryption :: RemoteConfig -> String -> Annex ()
setGcryptEncryption c remotename = do
	let participants = remoteconfig Git.GCrypt.remoteParticipantConfigKey
	case extractCipher c of
		Nothing -> noCrypto
		Just (EncryptedCipher _ _ (KeyIds { keyIds = ks})) -> do
			setConfig participants (unwords ks)
			let signingkey = ConfigKey $ Git.GCrypt.remoteSigningKey remotename
			cmd <- gpgCmd <$> Annex.getGitConfig
			skeys <- M.keys <$> liftIO (secretKeys cmd)
			case filter (`elem` ks) skeys of
				[] -> noop
				(k:_) -> setConfig signingkey k
		Just (SharedCipher _) ->
			unsetConfig participants
	setConfig (remoteconfig Git.GCrypt.remotePublishParticipantConfigKey)
		(Git.Config.boolConfig True)
  where
	remoteconfig n = ConfigKey $ n remotename

store :: Remote -> Remote.Rsync.RsyncOpts -> Storer
store r rsyncopts
	| not $ Git.repoIsUrl (repo r) = 
		byteStorer $ \k b p -> guardUsable (repo r) (return False) $ liftIO $ do
			let tmpdir = Git.repoLocation (repo r) </> "tmp" </> keyFile k
			void $ tryIO $ createDirectoryIfMissing True tmpdir
			let tmpf = tmpdir </> keyFile k
			meteredWriteFile p tmpf b
			let destdir = parentDir $ gCryptLocation r k
			Remote.Directory.finalizeStoreGeneric tmpdir destdir
			return True
	| Git.repoIsSsh (repo r) = if accessShell r
		then fileStorer $ \k f p -> Ssh.rsyncHelper (Just p)
			=<< Ssh.rsyncParamsRemote False r Upload k f Nothing
		else fileStorer $ Remote.Rsync.store rsyncopts
	| otherwise = unsupportedUrl

retrieve :: Remote -> Remote.Rsync.RsyncOpts -> Retriever
retrieve r rsyncopts
	| not $ Git.repoIsUrl (repo r) = byteRetriever $ \k sink ->
		guardUsable (repo r) (return False) $
			sink =<< liftIO (L.readFile $ gCryptLocation r k)
	| Git.repoIsSsh (repo r) = if accessShell r
		then fileRetriever $ \f k p ->
			unlessM (Ssh.rsyncHelper (Just p) =<< Ssh.rsyncParamsRemote False r Download k f Nothing) $
				error "rsync failed"
		else fileRetriever $ Remote.Rsync.retrieve rsyncopts
	| otherwise = unsupportedUrl
  where

remove :: Remote -> Remote.Rsync.RsyncOpts -> Remover
remove r rsyncopts k
	| not $ Git.repoIsUrl (repo r) = guardUsable (repo r) (return False) $
		liftIO $ Remote.Directory.removeDirGeneric (Git.repoLocation (repo r)) (parentDir (gCryptLocation r k))
	| Git.repoIsSsh (repo r) = shellOrRsync r removeshell removersync
	| otherwise = unsupportedUrl
  where
	removersync = Remote.Rsync.remove rsyncopts k
	removeshell = Ssh.dropKey (repo r) k

checkKey :: Remote -> Remote.Rsync.RsyncOpts -> CheckPresent
checkKey r rsyncopts k
	| not $ Git.repoIsUrl (repo r) =
		guardUsable (repo r) (cantCheck $ repo r) $
			liftIO $ doesFileExist (gCryptLocation r k)
	| Git.repoIsSsh (repo r) = shellOrRsync r checkshell checkrsync
	| otherwise = unsupportedUrl
  where
	checkrsync = Remote.Rsync.checkKey (repo r) rsyncopts k
	checkshell = Ssh.inAnnex (repo r) k

{- Annexed objects are hashed using lower-case directories for max
 - portability. -}
gCryptLocation :: Remote -> Key -> FilePath
gCryptLocation r key = Git.repoLocation (repo r) </> objectDir </> keyPath key (hashDirLower def)

data AccessMethod = AccessDirect | AccessShell

fromAccessMethod :: AccessMethod -> String
fromAccessMethod AccessShell = "shell"
fromAccessMethod AccessDirect = "true"

toAccessMethod :: String -> AccessMethod
toAccessMethod "shell" = AccessShell
toAccessMethod _ = AccessDirect

getGCryptUUID :: Bool -> Git.Repo -> Annex (Maybe UUID)
getGCryptUUID fast r = (genUUIDInNameSpace gCryptNameSpace <$>) . fst
	<$> getGCryptId fast r def

coreGCryptId :: String
coreGCryptId = "core.gcrypt-id"

{- gcrypt repos set up by git-annex as special remotes have a
 - core.gcrypt-id setting in their config, which can be mapped back to
 - the remote's UUID.
 -
 - In fast mode, only checks local repos. To check a remote repo,
 - tries git-annex-shell and direct rsync of the git config file.
 -
 - (Also returns a version of input repo with its config read.) -}
getGCryptId :: Bool -> Git.Repo -> RemoteGitConfig -> Annex (Maybe Git.GCrypt.GCryptId, Git.Repo)
getGCryptId fast r gc
	| Git.repoIsLocal r || Git.repoIsLocalUnknown r = extract <$>
		liftIO (catchMaybeIO $ Git.Config.read r)
	| not fast = extract . liftM fst <$> getM (eitherToMaybe <$>)
		[ Ssh.onRemote r (Git.Config.fromPipe r, return (Left $ error "configlist failed")) "configlist" [] []
		, getConfigViaRsync r gc
		]
	| otherwise = return (Nothing, r)
  where
	extract Nothing = (Nothing, r)
	extract (Just r') = (Git.Config.getMaybe coreGCryptId r', r')

getConfigViaRsync :: Git.Repo -> RemoteGitConfig -> Annex (Either SomeException (Git.Repo, String))
getConfigViaRsync r gc = do
	(rsynctransport, rsyncurl, _) <- rsyncTransport r gc
	liftIO $ do
		withTmpFile "tmpconfig" $ \tmpconfig _ -> do
			void $ rsync $ rsynctransport ++
				[ Param $ rsyncurl ++ "/config"
				, Param tmpconfig
				]
			Git.Config.fromFile r tmpconfig
