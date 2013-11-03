{- git remotes encrypted using git-remote-gcrypt
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Remote.GCrypt (
	remote,
	gen,
	getGCryptUUID,
	coreGCryptId,
	setupRepo
) where

import qualified Data.Map as M
import qualified Data.ByteString.Lazy as L
import Control.Exception.Extensible

import Common.Annex
import Types.Remote
import Types.GitConfig
import Types.Crypto
import qualified Git
import qualified Git.Command
import qualified Git.Config
import qualified Git.GCrypt
import qualified Git.Construct
import qualified Git.Types as Git ()
import qualified Annex.Branch
import qualified Annex.Content
import Config
import Config.Cost
import Remote.Helper.Git
import Remote.Helper.Encryptable
import Remote.Helper.Special
import Remote.Helper.Messages
import qualified Remote.Helper.Ssh as Ssh
import Utility.Metered
import Crypto
import Annex.UUID
import Annex.Ssh
import qualified Remote.Rsync
import Utility.Rsync
import Utility.Tmp
import Logs.Remote
import Logs.Transfer
import Utility.Gpg
import Annex.Content

remote :: RemoteType
remote = RemoteType {
	typename = "gcrypt",
	-- Remote.Git takes care of enumerating gcrypt remotes too,
	-- and will call our gen on them.
	enumerate = return [],
	generate = gen,
	setup = gCryptSetup
}

gen :: Git.Repo -> UUID -> RemoteConfig -> RemoteGitConfig -> Annex (Maybe Remote)
gen gcryptr u c gc = do
	g <- gitRepo
	-- get underlying git repo with real path, not gcrypt path
	r <- liftIO $ Git.GCrypt.encryptedRemote g gcryptr
	let r' = r { Git.remoteName = Git.remoteName gcryptr }
	-- doublecheck that cache matches underlying repo's gcrypt-id
	-- (which might not be set), only for local repos
	(mgcryptid, r'') <- getGCryptId True r'
	case (mgcryptid, Git.GCrypt.remoteRepoId g (Git.remoteName gcryptr)) of
		(Just gcryptid, Just cachedgcryptid)
			| gcryptid /= cachedgcryptid -> resetup gcryptid r''
		_ -> gen' r'' u c gc
  where
	-- A different drive may have been mounted, making a different
	-- gcrypt remote available. So need to set the cached
	-- gcrypt-id and annex-uuid of the remote to match the remote
	-- that is now available. Also need to set the gcrypt particiants
	-- correctly.
	resetup gcryptid r = do
		let u' = genUUIDInNameSpace gCryptNameSpace gcryptid
		v <- M.lookup u' <$> readRemoteLog
		case (Git.remoteName gcryptr, v) of
			(Just remotename, Just c') -> do
				setGcryptEncryption c' remotename
				setConfig (remoteConfig gcryptr "uuid") (fromUUID u')
				setConfig (ConfigKey $ Git.GCrypt.remoteConfigKey "gcrypt-id" remotename) gcryptid
				gen' r u' c' gc
			_ -> do
				warning $ "not using unknown gcrypt repository pointed to by remote " ++ Git.repoDescribe r
				return Nothing

gen' :: Git.Repo -> UUID -> RemoteConfig -> RemoteGitConfig -> Annex (Maybe Remote)
gen' r u c gc = do
	cst <- remoteCost gc $
		if repoCheap r then nearlyCheapRemoteCost else expensiveRemoteCost
	(rsynctransport, rsyncurl) <- rsyncTransportToObjects r
	let rsyncopts = Remote.Rsync.genRsyncOpts c gc rsynctransport rsyncurl
	let this = Remote 
		{ uuid = u
		, cost = cst
		, name = Git.repoDescribe r
		, storeKey = \_ _ _ -> noCrypto
		, retrieveKeyFile = \_ _ _ _ -> noCrypto
		, retrieveKeyFileCheap = \_ _ -> return False
		, removeKey = remove this rsyncopts
		, hasKey = checkPresent this rsyncopts
		, hasKeyCheap = repoCheap r
		, whereisKey = Nothing
		, remoteFsck = Nothing
		, repairRepo = Nothing
		, config = c
		, localpath = localpathCalc r
		, repo = r
		, gitconfig = gc { remoteGitConfig = Just $ extractGitConfig r }
		, readonly = Git.repoIsHttp r
		, globallyAvailable = globallyAvailableCalc r
		, remotetype = remote
	}
	return $ Just $ encryptableRemote c
		(store this rsyncopts)
		(retrieve this rsyncopts)
		this

rsyncTransportToObjects :: Git.Repo -> Annex ([CommandParam], String)
rsyncTransportToObjects r = do
	(rsynctransport, rsyncurl, _) <- rsyncTransport r
	return (rsynctransport, rsyncurl ++ "/annex/objects")

rsyncTransport :: Git.Repo -> Annex ([CommandParam], String, AccessMethod)
rsyncTransport r
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
		opts <- sshCachingOptions (host, Nothing) []
		return (rsyncShell $ Param "ssh" : opts, host ++ ":" ++ rsyncpath, AccessShell)
	othertransport = return ([], loc, AccessDirect)

noCrypto :: Annex a
noCrypto = error "cannot use gcrypt remote without encryption enabled"

unsupportedUrl :: Annex a
unsupportedUrl = error "using non-ssh remote repo url with gcrypt is not supported"

gCryptSetup :: Maybe UUID -> RemoteConfig -> Annex (RemoteConfig, UUID)
gCryptSetup mu c = go $ M.lookup "gitrepo" c
  where
	remotename = fromJust (M.lookup "name" c)
  	go Nothing = error "Specify gitrepo="
	go (Just gitrepo) = do
		c' <- encryptionSetup c
		inRepo $ Git.Command.run 
			[ Params "remote add"
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
			, Param $ show Annex.Branch.fullname
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
					else error $ "uuid mismatch " ++ show (u, mu, gcryptid)

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
		(_, _, accessmethod) <- rsyncTransport r
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
		(rsynctransport, rsyncurl, _) <- rsyncTransport r
		let tmpconfig = tmp </> "config"
		void $ liftIO $ rsync $ rsynctransport ++
			[ Param $ rsyncurl ++ "/config"
			, Param tmpconfig
			]
		liftIO $ do
			void $ Git.Config.changeFile tmpconfig coreGCryptId gcryptid
			void $ Git.Config.changeFile tmpconfig denyNonFastForwards (Git.Config.boolConfig False)
		ok <- liftIO $ rsync $ rsynctransport ++
			[ Params "--recursive"
			, Param $ tmp ++ "/"
			, Param rsyncurl
			]
		unless ok $
			error "Failed to connect to remote to set it up."
		return AccessDirect

	{-  Ask git-annex-shell to configure the repository as a gcrypt
	 -  repository. May fail if it is too old. -}
	gitannexshellsetup = Ssh.onRemote r (boolSystem, False)
		"gcryptsetup" [ Param gcryptid ] []

	denyNonFastForwards = "receive.denyNonFastForwards"

shellOrRsync :: Remote -> Annex a -> Annex a -> Annex a
shellOrRsync r ashell arsync = case method of
	AccessShell -> ashell
	_ -> arsync
  where
  	method = toAccessMethod $ fromMaybe "" $
		remoteAnnexGCrypt $ gitconfig r

{- Configure gcrypt to use the same list of keyids that
 - were passed to initremote as its participants.
 - Also, configure it to use a signing key that is in the list of
 - participants, which gcrypt requires is the case, and may not be
 - depending on system configuration.
 -
 - (For shared encryption, gcrypt's default behavior is used.) -}
setGcryptEncryption :: RemoteConfig -> String -> Annex ()
setGcryptEncryption c remotename = do
	let participants = ConfigKey $ Git.GCrypt.remoteParticipantConfigKey remotename
	case extractCipher c of
		Nothing -> noCrypto
		Just (EncryptedCipher _ _ (KeyIds { keyIds = ks})) -> do
			setConfig participants (unwords ks)
			let signingkey = ConfigKey $ Git.GCrypt.remoteSigningKey remotename
			skeys <- M.keys <$> liftIO secretKeys
			case filter (`elem` ks) skeys of
				[] -> noop
				(k:_) -> setConfig signingkey k
		Just (SharedCipher _) ->
			unsetConfig participants

store :: Remote -> Remote.Rsync.RsyncOpts -> (Cipher, Key) -> Key -> MeterUpdate -> Annex Bool
store r rsyncopts (cipher, enck) k p
	| not $ Git.repoIsUrl (repo r) = guardUsable (repo r) False $
		metered (Just p) k $ \meterupdate -> spoolencrypted $ \h -> do
			let dest = gCryptLocation r enck
			createDirectoryIfMissing True $ parentDir dest
			readBytes (meteredWriteFile meterupdate dest) h
			return True
	| Git.repoIsSsh (repo r) = shellOrRsync r storeshell storersync
	| otherwise = unsupportedUrl
  where
  	gpgopts = getGpgEncParams r
	storersync = Remote.Rsync.storeEncrypted rsyncopts gpgopts (cipher, enck) k p
	storeshell = withTmp enck $ \tmp ->
		ifM (spoolencrypted $ readBytes $ \b -> catchBoolIO $ L.writeFile tmp b >> return True)
			( Ssh.rsyncHelper (Just p)
				=<< Ssh.rsyncParamsRemote False r Upload enck tmp Nothing
			, return False
			)
	spoolencrypted a = Annex.Content.sendAnnex k noop $ \src ->
		liftIO $ catchBoolIO $
			encrypt gpgopts cipher (feedFile src) a

retrieve :: Remote -> Remote.Rsync.RsyncOpts -> (Cipher, Key) -> Key -> FilePath -> MeterUpdate -> Annex Bool
retrieve r rsyncopts (cipher, enck) k d p
	| not $ Git.repoIsUrl (repo r) = guardUsable (repo r) False $ do
		retrievewith $ L.readFile src
		return True
	| Git.repoIsSsh (repo r) = shellOrRsync r retrieveshell retrieversync
	| otherwise = unsupportedUrl
  where
	src = gCryptLocation r enck
	retrievewith a = metered (Just p) k $ \meterupdate -> liftIO $
		a >>= \b -> 
			decrypt cipher (feedBytes b)
				(readBytes $ meteredWriteFile meterupdate d)
	retrieversync = Remote.Rsync.retrieveEncrypted rsyncopts (cipher, enck) k d p
	retrieveshell = withTmp enck $ \tmp ->
		ifM (Ssh.rsyncHelper (Just p) =<< Ssh.rsyncParamsRemote False r Download enck tmp Nothing)
			( liftIO $ catchBoolIO $ do
				decrypt cipher (feedFile tmp) $
					readBytes $ L.writeFile d
				return True
			, return False
			)

remove :: Remote -> Remote.Rsync.RsyncOpts -> Key -> Annex Bool
remove r rsyncopts k
	| not $ Git.repoIsUrl (repo r) = guardUsable (repo r) False $ do
		liftIO $ removeDirectoryRecursive $ parentDir $ gCryptLocation r k
		return True
	| Git.repoIsSsh (repo r) = shellOrRsync r removeshell removersync
	| otherwise = unsupportedUrl
  where
	removersync = Remote.Rsync.remove rsyncopts k
	removeshell = Ssh.dropKey (repo r) k

checkPresent :: Remote -> Remote.Rsync.RsyncOpts -> Key -> Annex (Either String Bool)
checkPresent r rsyncopts k
	| not $ Git.repoIsUrl (repo r) =
		guardUsable (repo r) (cantCheck $ repo r) $
			liftIO $ catchDefaultIO (cantCheck $ repo r) $
				Right <$> doesFileExist (gCryptLocation r k)
	| Git.repoIsSsh (repo r) = shellOrRsync r checkshell checkrsync
	| otherwise = unsupportedUrl
  where
  	checkrsync = Remote.Rsync.checkPresent (repo r) rsyncopts k
	checkshell = Ssh.inAnnex (repo r) k

{- Annexed objects are hashed using lower-case directories for max
 - portability. -}
gCryptLocation :: Remote -> Key -> FilePath
gCryptLocation r key = Git.repoLocation (repo r) </> objectDir </> keyPath key hashDirLower

data AccessMethod = AccessDirect | AccessShell

fromAccessMethod :: AccessMethod -> String
fromAccessMethod AccessShell = "shell"
fromAccessMethod AccessDirect = "true"

toAccessMethod :: String -> AccessMethod
toAccessMethod "shell" = AccessShell
toAccessMethod _ = AccessDirect

getGCryptUUID :: Bool -> Git.Repo -> Annex (Maybe UUID)
getGCryptUUID fast r = (genUUIDInNameSpace gCryptNameSpace <$>) . fst
	<$> getGCryptId fast r

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
getGCryptId :: Bool -> Git.Repo -> Annex (Maybe Git.GCrypt.GCryptId, Git.Repo)
getGCryptId fast r
	| Git.repoIsLocal r || Git.repoIsLocalUnknown r = extract <$>
		liftIO (catchMaybeIO $ Git.Config.read r)
	| not fast = extract . liftM fst <$> getM (eitherToMaybe <$>)
		[ Ssh.onRemote r (Git.Config.fromPipe r, Left undefined) "configlist" [] []
		, getConfigViaRsync r
		]
	| otherwise = return (Nothing, r)
  where
	extract Nothing = (Nothing, r)
	extract (Just r') = (Git.Config.getMaybe coreGCryptId r', r')

getConfigViaRsync :: Git.Repo -> Annex (Either SomeException (Git.Repo, String))
getConfigViaRsync r = do
	(rsynctransport, rsyncurl, _) <- rsyncTransport r
	liftIO $ do
		withTmpFile "tmpconfig" $ \tmpconfig _ -> do
			void $ rsync $ rsynctransport ++
				[ Param $ rsyncurl ++ "/config"
				, Param tmpconfig
				]
			Git.Config.fromFile r tmpconfig
