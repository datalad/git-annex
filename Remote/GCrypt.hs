{- git remotes encrypted using git-remote-gcrypt
 -
 - Copyright 2013-2020 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Remote.GCrypt (
	remote,
	chainGen,
	getGCryptUUID,
	coreGCryptId,
	setupRepo,
	accessShellConfig,
	setGcryptEncryption,
) where

import qualified Data.Map as M
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified System.FilePath.ByteString as P
import Control.Exception
import Data.Default

import Annex.Common
import qualified Annex
import Types.Remote
import Types.GitConfig
import Types.Crypto
import Types.Creds
import Types.Transfer
import Git.Types (ConfigKey(..), fromConfigKey, fromConfigValue)
import qualified Git
import qualified Git.Command
import qualified Git.Config
import qualified Git.GCrypt
import qualified Git.Construct
import qualified Annex.Branch
import Config
import Config.Cost
import Annex.SpecialRemote.Config
import Remote.Helper.Git
import Remote.Helper.Encryptable
import Remote.Helper.Special
import Remote.Helper.Messages
import Remote.Helper.ExportImport
import qualified Remote.Helper.Ssh as Ssh
import Utility.Metered
import Annex.UUID
import Annex.Ssh
import Annex.Perms
import qualified Remote.Rsync
import qualified Remote.Directory
import Utility.Rsync
import Utility.Tmp
import Logs.Remote
import Utility.Gpg
import Utility.SshHost
import Utility.Tuple
import Utility.Directory.Create
import Messages.Progress
import Types.ProposedAccepted

remote :: RemoteType
remote = specialRemoteType $ RemoteType
	{ typename = "gcrypt"
	-- Remote.Git takes care of enumerating gcrypt remotes too,
	-- and will call our chainGen on them.
	, enumerate = const (return [])
	, generate = gen
	, configParser = mkRemoteConfigParser $
		Remote.Rsync.rsyncRemoteConfigs ++
		[ optionalStringParser gitRepoField
			(FieldDesc "(required) path or url to gcrypt repository")
		]
	, setup = gCryptSetup
	, exportSupported = exportUnsupported
	, importSupported = importUnsupported
	, thirdPartyPopulated = False
	}

gitRepoField :: RemoteConfigField
gitRepoField = Accepted "gitrepo"

chainGen :: Git.Repo -> UUID -> RemoteConfig -> RemoteGitConfig -> RemoteStateHandle -> Annex (Maybe Remote)
chainGen gcryptr u rc gc rs = do
	g <- gitRepo
	-- get underlying git repo with real path, not gcrypt path
	r <- liftIO $ Git.GCrypt.encryptedRemote g gcryptr
	let r' = r { Git.remoteName = Git.remoteName gcryptr }
	gen r' u rc gc rs

gen :: Git.Repo -> UUID -> RemoteConfig -> RemoteGitConfig -> RemoteStateHandle -> Annex (Maybe Remote)
gen baser u rc gc rs = do
	-- doublecheck that cache matches underlying repo's gcrypt-id
	-- (which might not be set), only for local repos
	(mgcryptid, r) <- getGCryptId True baser gc
	g <- gitRepo
	case (mgcryptid, Git.GCrypt.remoteRepoId g (Git.remoteName baser)) of
		(Just gcryptid, Just cachedgcryptid)
			| gcryptid /= cachedgcryptid -> resetup gcryptid r
		_ -> do
			c <- parsedRemoteConfig remote rc
			gen' r u c gc rs
  where
	-- A different drive may have been mounted, making a different
	-- gcrypt remote available. So need to set the cached
	-- gcrypt-id and annex-uuid of the remote to match the remote
	-- that is now available. Also need to set the gcrypt particiants
	-- correctly.
	resetup gcryptid r = do
		let u' = genUUIDInNameSpace gCryptNameSpace gcryptid
		v <- M.lookup u' <$> remoteConfigMap
		case (Git.remoteName baser, v) of
			(Just remotename, Just rc') -> do
				pc <- parsedRemoteConfig remote rc'
				setGcryptEncryption pc remotename
				storeUUIDIn (remoteAnnexConfig baser "uuid") u'
				setConfig (Git.GCrypt.remoteConfigKey "gcrypt-id" remotename) gcryptid
				gen' r u' pc gc rs
			_ -> do
				warning $ UnquotedString $ "not using unknown gcrypt repository pointed to by remote " ++ Git.repoDescribe r
				return Nothing

gen' :: Git.Repo -> UUID -> ParsedRemoteConfig -> RemoteGitConfig -> RemoteStateHandle -> Annex (Maybe Remote)
gen' r u c gc rs = do
	cst <- remoteCost gc c $
		if repoCheap r
			then nearlyCheapRemoteCost
			else expensiveRemoteCost
	let (rsynctransport, rsyncurl, accessmethod) = rsyncTransportToObjects r gc
	protectsargs <- liftIO Remote.Rsync.probeRsyncProtectsArgs
	let rsyncopts = Remote.Rsync.genRsyncOpts protectsargs c gc rsynctransport rsyncurl
	let this = Remote 
		{ uuid = u
		, cost = cst
		, name = Git.repoDescribe r
		, storeKey = storeKeyDummy
		, retrieveKeyFile = retrieveKeyFileDummy
		, retrieveKeyFileCheap = Nothing
		, retrievalSecurityPolicy = RetrievalAllKeysSecure
		, removeKey = removeKeyDummy
		, lockContent = Nothing
		, checkPresent = checkPresentDummy
		, checkPresentCheap = repoCheap r
		, exportActions = exportUnsupported
		, importActions = importUnsupported
		, whereisKey = Nothing
		, remoteFsck = Nothing
		, repairRepo = Nothing
		, config = c
		, localpath = localpathCalc r
		, getRepo = return r
		, gitconfig = gc
		, readonly = Git.repoIsHttp r
		, appendonly = False
		, untrustworthy = False
		, availability = availabilityCalc r
		, remotetype = remote
		, mkUnavailable = return Nothing
		, getInfo = gitRepoInfo this
		, claimUrl = Nothing
		, checkUrl = Nothing
		, remoteStateHandle = rs
	}
	return $ Just $ specialRemote' specialcfg c
		(store this rsyncopts accessmethod)
		(retrieve this rsyncopts accessmethod)
		(remove this rsyncopts accessmethod)
		(checkKey this rsyncopts accessmethod)
		this
  where
	specialcfg
		| Git.repoIsUrl r = (specialRemoteCfg c)
			-- Rsync displays its own progress.
			{ displayProgress = False }
		| otherwise = specialRemoteCfg c

rsyncTransportToObjects :: Git.Repo -> RemoteGitConfig -> (Annex [CommandParam], String, AccessMethod)
rsyncTransportToObjects r gc =
	let (rsynctransport, rsyncurl, m) = rsyncTransport r gc
	in (rsynctransport, rsyncurl ++ "/annex/objects", m)

rsyncTransport :: Git.Repo -> RemoteGitConfig -> (Annex [CommandParam], String, AccessMethod)
rsyncTransport r gc
	| sshprefix `isPrefixOf` loc = sshtransport $ break (== '/') $ drop (length sshprefix) loc
	| "rsync://" `isPrefixOf` loc = rsyncoversshtransport
	| ":" `isInfixOf` loc = sshtransport $ separate (== ':') loc
	| otherwise = rsyncoversshtransport
  where
	sshprefix = "ssh://" :: String
	loc = Git.repoLocation r
	sshtransport (host, path) =
		let rsyncpath = if "/~/" `isPrefixOf` path
			then drop 3 path
			else path
		    sshhost = either giveup id (mkSshHost host)
		    mkopts = rsyncShell . (Param "ssh" :) 
			<$> sshOptions ConsumeStdin (sshhost, Nothing) gc []
		in (mkopts, fromSshHost sshhost ++ ":" ++ rsyncpath, AccessGitAnnexShell)
	rsyncoversshtransport =
		-- git-remote-gcrypt uses a rsync:// url to mean
		-- rsync over ssh. But to rsync, that's rsync protocol,
		-- so it must be converted to a form that rsync will treat
		-- as rsync over ssh.
		-- There are two url forms that git-remote-gcrypt
		-- supports:  rsync://userhost/path and rsync://userhost:path
		-- change to: userhost:/path            userhost:path
		let loc' = replace "rsync://" "" loc
		    loc'' = if ':' `elem` loc'
		    	then loc'
		    	else let (a, b) = break (== '/') loc' in a ++ ":" ++ b
		in (pure [], loc'', AccessRsyncOverSsh)

noCrypto :: Annex a
noCrypto = giveup "cannot use gcrypt remote without encryption enabled"

unsupportedUrl :: a
unsupportedUrl = giveup "unsupported repo url for gcrypt"

gCryptSetup :: SetupStage -> Maybe UUID -> Maybe CredPair -> RemoteConfig -> RemoteGitConfig -> Annex (RemoteConfig, UUID)
gCryptSetup _ mu _ c gc = go $ fromProposedAccepted <$> M.lookup gitRepoField c
  where
	remotename = fromJust (lookupName c)
	go Nothing = giveup "Specify gitrepo="
	go (Just gitrepo) = do
		(c', _encsetup) <- encryptionSetup c gc

		let url = Git.GCrypt.urlPrefix ++ gitrepo
		rs <- Annex.getGitRemotes
		case filter (\r -> Git.remoteName r == Just remotename) rs of
			[] -> inRepo $ Git.Command.run 
				[ Param "remote", Param "add"
				, Param remotename
				, Param url
				]
			(r:_)
				| Git.repoLocation r == url -> noop
				| otherwise -> giveup "Another remote with the same name already exists."		

		pc <- either giveup return . parseRemoteConfig c'
			=<< configParser remote c'
		setGcryptEncryption pc remotename

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
			Nothing -> giveup "unable to determine gcrypt-id of remote"
			Just gcryptid -> do
				let u = genUUIDInNameSpace gCryptNameSpace gcryptid
				if Just u == mu || isNothing mu
					then do
						method <- setupRepo gcryptid =<< inRepo (Git.Construct.fromRemoteLocation gitrepo False)
						gitConfigSpecialRemote u c' [("gcrypt", fromAccessMethod method)]
						return (c', u)
					else giveup $ "uuid mismatch; expected " ++ show mu ++ " but remote gitrepo has " ++ show u ++ " (" ++ show gcryptid ++ ")"

{- Sets up the gcrypt repository. The repository is either a local
 - repo, or it is accessed via rsync over ssh (without using
 - git-annex-shell), or it is accessed over ssh and git-annex-shell
 - is available to manage it.
 -
 - The GCryptID is recorded in the repository's git config for later use.
 - Also, if the git config has receive.denyNonFastForwards set, disable
 - it; gcrypt relies on being able to fast-forward branches.
 -}
setupRepo :: Git.GCrypt.GCryptId -> Git.Repo -> Annex AccessMethod
setupRepo gcryptid r
	| Git.repoIsUrl r = do
		dummycfg <- liftIO dummyRemoteGitConfig
		let (_, _, accessmethod) = rsyncTransport r dummycfg
		case accessmethod of
			AccessRsyncOverSsh -> rsyncsetup
			AccessGitAnnexShell -> ifM gitannexshellsetup
				( return AccessGitAnnexShell
				, rsyncsetup
				)
	| Git.repoIsLocalUnknown r = localsetup =<< liftIO (Git.Config.read r)
	| otherwise = localsetup r
  where
	localsetup r' = do
		let setconfig k v = liftIO $ Git.Command.run [Param "config", Param (fromConfigKey k), Param v] r'
		setconfig coreGCryptId gcryptid
		setconfig denyNonFastForwards (Git.Config.boolConfig False)
		return AccessRsyncOverSsh

	{- As well as modifying the remote's git config, 
	 - create the objectDir on the remote,
	 - which is needed for rsync of objects to it to work.
	 -}
	rsyncsetup = Remote.Rsync.withRsyncScratchDir $ \tmp -> do
		createAnnexDirectory (toRawFilePath tmp P.</> objectDir)
		dummycfg <- liftIO dummyRemoteGitConfig
		let (rsynctransport, rsyncurl, _) = rsyncTransport r dummycfg
		let tmpconfig = tmp </> "config"
		opts <- rsynctransport
		void $ liftIO $ rsync $ opts ++
			[ Param $ rsyncurl ++ "/config"
			, Param tmpconfig
			]
		liftIO $ do
			void $ Git.Config.changeFile tmpconfig coreGCryptId (encodeBS gcryptid)
			void $ Git.Config.changeFile tmpconfig denyNonFastForwards (Git.Config.boolConfig' False)
		ok <- liftIO $ rsync $ opts ++
			[ Param "--recursive"
			, Param $ tmp ++ "/"
			, Param rsyncurl
			]
		unless ok $
			giveup "Failed to connect to remote to set it up."
		return AccessRsyncOverSsh

	{-  Ask git-annex-shell to configure the repository as a gcrypt
	 -  repository. May fail if it is too old. -}
	gitannexshellsetup = Ssh.onRemote NoConsumeStdin r
		(\f p -> liftIO (boolSystem f p), return False)
		"gcryptsetup" [ Param gcryptid ] []

	denyNonFastForwards = ConfigKey "receive.denyNonFastForwards"

accessShell :: Remote -> Bool
accessShell = accessShellConfig . gitconfig

accessShellConfig :: RemoteGitConfig -> Bool
accessShellConfig c = case method of
	AccessGitAnnexShell -> True
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
 - Also, sets gcrypt-publish-participants to avoid unnecessary gpg
 - passphrase prompts.
 -}
setGcryptEncryption :: ParsedRemoteConfig -> String -> Annex ()
setGcryptEncryption c remotename = do
	let participants = remoteconfig Git.GCrypt.remoteParticipantConfigKey
	case extractCipher c of
		Nothing -> noCrypto
		Just cip -> case cipherKeyIds cip of
			Nothing -> noop
			Just (KeyIds { keyIds = ks}) -> do
				setConfig participants (unwords ks)
				let signingkey = Git.GCrypt.remoteSigningKey remotename
				cmd <- gpgCmd <$> Annex.getGitConfig
				skeys <- M.keys <$> liftIO (secretKeys cmd)
				case filter (`elem` ks) skeys of
					[] -> noop
					(k:_) -> setConfig signingkey k
	setConfig (remoteconfig Git.GCrypt.remotePublishParticipantConfigKey)
		(Git.Config.boolConfig True)
  where
	remoteconfig n = n remotename

store :: Remote -> Remote.Rsync.RsyncOpts -> AccessMethod -> Storer
store r rsyncopts accessmethod k s p = do
	repo <- getRepo r
	store' repo r rsyncopts accessmethod k s p

store' :: Git.Repo -> Remote -> Remote.Rsync.RsyncOpts -> AccessMethod -> Storer
store' repo r rsyncopts accessmethod
	| not $ Git.repoIsUrl repo = 
		byteStorer $ \k b p -> guardUsable repo (giveup "cannot access remote") $ liftIO $ do
			let tmpdir = Git.repoPath repo P.</> "tmp" P.</> keyFile k
			void $ tryIO $ createDirectoryUnder [Git.repoPath repo] tmpdir
			let tmpf = tmpdir P.</> keyFile k
			meteredWriteFile p (fromRawFilePath tmpf) b
			let destdir = parentDir $ toRawFilePath $ gCryptLocation repo k
			Remote.Directory.finalizeStoreGeneric (Git.repoPath repo) tmpdir destdir
	| Git.repoIsSsh repo = if accessShell r
		then fileStorer $ \k f p -> do
			oh <- mkOutputHandler
			ok <- Ssh.rsyncHelper oh (Just p)
				=<< Ssh.rsyncParamsRemote r Upload k f
			unless ok $
				giveup "rsync failed"
		else storersync
	| accessmethod == AccessRsyncOverSsh = storersync
	| otherwise = unsupportedUrl
  where
	storersync = fileStorer $ Remote.Rsync.store rsyncopts

retrieve :: Remote -> Remote.Rsync.RsyncOpts -> AccessMethod -> Retriever
retrieve r rsyncopts accessmethod k p miv sink = do
	repo <- getRepo r
	retrieve' repo r rsyncopts accessmethod k p miv sink

retrieve' :: Git.Repo -> Remote -> Remote.Rsync.RsyncOpts -> AccessMethod -> Retriever
retrieve' repo r rsyncopts accessmethod
	| not $ Git.repoIsUrl repo = byteRetriever $ \k sink ->
		guardUsable repo (giveup "cannot access remote") $
			sink =<< liftIO (L.readFile $ gCryptLocation repo k)
	| Git.repoIsSsh repo = if accessShell r
		then fileRetriever $ \f k p -> do
			ps <- Ssh.rsyncParamsRemote r Download k
				(fromRawFilePath f)
			oh <- mkOutputHandler
			unlessM (Ssh.rsyncHelper oh (Just p) ps) $
				giveup "rsync failed"
		else retrieversync
	| accessmethod == AccessRsyncOverSsh = retrieversync
	| otherwise = unsupportedUrl
  where
	retrieversync = fileRetriever $ Remote.Rsync.retrieve rsyncopts

remove :: Remote -> Remote.Rsync.RsyncOpts -> AccessMethod -> Remover
remove r rsyncopts accessmethod k = do
	repo <- getRepo r
	remove' repo r rsyncopts accessmethod k

remove' :: Git.Repo -> Remote -> Remote.Rsync.RsyncOpts -> AccessMethod -> Remover
remove' repo r rsyncopts accessmethod k
	| not $ Git.repoIsUrl repo = guardUsable repo (giveup "cannot access remote") $
		liftIO $ Remote.Directory.removeDirGeneric True
			(gCryptTopDir repo)
			(fromRawFilePath (parentDir (toRawFilePath (gCryptLocation repo k))))
	| Git.repoIsSsh repo = shellOrRsync r removeshell removersync
	| accessmethod == AccessRsyncOverSsh = removersync
	| otherwise = unsupportedUrl
  where
	removersync = Remote.Rsync.remove rsyncopts k
	removeshell = Ssh.dropKey repo k

checkKey :: Remote -> Remote.Rsync.RsyncOpts -> AccessMethod -> CheckPresent
checkKey r rsyncopts accessmethod k = do
	repo <- getRepo r
	checkKey' repo r rsyncopts accessmethod k

checkKey' :: Git.Repo -> Remote -> Remote.Rsync.RsyncOpts -> AccessMethod -> CheckPresent
checkKey' repo r rsyncopts accessmethod k
	| not $ Git.repoIsUrl repo =
		guardUsable repo (cantCheck repo) $
			liftIO $ doesFileExist (gCryptLocation repo k)
	| Git.repoIsSsh repo = shellOrRsync r checkshell checkrsync
	| accessmethod == AccessRsyncOverSsh = checkrsync
	| otherwise = unsupportedUrl
  where
	checkrsync = Remote.Rsync.checkKey rsyncopts k
	checkshell = Ssh.inAnnex repo k

gCryptTopDir :: Git.Repo -> FilePath
gCryptTopDir repo = Git.repoLocation repo </> fromRawFilePath objectDir

{- Annexed objects are hashed using lower-case directories for max
 - portability. -}
gCryptLocation :: Git.Repo -> Key -> FilePath
gCryptLocation repo key = gCryptTopDir repo
	</> fromRawFilePath (keyPath key (hashDirLower def))

data AccessMethod = AccessRsyncOverSsh | AccessGitAnnexShell
	deriving (Eq)

fromAccessMethod :: AccessMethod -> String
fromAccessMethod AccessGitAnnexShell = "shell"
fromAccessMethod AccessRsyncOverSsh = "true"

toAccessMethod :: String -> AccessMethod
toAccessMethod "shell" = AccessGitAnnexShell
toAccessMethod _ = AccessRsyncOverSsh

getGCryptUUID :: Bool -> Git.Repo -> Annex (Maybe UUID)
getGCryptUUID fast r = do
	dummycfg <- liftIO dummyRemoteGitConfig
	(genUUIDInNameSpace gCryptNameSpace <$>) . fst
		<$> getGCryptId fast r dummycfg

coreGCryptId :: ConfigKey
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
	| not fast = extract . liftM fst3 <$> getM (eitherToMaybe <$>)
		[ Ssh.onRemote NoConsumeStdin r (\f p -> liftIO (Git.Config.fromPipe r f p Git.Config.ConfigList), return (Left $ giveup "configlist failed")) "configlist" [] []
		, getConfigViaRsync r gc
		]
	| otherwise = return (Nothing, r)
  where
	extract Nothing = (Nothing, r)
	extract (Just r') = (fromConfigValue <$> Git.Config.getMaybe coreGCryptId r', r')

getConfigViaRsync :: Git.Repo -> RemoteGitConfig -> Annex (Either SomeException (Git.Repo, S.ByteString, String))
getConfigViaRsync r gc = do
	let (rsynctransport, rsyncurl, _) = rsyncTransport r gc
	opts <- rsynctransport
	liftIO $ do
		withTmpFile "tmpconfig" $ \tmpconfig _ -> do
			void $ rsync $ opts ++
				[ Param $ rsyncurl ++ "/config"
				, Param tmpconfig
				]
			Git.Config.fromFile r tmpconfig
