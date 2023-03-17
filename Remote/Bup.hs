{- Using bup as a remote.
 -
 - Copyright 2011-2022 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE RankNTypes, OverloadedStrings #-}

module Remote.Bup (remote) where

import qualified Data.Map as M
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified System.FilePath.ByteString as P
import Data.ByteString.Lazy.UTF8 (fromString)
import Control.Concurrent.Async

import Annex.Common
import qualified Annex
import Types.Remote
import Types.Creds
import Git.Types (ConfigValue(..), fromConfigKey)
import qualified Git
import qualified Git.Command
import qualified Git.Config
import qualified Git.Construct
import qualified Git.Ref
import Config
import Config.Cost
import qualified Remote.Helper.Ssh as Ssh
import Annex.SpecialRemote.Config
import Remote.Helper.Special
import Remote.Helper.ExportImport
import Utility.Hash
import Utility.UserInfo
import Annex.UUID
import Annex.Ssh
import Annex.LockFile
import Annex.Perms
import Utility.Metered
import Types.ProposedAccepted

type BupRepo = String

remote :: RemoteType
remote = specialRemoteType $ RemoteType
	{ typename = "bup"
	, enumerate = const (findSpecialRemotes "buprepo")
	, generate = gen
	, configParser = mkRemoteConfigParser
		[ optionalStringParser buprepoField
			(FieldDesc "(required) bup repository to use")
		]
	, setup = bupSetup
	, exportSupported = exportUnsupported
	, importSupported = importUnsupported
	, thirdPartyPopulated = False
	}

buprepoField :: RemoteConfigField
buprepoField = Accepted "buprepo"

gen :: Git.Repo -> UUID -> RemoteConfig -> RemoteGitConfig -> RemoteStateHandle -> Annex (Maybe Remote)
gen r u rc gc rs = do
	c <- parsedRemoteConfig remote rc
	bupr <- liftIO $ bup2GitRemote buprepo
	cst <- remoteCost gc c $
		if bupLocal buprepo
			then nearlyCheapRemoteCost
			else expensiveRemoteCost
	(u', bupr') <- getBupUUID bupr u
	
	let this = Remote
		{ uuid = u'
		, cost = cst
		, name = Git.repoDescribe r
		, storeKey = storeKeyDummy
		, retrieveKeyFile = retrieveKeyFileDummy
		, retrieveKeyFileCheap = Nothing
		-- Bup uses git, which cryptographically verifies content
		-- (with SHA1, but sufficiently for this).
		, retrievalSecurityPolicy = RetrievalAllKeysSecure
		, removeKey = removeKeyDummy
		, lockContent = Nothing
		, checkPresent = checkPresentDummy
		, checkPresentCheap = bupLocal buprepo
		, exportActions = exportUnsupported
		, importActions = importUnsupported
		, whereisKey = Nothing
		, remoteFsck = Nothing
		, repairRepo = Nothing
		, config = c
		, getRepo = return r
		, gitconfig = gc
		, localpath = if bupLocal buprepo && not (null buprepo)
			then Just buprepo
			else Nothing
		, remotetype = remote
		, availability = if bupLocal buprepo then LocallyAvailable else GloballyAvailable
		, readonly = False
		, appendonly = False
		, untrustworthy = False
		, mkUnavailable = return Nothing
		, getInfo = return [("repo", buprepo)]
		, claimUrl = Nothing
		, checkUrl = Nothing
		, remoteStateHandle = rs
		}
	let specialcfg = (specialRemoteCfg c)
		-- chunking would not improve bup
		{ chunkConfig = NoChunks
		}
	return $ Just $ specialRemote' specialcfg c
		(store this buprepo)
		(retrieve this buprepo)
		(remove buprepo)
		(checkKey bupr')
		this
  where
	buprepo = fromMaybe (giveup "missing buprepo") $ remoteAnnexBupRepo gc

bupSetup :: SetupStage -> Maybe UUID -> Maybe CredPair -> RemoteConfig -> RemoteGitConfig -> Annex (RemoteConfig, UUID)
bupSetup _ mu _ c gc = do
	u <- maybe (liftIO genUUID) return mu

	-- verify configuration is sane
	let buprepo = maybe (giveup "Specify buprepo=") fromProposedAccepted $
		M.lookup buprepoField c
	(c', _encsetup) <- encryptionSetup c gc

	-- bup init will create the repository.
	-- (If the repository already exists, bup init again appears safe.)
	showAction "bup init"
	unlessM (bup "init" buprepo []) $ giveup "bup init failed"

	storeBupUUID u buprepo

	-- The buprepo is stored in git config, as well as this repo's
	-- persistent state, so it can vary between hosts.
	gitConfigSpecialRemote u c' [("buprepo", buprepo)]

	return (c', u)

bupParams :: String -> BupRepo -> [CommandParam] -> [CommandParam]
bupParams command buprepo params = 
	Param command : [Param "-r", Param buprepo] ++ params

bup :: String -> BupRepo -> [CommandParam] -> Annex Bool
bup command buprepo params = do
	showOutput -- make way for bup output
	liftIO $ boolSystem "bup" $ bupParams command buprepo params

bupSplitParams :: Remote -> BupRepo -> Key -> [CommandParam] -> [CommandParam]
bupSplitParams r buprepo k src =
	let os = map Param $ remoteAnnexBupSplitOptions $ gitconfig r
	in bupParams "split" buprepo 
		(os ++ [Param "-q", Param "-n", Param (bupRef k)] ++ src)

store :: Remote -> BupRepo -> Storer
store r buprepo = byteStorer $ \k b p -> lockBup True r $ do
	liftIO $ withNullHandle $ \nullh ->
		let params = bupSplitParams r buprepo k []
		    cmd = (proc "bup" (toCommand params))
			{ std_in = CreatePipe
			, std_out = UseHandle nullh
			, std_err = CreatePipe
			}
		    feeder = \h -> do
			meteredWrite p (S.hPut h) b
			hClose h
		in withCreateProcess cmd (go feeder cmd)
  where
	go feeder p (Just inh) _ (Just errh) pid = do
		-- bup split is noisy to stderr even with the -q
		-- option. But when bup fails, the stderr needs
		-- to be displayed.
		(feedresult, erroutput) <- tryNonAsync (feeder inh)
			`concurrently` hGetContentsStrict errh
		waitForProcess pid >>= \case
			ExitSuccess -> case feedresult of
				Right () -> return ()
				Left e -> throwM e
			ExitFailure n -> giveup $ 
				showCmd p ++ " exited " ++ show n ++
					" (stderr output: " ++ erroutput ++ ")"
	go _ _ _ _ _ _ = error "internal"

retrieve :: Remote -> BupRepo -> Retriever
retrieve r buprepo = byteRetriever $ \k sink -> lockBup True r $ do
	let params = bupParams "join" buprepo [Param $ bupRef k]
	let p = (proc "bup" (toCommand params))
		{ std_out = CreatePipe }
	bracketIO (createProcess p) cleanupProcess (go sink p)
  where
	go sink p (_, Just h, _, pid) = do
		v <- sink =<< liftIO (L.hGetContents h)
		liftIO $ do
			hClose h
			forceSuccessProcess p pid
		return v
	go _ _ _ = error "internal"

{- Cannot revert having stored a key in bup, but at least the data for the
 - key will be used for deltaing data of other keys stored later.
 -
 - We can, however, remove the git branch that bup created for the key.
 -}
remove :: BupRepo -> Remover
remove buprepo k = do
	go =<< liftIO (bup2GitRemote buprepo)
	warning "content cannot be completely removed from bup remote"
  where
	go r
		| Git.repoIsUrl r = void $ onBupRemote r boolSystem "git" params
		| otherwise = void $ liftIO $ catchMaybeIO $ do
			r' <- Git.Config.read r
			boolSystem "git" $ Git.Command.gitCommandLine params r'
	params = [ Param "branch", Param "-q", Param "-D", Param (bupRef k) ]

{- Bup does not provide a way to tell if a given dataset is present
 - in a bup repository. One way it to check if the git repository has
 - a branch matching the name (as created by bup split -n).
 -}
checkKey :: Git.Repo -> CheckPresent
checkKey bupr k
	| Git.repoIsUrl bupr = onBupRemote bupr boolSystem "git" params
	| otherwise = liftIO $ boolSystem "git" $
		Git.Command.gitCommandLine params bupr
  where
	params = 
		[ Param "show-ref"
		, Param "--quiet"
		, Param "--verify"
		, Param $ "refs/heads/" ++ bupRef k
		]

{- Store UUID in the annex.uuid setting of the bup repository. -}
storeBupUUID :: UUID -> BupRepo -> Annex ()
storeBupUUID u buprepo = do
	r <- liftIO $ bup2GitRemote buprepo
	if Git.repoIsUrl r
		then do
			showAction "storing uuid"
			unlessM (onBupRemote r boolSystem "git"
				[Param "config", Param (fromConfigKey configkeyUUID), Param v]) $
					giveup "ssh failed"
		else liftIO $ do
			r' <- Git.Config.read r
			let noolduuid = case Git.Config.get configkeyUUID mempty r' of
				ConfigValue olduuid -> S.null olduuid
				NoConfigValue -> True
			when noolduuid $
				Git.Command.run
					[ Param "config"
					, Param "annex.uuid"
					, Param v
					] r'
  where
	v = fromUUID u

onBupRemote :: Git.Repo -> (FilePath -> [CommandParam] -> IO a) -> FilePath -> [CommandParam] -> Annex a
onBupRemote r runner command params = do
	c <- Annex.getRemoteGitConfig r
	let remotecmd = "cd " ++ dir ++ " && " ++ unwords (command : toCommand params)
	(sshcmd, sshparams) <- Ssh.toRepo NoConsumeStdin r c remotecmd
	liftIO $ runner sshcmd sshparams
  where
	path = fromRawFilePath $ Git.repoPath r
	base = fromMaybe path (stripPrefix "/~/" path)
	dir = shellEscape base

{- Allow for bup repositories on removable media by checking
 - local bup repositories to see if they are available, and getting their
 - uuid (which may be different from the stored uuid for the bup remote).
 -
 - If a bup repository is not available, returns NoUUID.
 - This will cause checkPresent to indicate nothing from the bup remote
 - is known to be present.
 -
 - Also, returns a version of the repo with config read, if it is local.
 -}
getBupUUID :: Git.Repo -> UUID -> Annex (UUID, Git.Repo)
getBupUUID r u
	| Git.repoIsUrl r = return (u, r)
	| otherwise = liftIO $ do
		ret <- tryIO $ Git.Config.read r
		case ret of
			Right r' -> return (toUUID $ Git.Config.get configkeyUUID mempty r', r')
			Left _ -> return (NoUUID, r)

{- Converts a bup remote path spec into a Git.Repo. There are some
 - differences in path representation between git and bup. -}
bup2GitRemote :: BupRepo -> IO Git.Repo
bup2GitRemote "" = do
	-- bup -r "" operates on ~/.bup
	h <- myHomeDir
	Git.Construct.fromPath $ toRawFilePath $ h </> ".bup"
bup2GitRemote r
	| bupLocal r = 
		if "/" `isPrefixOf` r
			then Git.Construct.fromPath (toRawFilePath r)
			else giveup "please specify an absolute path"
	| otherwise = Git.Construct.fromUrl $ "ssh://" ++ host ++ slash dir
  where
	bits = splitc ':' r
	host = Prelude.head bits
	dir = intercalate ":" $ drop 1 bits
	-- "host:~user/dir" is not supported specially by bup;
	-- "host:dir" is relative to the home directory;
	-- "host:" goes in ~/.bup
	slash d
		| null d = "/~/.bup"
		| "/" `isPrefixOf` d = d
		| otherwise = "/~/" ++ d

{- Converts a key into a git ref name, which bup-split -n will use to point
 - to it. -}
bupRef :: Key -> String
bupRef k
	| Git.Ref.legal True shown = shown
	| otherwise = "git-annex-" ++ show (sha2_256 (fromString shown))
  where
	shown = serializeKey k

bupLocal :: BupRepo -> Bool
bupLocal = notElem ':'

{- Bup is not concurrency safe, so use a lock file. Only one writer process
 - should run at a time; multiple readers may run if no writer is running. -}
lockBup :: Bool -> Remote -> Annex a -> Annex a
lockBup writer r a = do
	dir <- fromRepo gitAnnexRemotesDir
	unlessM (liftIO $ doesDirectoryExist (fromRawFilePath dir)) $
		createAnnexDirectory dir
	let remoteid = fromUUID (uuid r)
	let lck = dir P.</> remoteid <> ".lck"
	if writer
		then withExclusiveLock lck a
		else withSharedLock lck a
