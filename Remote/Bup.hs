{- Using bup as a remote.
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Remote.Bup (remote) where

import qualified Data.ByteString.Lazy as L
import qualified Data.Map as M
import System.Process
import Data.ByteString.Lazy.UTF8 (fromString)

import Common.Annex
import Types.Remote
import Types.Key
import qualified Git
import qualified Git.Command
import qualified Git.Config
import qualified Git.Construct
import qualified Git.Ref
import Config
import Config.Cost
import qualified Remote.Helper.Ssh as Ssh
import Remote.Helper.Special
import Remote.Helper.Encryptable
import Remote.Helper.Messages
import Crypto
import Utility.Hash
import Utility.UserInfo
import Annex.Content
import Annex.UUID
import Utility.Metered

type BupRepo = String

remote :: RemoteType
remote = RemoteType {
	typename = "bup",
	enumerate = findSpecialRemotes "buprepo",
	generate = gen,
	setup = bupSetup
}

gen :: Git.Repo -> UUID -> RemoteConfig -> RemoteGitConfig -> Annex (Maybe Remote)
gen r u c gc = do
	bupr <- liftIO $ bup2GitRemote buprepo
	cst <- remoteCost gc $
		if bupLocal buprepo
			then nearlyCheapRemoteCost
			else expensiveRemoteCost
	(u', bupr') <- getBupUUID bupr u
	
	let new = Remote
		{ uuid = u'
		, cost = cst
		, name = Git.repoDescribe r
		, storeKey = store new buprepo
		, retrieveKeyFile = retrieve buprepo
		, retrieveKeyFileCheap = retrieveCheap buprepo
		, removeKey = remove
		, hasKey = checkPresent r bupr'
		, hasKeyCheap = bupLocal buprepo
		, whereisKey = Nothing
		, remoteFsck = Nothing
		, repairRepo = Nothing
		, config = c
		, repo = r
		, gitconfig = gc
		, localpath = if bupLocal buprepo && not (null buprepo)
			then Just buprepo
			else Nothing
		, remotetype = remote
		, availability = if bupLocal buprepo then LocallyAvailable else GloballyAvailable
		, readonly = False
		}
	return $ Just $ encryptableRemote c
		(storeEncrypted new buprepo)
		(retrieveEncrypted buprepo)
		new
  where
	buprepo = fromMaybe (error "missing buprepo") $ remoteAnnexBupRepo gc

bupSetup :: Maybe UUID -> RemoteConfig -> Annex (RemoteConfig, UUID)
bupSetup mu c = do
	u <- maybe (liftIO genUUID) return mu

	-- verify configuration is sane
	let buprepo = fromMaybe (error "Specify buprepo=") $
		M.lookup "buprepo" c
	c' <- encryptionSetup c

	-- bup init will create the repository.
	-- (If the repository already exists, bup init again appears safe.)
	showAction "bup init"
	unlessM (bup "init" buprepo []) $ error "bup init failed"

	storeBupUUID u buprepo

	-- The buprepo is stored in git config, as well as this repo's
	-- persistant state, so it can vary between hosts.
	gitConfigSpecialRemote u c' "buprepo" buprepo

	return (c', u)

bupParams :: String -> BupRepo -> [CommandParam] -> [CommandParam]
bupParams command buprepo params = 
	Param command : [Param "-r", Param buprepo] ++ params

bup :: String -> BupRepo -> [CommandParam] -> Annex Bool
bup command buprepo params = do
	showOutput -- make way for bup output
	liftIO $ boolSystem "bup" $ bupParams command buprepo params

pipeBup :: [CommandParam] -> Maybe Handle -> Maybe Handle -> IO Bool
pipeBup params inh outh = do
	p <- runProcess "bup" (toCommand params)
		Nothing Nothing inh outh Nothing
	ok <- waitForProcess p
	case ok of
		ExitSuccess -> return True
		_ -> return False

bupSplitParams :: Remote -> BupRepo -> Key -> [CommandParam] -> Annex [CommandParam]
bupSplitParams r buprepo k src = do
	let os = map Param $ remoteAnnexBupSplitOptions $ gitconfig r
	showOutput -- make way for bup output
	return $ bupParams "split" buprepo 
		(os ++ [Param "-n", Param (bupRef k)] ++ src)

store :: Remote -> BupRepo -> Key -> AssociatedFile -> MeterUpdate -> Annex Bool
store r buprepo k _f _p = sendAnnex k (rollback k buprepo) $ \src -> do
	params <- bupSplitParams r buprepo k [File src]
	liftIO $ boolSystem "bup" params

storeEncrypted :: Remote -> BupRepo -> (Cipher, Key) -> Key -> MeterUpdate -> Annex Bool
storeEncrypted r buprepo (cipher, enck) k _p =
	sendAnnex k (rollback enck buprepo) $ \src -> do
		params <- bupSplitParams r buprepo enck []
		liftIO $ catchBoolIO $
			encrypt (getGpgEncParams r) cipher (feedFile src) $ \h ->
				pipeBup params (Just h) Nothing

retrieve :: BupRepo -> Key -> AssociatedFile -> FilePath -> MeterUpdate -> Annex Bool
retrieve buprepo k _f d _p = do
	let params = bupParams "join" buprepo [Param $ bupRef k]
	liftIO $ catchBoolIO $ withFile d WriteMode $
		pipeBup params Nothing . Just

retrieveCheap :: BupRepo -> Key -> FilePath -> Annex Bool
retrieveCheap _ _ _ = return False

retrieveEncrypted :: BupRepo -> (Cipher, Key) -> Key -> FilePath -> MeterUpdate -> Annex Bool
retrieveEncrypted buprepo (cipher, enck) _ f _p = liftIO $ catchBoolIO $
	withHandle StdoutHandle createProcessSuccess p $ \h -> do
		decrypt cipher (\toh -> L.hPut toh =<< L.hGetContents h) $
			readBytes $ L.writeFile f
		return True
  where
	params = bupParams "join" buprepo [Param $ bupRef enck]
	p = proc "bup" $ toCommand params

remove :: Key -> Annex Bool
remove _ = do
	warning "content cannot be removed from bup remote"
	return False

{- Cannot revert having stored a key in bup, but at least the data for the
 - key will be used for deltaing data of other keys stored later.
 -
 - We can, however, remove the git branch that bup created for the key.
 -}
rollback :: Key -> BupRepo -> Annex ()
rollback k bupr = go =<< liftIO (bup2GitRemote bupr)
  where
	go r
		| Git.repoIsUrl r = void $ onBupRemote r boolSystem "git" params
		| otherwise = void $ liftIO $ catchMaybeIO $
			boolSystem "git" $ Git.Command.gitCommandLine params r
	params = [ Params "branch -D", Param (bupRef k) ]

{- Bup does not provide a way to tell if a given dataset is present
 - in a bup repository. One way it to check if the git repository has
 - a branch matching the name (as created by bup split -n).
 -}
checkPresent :: Git.Repo -> Git.Repo -> Key -> Annex (Either String Bool)
checkPresent r bupr k
	| Git.repoIsUrl bupr = do
		showChecking r
		ok <- onBupRemote bupr boolSystem "git" params
		return $ Right ok
	| otherwise = liftIO $ catchMsgIO $
		boolSystem "git" $ Git.Command.gitCommandLine params bupr
  where
	params = 
		[ Params "show-ref --quiet --verify"
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
				[Params $ "config annex.uuid " ++ v]) $
					error "ssh failed"
		else liftIO $ do
			r' <- Git.Config.read r
			let olduuid = Git.Config.get "annex.uuid" "" r'
			when (olduuid == "") $
				Git.Command.run
					[ Param "config"
					, Param "annex.uuid"
					, Param v
					] r'
  where
	v = fromUUID u

onBupRemote :: Git.Repo -> (FilePath -> [CommandParam] -> IO a) -> FilePath -> [CommandParam] -> Annex a
onBupRemote r a command params = do
	sshparams <- Ssh.toRepo r [Param $
			"cd " ++ dir ++ " && " ++ unwords (command : toCommand params)]
	liftIO $ a "ssh" sshparams
  where
	path = Git.repoPath r
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
			Right r' -> return (toUUID $ Git.Config.get "annex.uuid" "" r', r')
			Left _ -> return (NoUUID, r)

{- Converts a bup remote path spec into a Git.Repo. There are some
 - differences in path representation between git and bup. -}
bup2GitRemote :: BupRepo -> IO Git.Repo
bup2GitRemote "" = do
	-- bup -r "" operates on ~/.bup
	h <- myHomeDir
	Git.Construct.fromAbsPath $ h </> ".bup"
bup2GitRemote r
	| bupLocal r = 
		if "/" `isPrefixOf` r
			then Git.Construct.fromAbsPath r
			else error "please specify an absolute path"
	| otherwise = Git.Construct.fromUrl $ "ssh://" ++ host ++ slash dir
  where
	bits = split ":" r
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
	| otherwise = "git-annex-" ++ show (sha256 (fromString shown))
  where
	shown = key2file k

bupLocal :: BupRepo -> Bool
bupLocal = notElem ':'
