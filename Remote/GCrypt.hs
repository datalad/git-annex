{- git remotes encrypted using git-remote-gcrypt
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Remote.GCrypt (remote, gen) where

import qualified Data.Map as M

import Common.Annex
import Types.Remote
import Types.GitConfig
import Types.Crypto
import qualified Git
import qualified Git.Command
import qualified Git.Config
import qualified Git.GCrypt
import qualified Git.Types as Git ()
import qualified Annex.Branch
import qualified Annex.Content
import Config
import Config.Cost
import Remote.Helper.Git
import Remote.Helper.Encryptable
import Utility.Metered
import Crypto
import Annex.UUID

remote :: RemoteType
remote = RemoteType {
	typename = "gcrypt",
	-- Remote.Git takes care of enumerating gcrypt remotes too,
	-- and will call our gen on them.
	enumerate = return [],
	generate = gen,
	setup = gCryptSetup
}

gen :: Git.Repo -> UUID -> RemoteConfig -> RemoteGitConfig -> Annex Remote
gen gcryptr u c gc = do
	g <- gitRepo
	-- get underlying git repo with real path, not gcrypt path
	r <- liftIO $ Git.GCrypt.encryptedRepo g gcryptr
	let r' = r { Git.remoteName = Git.remoteName gcryptr }
	-- read config of underlying repo if it's local
	r'' <- if Git.repoIsLocalUnknown r'
		then liftIO $ catchDefaultIO r' $ Git.Config.read r'
		else return r'
	gen' r'' u c gc

gen' :: Git.Repo -> UUID -> RemoteConfig -> RemoteGitConfig -> Annex Remote
gen' r u c gc = new <$> remoteCost gc defcst
  where
	defcst = if repoCheap r then nearlyCheapRemoteCost else expensiveRemoteCost
	new cst = encryptableRemote c
		(store this)
		(retrieve this)
		this
	  where
		this = Remote 
			{ uuid = u
			, cost = cst
			, name = Git.repoDescribe r
			, storeKey = \_ _ _ -> noCrypto
			, retrieveKeyFile = \_ _ _ _ -> noCrypto
			, retrieveKeyFileCheap = \_ _ -> return False
			, removeKey = remove
			, hasKey = checkPresent this
			, hasKeyCheap = repoCheap r
			, whereisKey = Nothing
			, config = M.empty
			, localpath = localpathCalc r
			, repo = r
			, gitconfig = gc { remoteGitConfig = Just $ extractGitConfig r }
			, readonly = Git.repoIsHttp r
			, globallyAvailable = globallyAvailableCalc r
			, remotetype = remote
			}

noCrypto :: Annex a
noCrypto = error "cannot use gcrypt remote without encryption enabled"

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

		{- Configure gcrypt to use the same list of keyids that
		 - were passed to initremote, unless shared encryption
		 - was used. -}
		case extractCipher c' of
			Nothing -> noCrypto
			Just (EncryptedCipher _ _ (KeyIds { keyIds = ks})) ->
				setConfig (ConfigKey $ Git.GCrypt.remoteParticipantConfigKey remotename) (unwords ks)
			_ -> noop

		{- Run a git fetch and a push to the git repo in order to get
		 - its gcrypt-id set up, so that later git annex commands
		 - will use the remote as a ggcrypt remote. The fetch is
		 - needed if the repo already exists; the push is needed
		 - if the repo has not yet been initialized by gcrypt. -}
		void $ inRepo $ Git.Command.runBool
			[ Param "fetch"
			, Param remotename
			]
		void $ inRepo $ Git.Command.runBool
			[ Param "push"
			, Param remotename
			, Param $ show $ Annex.Branch.fullname
			]
		g <- inRepo Git.Config.reRead
		case Git.GCrypt.remoteRepoId g (Just remotename) of
			Nothing -> error "unable to determine gcrypt-id of remote"
			Just v -> do
				let u = genUUIDInNameSpace gCryptNameSpace v
				if Just u == mu || mu == Nothing
					then return (c', u)
					else error "uuid mismatch"

store :: Remote -> (Cipher, Key) -> Key -> MeterUpdate -> Annex Bool
store r (cipher, enck) k p
	| not $ Git.repoIsUrl (repo r) = guardUsable (repo r) False $
		sendwith $ \meterupdate h -> do
			createDirectoryIfMissing True $ parentDir dest
			readBytes (meteredWriteFile meterupdate dest) h
			return True
	| Git.repoIsSsh (repo r) = sendwith $ \h -> undefined
	| otherwise = error "storing on non-ssh remote repo not supported"
  where
	dest = gCryptLocation r enck
  	sendwith a = metered (Just p) k $ \meterupdate ->
		Annex.Content.sendAnnex k noop $ \src ->
			liftIO $ catchBoolIO $
				encrypt (getGpgEncParams r) cipher (feedFile src) (a meterupdate)

retrieve :: Remote -> (Cipher, Key) -> Key -> FilePath -> MeterUpdate -> Annex Bool
retrieve r (cipher, enck) k d p = undefined

remove :: Key -> Annex Bool
remove k = undefined

checkPresent :: Remote -> Key -> Annex (Either String Bool)
checkPresent r k
	| not $ Git.repoIsUrl (repo r) =
		guardUsable (repo r) unknown $
			liftIO $ catchDefaultIO unknown $
				Right <$> doesFileExist (gCryptLocation r k)
	| Git.repoIsSsh (repo r) = undefined
	| otherwise = error "storing on non-ssh remote repo not supported"
  where
	unknown = Left $ "unable to check " ++ Git.repoDescribe (repo r) ++ show (repo r)

gCryptLocation :: Remote -> Key -> FilePath
gCryptLocation r key = Git.repoLocation (repo r) </> annexLocation key hashDirLower
