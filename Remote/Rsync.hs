{- A remote that is only accessible by rsync.
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Remote.Rsync (remote) where

import qualified Data.ByteString.Lazy as L
import qualified Data.Map as M
import System.Posix.Process (getProcessID)

import Common.Annex
import Types.Remote
import qualified Git
import Config
import Config.Cost
import Annex.Content
import Remote.Helper.Special
import Remote.Helper.Encryptable
import Crypto
import Utility.Rsync
import Utility.CopyFile
import Utility.Metered
import Annex.Perms

type RsyncUrl = String

data RsyncOpts = RsyncOpts
	{ rsyncUrl :: RsyncUrl
	, rsyncOptions :: [CommandParam]
	, rsyncShellEscape :: Bool
}

remote :: RemoteType
remote = RemoteType {
	typename = "rsync",
	enumerate = findSpecialRemotes "rsyncurl",
	generate = gen,
	setup = rsyncSetup
}

gen :: Git.Repo -> UUID -> RemoteConfig -> RemoteGitConfig -> Annex Remote
gen r u c gc = do
	cst <- remoteCost gc expensiveRemoteCost
	return $ encryptableRemote c
		(storeEncrypted o $ getGpgOpts gc)
		(retrieveEncrypted o)
		Remote
			{ uuid = u
			, cost = cst
			, name = Git.repoDescribe r
			, storeKey = store o
			, retrieveKeyFile = retrieve o
			, retrieveKeyFileCheap = retrieveCheap o
			, removeKey = remove o
			, hasKey = checkPresent r o
			, hasKeyCheap = False
			, whereisKey = Nothing
			, config = M.empty
			, repo = r
			, gitconfig = gc
			, localpath = if islocal
				then Just $ rsyncUrl o
				else Nothing
			, readonly = False
			, globallyAvailable = not $ islocal
			, remotetype = remote
			}
  where
	o = RsyncOpts url opts escape
	islocal = rsyncUrlIsPath $ rsyncUrl o
	url = fromMaybe (error "missing rsyncurl") $ remoteAnnexRsyncUrl gc
	opts = map Param $ filter safe $ remoteAnnexRsyncOptions gc
	escape = M.lookup "shellescape" c /= Just "no"
	safe opt
		-- Don't allow user to pass --delete to rsync;
		-- that could cause it to delete other keys
		-- in the same hash bucket as a key it sends.
		| opt == "--delete" = False
		| opt == "--delete-excluded" = False
		| otherwise = True

rsyncSetup :: UUID -> RemoteConfig -> Annex RemoteConfig
rsyncSetup u c = do
	-- verify configuration is sane
	let url = fromMaybe (error "Specify rsyncurl=") $
		M.lookup "rsyncurl" c
	c' <- encryptionSetup c

	-- The rsyncurl is stored in git config, not only in this remote's
	-- persistant state, so it can vary between hosts.
	gitConfigSpecialRemote u c' "rsyncurl" url
	return c'

rsyncEscape :: RsyncOpts -> String -> String
rsyncEscape o s
	| rsyncShellEscape o && rsyncUrlIsShell (rsyncUrl o) = shellEscape s
	| otherwise = s

rsyncUrls :: RsyncOpts -> Key -> [String]
rsyncUrls o k = map use annexHashes
  where
	use h = rsyncUrl o </> h k </> rsyncEscape o (f </> f)
	f = keyFile k

store :: RsyncOpts -> Key -> AssociatedFile -> MeterUpdate -> Annex Bool
store o k _f p = sendAnnex k (void $ remove o k) $ rsyncSend o p k False

storeEncrypted :: RsyncOpts -> GpgOpts -> (Cipher, Key) -> Key -> MeterUpdate -> Annex Bool
storeEncrypted o gpgOpts (cipher, enck) k p = withTmp enck $ \tmp ->
	sendAnnex k (void $ remove o enck) $ \src -> do
		liftIO $ encrypt gpgOpts cipher (feedFile src) $
			readBytes $ L.writeFile tmp
		rsyncSend o p enck True tmp

retrieve :: RsyncOpts -> Key -> AssociatedFile -> FilePath -> MeterUpdate -> Annex Bool
retrieve o k _ f p = rsyncRetrieve o k f (Just p)

retrieveCheap :: RsyncOpts -> Key -> FilePath -> Annex Bool
retrieveCheap o k f = ifM (preseedTmp k f) ( rsyncRetrieve o k f Nothing , return False )

retrieveEncrypted :: RsyncOpts -> (Cipher, Key) -> Key -> FilePath -> MeterUpdate -> Annex Bool
retrieveEncrypted o (cipher, enck) _ f p = withTmp enck $ \tmp ->
	ifM (rsyncRetrieve o enck tmp (Just p))
		( liftIO $ catchBoolIO $ do
			decrypt cipher (feedFile tmp) $
				readBytes $ L.writeFile f
			return True
		, return False
		)

remove :: RsyncOpts -> Key -> Annex Bool
remove o k = withRsyncScratchDir $ \tmp -> liftIO $ do
	{- Send an empty directory to rysnc to make it delete. -}
	let dummy = tmp </> keyFile k
	createDirectoryIfMissing True dummy
	rsync $ rsyncOptions o ++
		map (\s -> Param $ "--include=" ++ s) includes ++
		[ Param "--exclude=*" -- exclude everything else
		, Params "--quiet --delete --recursive"
		, partialParams
		, Param $ addTrailingPathSeparator dummy
		, Param $ rsyncUrl o
		]
  where
	{- Specify include rules to match the directories where the
	 - content could be. Note that the parent directories have
	 - to also be explicitly included, due to how rsync
	 - traverses directories. -}
	includes = concatMap use annexHashes
	use h = let dir = h k in
		[ parentDir dir
		, dir
		-- match content directory and anything in it
		, dir </> keyFile k </> "***"
		]

checkPresent :: Git.Repo -> RsyncOpts -> Key -> Annex (Either String Bool)
checkPresent r o k = do
	showAction $ "checking " ++ Git.repoDescribe r
	-- note: Does not currently differentiate between rsync failing
	-- to connect, and the file not being present.
	Right <$> check
  where
	check = untilTrue (rsyncUrls o k) $ \u -> 
		liftIO $ catchBoolIO $ do
			withQuietOutput createProcessSuccess $
				proc "rsync" $ toCommand $
					rsyncOptions o ++ [Param u]
			return True

{- Rsync params to enable resumes of sending files safely,
 - ensure that files are only moved into place once complete
 -}
partialParams :: CommandParam
partialParams = Params "--partial --partial-dir=.rsync-partial"

{- Runs an action in an empty scratch directory that can be used to build
 - up trees for rsync. -}
withRsyncScratchDir :: (FilePath -> Annex Bool) -> Annex Bool
withRsyncScratchDir a = do
	pid <- liftIO getProcessID
	t <- fromRepo gitAnnexTmpDir
	createAnnexDirectory t
	let tmp = t </> "rsynctmp" </> show pid
	nuke tmp
	liftIO $ createDirectoryIfMissing True tmp
	nuke tmp `after` a tmp
  where
	nuke d = liftIO $ whenM (doesDirectoryExist d) $
		removeDirectoryRecursive d

rsyncRetrieve :: RsyncOpts -> Key -> FilePath -> Maybe MeterUpdate -> Annex Bool
rsyncRetrieve o k dest callback =
	untilTrue (rsyncUrls o k) $ \u -> rsyncRemote o callback
		-- use inplace when retrieving to support resuming
		[ Param "--inplace"
		, Param u
		, Param dest
		]

rsyncRemote :: RsyncOpts -> (Maybe MeterUpdate) -> [CommandParam] -> Annex Bool
rsyncRemote o callback params = do
	showOutput -- make way for progress bar
	ifM (liftIO $ (maybe rsync rsyncProgress callback) ps)
		( return True
		, do
			showLongNote "rsync failed -- run git annex again to resume file transfer"
			return False
		)
  where
	defaultParams = [Params "--progress"]
	ps = rsyncOptions o ++ defaultParams ++ params

{- To send a single key is slightly tricky; need to build up a temporary
 - directory structure to pass to rsync so it can create the hash
 - directories.
 -
 - This would not be necessary if the hash directory structure used locally
 - was always the same as that used on the rsync remote. So if that's ever
 - unified, this gets nicer. Especially in the crippled filesystem case.
 - (When we have the right hash directory structure, we can just
 - pass --include=X --include=X/Y --include=X/Y/file --exclude=*)
 -}
rsyncSend :: RsyncOpts -> MeterUpdate -> Key -> Bool -> FilePath -> Annex Bool
rsyncSend o callback k canrename src = withRsyncScratchDir $ \tmp -> do
	let dest = tmp </> Prelude.head (keyPaths k)
	liftIO $ createDirectoryIfMissing True $ parentDir dest
	ok <- if canrename
		then do
			liftIO $ renameFile src dest
			return True
		else ifM crippledFileSystem
			( liftIO $ copyFileExternal src dest
			, do
				liftIO $ createLink src dest
				return True
			)
	if ok
		then rsyncRemote o (Just callback)
			[ Param "--recursive"
			, partialParams
			-- tmp/ to send contents of tmp dir
			, Param $ addTrailingPathSeparator tmp
			, Param $ rsyncUrl o
			]
		else return False
