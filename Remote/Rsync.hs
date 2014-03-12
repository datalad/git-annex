{- A remote that is only accessible by rsync.
 -
 - Copyright 2011 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Remote.Rsync (
	remote,
	storeEncrypted,
	retrieveEncrypted,
	remove,
	checkPresent,
	withRsyncScratchDir,
	genRsyncOpts,
	RsyncOpts
) where

import Common.Annex
import Types.Remote
import qualified Git
import Config
import Config.Cost
import Annex.Content
import Annex.UUID
import Annex.Ssh
import Remote.Helper.Special
import Remote.Helper.Encryptable
import Crypto
import Utility.Rsync
import Utility.CopyFile
import Utility.Metered
import Utility.PID
import Annex.Perms
import Logs.Transfer
import Types.Creds

import qualified Data.ByteString.Lazy as L
import qualified Data.Map as M

type RsyncUrl = String

data RsyncOpts = RsyncOpts
	{ rsyncUrl :: RsyncUrl
	, rsyncOptions :: [CommandParam]
	, rsyncUploadOptions :: [CommandParam]
	, rsyncDownloadOptions :: [CommandParam]
	, rsyncShellEscape :: Bool
}

remote :: RemoteType
remote = RemoteType {
	typename = "rsync",
	enumerate = findSpecialRemotes "rsyncurl",
	generate = gen,
	setup = rsyncSetup
}

gen :: Git.Repo -> UUID -> RemoteConfig -> RemoteGitConfig -> Annex (Maybe Remote)
gen r u c gc = do
	cst <- remoteCost gc expensiveRemoteCost
	(transport, url) <- rsyncTransport gc $
		fromMaybe (error "missing rsyncurl") $ remoteAnnexRsyncUrl gc
	let o = genRsyncOpts c gc transport url
	let islocal = rsyncUrlIsPath $ rsyncUrl o
	return $ Just $ encryptableRemote c
		(storeEncrypted o $ getGpgEncParams (c,gc))
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
			, remoteFsck = Nothing
			, repairRepo = Nothing
			, config = c
			, repo = r
			, gitconfig = gc
			, localpath = if islocal
				then Just $ rsyncUrl o
				else Nothing
			, readonly = False
			, availability = if islocal then LocallyAvailable else GloballyAvailable
			, remotetype = remote
			}

genRsyncOpts :: RemoteConfig -> RemoteGitConfig -> [CommandParam] -> RsyncUrl -> RsyncOpts
genRsyncOpts c gc transport url = RsyncOpts
	{ rsyncUrl = url
	, rsyncOptions = opts []
	, rsyncUploadOptions = transport ++ opts (remoteAnnexRsyncUploadOptions gc)
	, rsyncDownloadOptions = transport ++ opts (remoteAnnexRsyncDownloadOptions gc)
	, rsyncShellEscape = M.lookup "shellescape" c /= Just "no"
	}
  where
	opts specificopts = map Param $ filter safe $
		remoteAnnexRsyncOptions gc ++ specificopts
	safe opt
		-- Don't allow user to pass --delete to rsync;
		-- that could cause it to delete other keys
		-- in the same hash bucket as a key it sends.
		| opt == "--delete" = False
		| opt == "--delete-excluded" = False
		| otherwise = True

rsyncTransport :: RemoteGitConfig -> RsyncUrl -> Annex ([CommandParam], RsyncUrl)
rsyncTransport gc url
	| rsyncUrlIsShell url =
		(\rsh -> return (rsyncShell rsh, url)) =<<
		case fromNull ["ssh"] (remoteAnnexRsyncTransport gc) of
			"ssh":sshopts -> do
				let (port, sshopts') = sshReadPort sshopts
				    userhost = takeWhile (/=':') url
				-- Connection caching
				(Param "ssh":) <$> sshCachingOptions
					(userhost, port)
					(map Param $ loginopt ++ sshopts')
			"rsh":rshopts -> return $ map Param $ "rsh" :
				loginopt ++ rshopts
			rsh -> error $ "Unknown Rsync transport: "
				++ unwords rsh
	| otherwise = return ([], url)
  where
	login = case separate (=='@') url of
		(_h, "") -> Nothing
		(l, _)  -> Just l
	loginopt = maybe [] (\l -> ["-l",l]) login
	fromNull as xs = if null xs then as else xs

rsyncSetup :: Maybe UUID -> Maybe CredPair -> RemoteConfig -> Annex (RemoteConfig, UUID)
rsyncSetup mu _ c = do
	u <- maybe (liftIO genUUID) return mu
	-- verify configuration is sane
	let url = fromMaybe (error "Specify rsyncurl=") $
		M.lookup "rsyncurl" c
	c' <- encryptionSetup c

	-- The rsyncurl is stored in git config, not only in this remote's
	-- persistant state, so it can vary between hosts.
	gitConfigSpecialRemote u c' "rsyncurl" url
	return (c', u)

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

storeEncrypted :: RsyncOpts -> [CommandParam] -> (Cipher, Key) -> Key -> MeterUpdate -> Annex Bool
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
remove o k = do
	ps <- sendParams
	withRsyncScratchDir $ \tmp -> liftIO $ do
		{- Send an empty directory to rysnc to make it delete. -}
		let dummy = tmp </> keyFile k
		createDirectoryIfMissing True dummy
		rsync $ rsyncOptions o ++ ps ++
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

{- When sending files from crippled filesystems, the permissions can be all
 - messed up, and it's better to use the default permissions on the
 - destination. -}
sendParams :: Annex [CommandParam]
sendParams = ifM crippledFileSystem
	( return [rsyncUseDestinationPermissions]
	, return []
	)

{- Runs an action in an empty scratch directory that can be used to build
 - up trees for rsync. -}
withRsyncScratchDir :: (FilePath -> Annex a) -> Annex a
withRsyncScratchDir a = do
	p <- liftIO getPID
	t <- fromRepo gitAnnexTmpObjectDir
	createAnnexDirectory t
	let tmp = t </> "rsynctmp" </> show p
	nuke tmp
	liftIO $ createDirectoryIfMissing True tmp
	nuke tmp `after` a tmp
  where
	nuke d = liftIO $ whenM (doesDirectoryExist d) $
		removeDirectoryRecursive d

rsyncRetrieve :: RsyncOpts -> Key -> FilePath -> Maybe MeterUpdate -> Annex Bool
rsyncRetrieve o k dest callback =
	showResumable $ untilTrue (rsyncUrls o k) $ \u -> rsyncRemote Download o callback
		-- use inplace when retrieving to support resuming
		[ Param "--inplace"
		, Param u
		, File dest
		]

showResumable :: Annex Bool -> Annex Bool
showResumable a = ifM a
	( return True
	, do
		showLongNote "rsync failed -- run git annex again to resume file transfer"
		return False
	)

rsyncRemote :: Direction -> RsyncOpts -> Maybe MeterUpdate -> [CommandParam] -> Annex Bool
rsyncRemote direction o callback params = do
	showOutput -- make way for progress bar
	liftIO $ (maybe rsync rsyncProgress callback) $
		opts ++ [Params "--progress"] ++ params
  where
	opts
		| direction == Download = rsyncDownloadOptions o
		| otherwise = rsyncUploadOptions o

{- To send a single key is slightly tricky; need to build up a temporary
 - directory structure to pass to rsync so it can create the hash
 - directories.
 -
 - This would not be necessary if the hash directory structure used locally
 - was always the same as that used on the rsync remote. So if that's ever
 - unified, this gets nicer.
 - (When we have the right hash directory structure, we can just
 - pass --include=X --include=X/Y --include=X/Y/file --exclude=*)
 -}
rsyncSend :: RsyncOpts -> MeterUpdate -> Key -> Bool -> FilePath -> Annex Bool
rsyncSend o callback k canrename src = withRsyncScratchDir $ \tmp -> do
	let dest = tmp </> Prelude.head (keyPaths k)
	liftIO $ createDirectoryIfMissing True $ parentDir dest
	ok <- liftIO $ if canrename
		then do
			rename src dest
			return True
		else createLinkOrCopy src dest
	ps <- sendParams
	if ok
		then showResumable $ rsyncRemote Upload o (Just callback) $ ps ++
			[ Param "--recursive"
			, partialParams
			-- tmp/ to send contents of tmp dir
			, File $ addTrailingPathSeparator tmp
			, Param $ rsyncUrl o
			]
		else return False
