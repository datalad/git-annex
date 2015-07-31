{- A remote that is only accessible by rsync.
 -
 - Copyright 2011 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Remote.Rsync (
	remote,
	store,
	retrieve,
	remove,
	checkKey,
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
import Remote.Rsync.RsyncUrl
import Crypto
import Utility.Rsync
import Utility.CopyFile
import Messages.Progress
import Utility.Metered
import Logs.Transfer
import Types.Creds
import Types.Key (isChunkKey)
import Annex.DirHashes
import Utility.Tmp

import qualified Data.Map as M

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
	return $ Just $ specialRemote' specialcfg c
		(simplyPrepare $ fileStorer $ store o)
		(simplyPrepare $ fileRetriever $ retrieve o)
		(simplyPrepare $ remove o)
		(simplyPrepare $ checkKey r o)
		Remote
			{ uuid = u
			, cost = cst
			, name = Git.repoDescribe r
			, storeKey = storeKeyDummy
			, retrieveKeyFile = retreiveKeyFileDummy
			, retrieveKeyFileCheap = retrieveCheap o
			, removeKey = removeKeyDummy
			, checkPresent = checkPresentDummy
			, checkPresentCheap = False
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
			, mkUnavailable = return Nothing
			, getInfo = return [("url", url)]
			, claimUrl = Nothing
			, checkUrl = Nothing
			}
  where
	specialcfg = (specialRemoteCfg c)
		-- Rsync displays its own progress.
		{ displayProgress = False }

genRsyncOpts :: RemoteConfig -> RemoteGitConfig -> [CommandParam] -> RsyncUrl -> RsyncOpts
genRsyncOpts c gc transport url = RsyncOpts
	{ rsyncUrl = url
	, rsyncOptions = transport ++ opts []
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
				(Param "ssh":) <$> sshOptions
					(userhost, port) gc
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
	(c', _encsetup) <- encryptionSetup c

	-- The rsyncurl is stored in git config, not only in this remote's
	-- persistant state, so it can vary between hosts.
	gitConfigSpecialRemote u c' "rsyncurl" url
	return (c', u)

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
store :: RsyncOpts -> Key -> FilePath -> MeterUpdate -> Annex Bool
store o k src meterupdate = withRsyncScratchDir $ \tmp -> do
	let dest = tmp </> Prelude.head (keyPaths k)
	liftIO $ createDirectoryIfMissing True $ parentDir dest
	ok <- liftIO $ if canrename
		then do
			rename src dest
			return True
		else createLinkOrCopy src dest
	ps <- sendParams
	if ok
		then showResumable $ rsyncRemote Upload o (Just meterupdate) $ ps ++
			Param "--recursive" : partialParams ++
			-- tmp/ to send contents of tmp dir
			[ File $ addTrailingPathSeparator tmp
			, Param $ rsyncUrl o
			]
		else return False
  where
	{- If the key being sent is encrypted or chunked, the file
	 - containing its content is a temp file, and so can be
	 - renamed into place. Otherwise, the file is the annexed
	 - object file, and has to be copied or hard linked into place. -}
	canrename = isEncKey k || isChunkKey k

retrieve :: RsyncOpts -> FilePath -> Key -> MeterUpdate -> Annex ()
retrieve o f k p = 
	unlessM (rsyncRetrieve o k f (Just p)) $
		error "rsync failed"

retrieveCheap :: RsyncOpts -> Key -> AssociatedFile -> FilePath -> Annex Bool
retrieveCheap o k _af f = ifM (preseedTmp k f) ( rsyncRetrieve o k f Nothing , return False )

remove :: RsyncOpts -> Remover
remove o k = do
	ps <- sendParams
	withRsyncScratchDir $ \tmp -> liftIO $ do
		{- Send an empty directory to rysnc to make it delete. -}
		let dummy = tmp </> keyFile k
		createDirectoryIfMissing True dummy
		rsync $ rsyncOptions o ++ ps ++
			map (\s -> Param $ "--include=" ++ s) includes ++
			[ Param "--exclude=*" -- exclude everything else
			, Param "--quiet", Param "--delete", Param "--recursive"
			] ++ partialParams ++ 
			[ Param $ addTrailingPathSeparator dummy
			, Param $ rsyncUrl o
			]
  where
	{- Specify include rules to match the directories where the
	 - content could be. Note that the parent directories have
	 - to also be explicitly included, due to how rsync
	 - traverses directories. -}
	includes = concatMap use dirHashes
	use h = let dir = h def k in
		[ parentDir dir
		, dir
		-- match content directory and anything in it
		, dir </> keyFile k </> "***"
		]

checkKey :: Git.Repo -> RsyncOpts -> CheckPresent
checkKey r o k = do
	showAction $ "checking " ++ Git.repoDescribe r
	-- note: Does not currently differentiate between rsync failing
	-- to connect, and the file not being present.
	untilTrue (rsyncUrls o k) $ \u -> 
		liftIO $ catchBoolIO $ do
			withQuietOutput createProcessSuccess $
				proc "rsync" $ toCommand $
					rsyncOptions o ++ [Param u]
			return True

{- Rsync params to enable resumes of sending files safely,
 - ensure that files are only moved into place once complete
 -}
partialParams :: [CommandParam]
partialParams = [Param "--partial", Param "--partial-dir=.rsync-partial"]

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
	t <- fromRepo gitAnnexTmpObjectDir
	withTmpDirIn t "rsynctmp" a

rsyncRetrieve :: RsyncOpts -> Key -> FilePath -> Maybe MeterUpdate -> Annex Bool
rsyncRetrieve o k dest meterupdate =
	showResumable $ untilTrue (rsyncUrls o k) $ \u -> rsyncRemote Download o meterupdate
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
rsyncRemote direction o m params = do
	showOutput -- make way for progress bar
	case m of
		Nothing -> liftIO $ rsync ps
		Just meter -> do
			oh <- mkOutputHandler
			liftIO $ rsyncProgress oh meter ps
  where
	ps = opts ++ Param "--progress" : params
	opts
		| direction == Download = rsyncDownloadOptions o
		| otherwise = rsyncUploadOptions o
