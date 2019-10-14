{- A remote that is only accessible by rsync.
 -
 - Copyright 2011-2018 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
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

import Annex.Common
import Types.Remote
import qualified Git
import Config
import Config.Cost
import Annex.Content
import Annex.UUID
import Annex.Ssh
import Remote.Helper.Special
import Remote.Helper.Messages
import Remote.Helper.ExportImport
import Types.Export
import Remote.Rsync.RsyncUrl
import Crypto
import Utility.Rsync
import Utility.CopyFile
import Messages.Progress
import Utility.Metered
import Types.Transfer
import Types.Creds
import Annex.DirHashes
import Utility.Tmp.Dir
import Utility.SshHost

import qualified Data.Map as M

remote :: RemoteType
remote = RemoteType
	{ typename = "rsync"
	, enumerate = const (findSpecialRemotes "rsyncurl")
	, generate = gen
	, setup = rsyncSetup
	, exportSupported = exportIsSupported
	, importSupported = importUnsupported
	}

gen :: Git.Repo -> UUID -> RemoteConfig -> RemoteGitConfig -> RemoteStateHandle -> Annex (Maybe Remote)
gen r u c gc rs = do
	cst <- remoteCost gc expensiveRemoteCost
	(transport, url) <- rsyncTransport gc $
		fromMaybe (giveup "missing rsyncurl") $ remoteAnnexRsyncUrl gc
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
			, retrievalSecurityPolicy = RetrievalAllKeysSecure
			, removeKey = removeKeyDummy
			, lockContent = Nothing
			, checkPresent = checkPresentDummy
			, checkPresentCheap = False
			, exportActions = ExportActions
				{ storeExport = storeExportM o
				, retrieveExport = retrieveExportM o
				, removeExport = removeExportM o
				, checkPresentExport = checkPresentExportM o
				, removeExportDirectory = Just (removeExportDirectoryM o)
				, renameExport = renameExportM o
				}
			, importActions = importUnsupported
			, whereisKey = Nothing
			, remoteFsck = Nothing
			, repairRepo = Nothing
			, config = c
			, getRepo = return r
			, gitconfig = gc
			, localpath = if islocal
				then Just $ rsyncUrl o
				else Nothing
			, readonly = False
			, appendonly = False
			, availability = if islocal then LocallyAvailable else GloballyAvailable
			, remotetype = remote
			, mkUnavailable = return Nothing
			, getInfo = return [("url", url)]
			, claimUrl = Nothing
			, checkUrl = Nothing
			, remoteStateHandle = rs
			}
  where
	specialcfg = (specialRemoteCfg c)
		-- Rsync displays its own progress.
		{ displayProgress = False }

genRsyncOpts :: RemoteConfig -> RemoteGitConfig -> Annex [CommandParam] -> RsyncUrl -> RsyncOpts
genRsyncOpts c gc transport url = RsyncOpts
	{ rsyncUrl = url
	, rsyncOptions = appendtransport $ opts []
	, rsyncUploadOptions = appendtransport $
		opts (remoteAnnexRsyncUploadOptions gc)
	, rsyncDownloadOptions = appendtransport $
		opts (remoteAnnexRsyncDownloadOptions gc)
	, rsyncShellEscape = (yesNo =<< M.lookup "shellescape" c) /= Just False
	}
  where
	appendtransport l = (++ l) <$> transport
	opts specificopts = map Param $ filter safe $
		remoteAnnexRsyncOptions gc ++ specificopts
	safe opt
		-- Don't allow user to pass --delete to rsync;
		-- that could cause it to delete other keys
		-- in the same hash bucket as a key it sends.
		| opt == "--delete" = False
		| opt == "--delete-excluded" = False
		| otherwise = True

rsyncTransport :: RemoteGitConfig -> RsyncUrl -> Annex (Annex [CommandParam], RsyncUrl)
rsyncTransport gc url
	| rsyncUrlIsShell url =
		(\transport -> return (rsyncShell <$> transport, url)) =<<
		case fromNull ["ssh"] (remoteAnnexRsyncTransport gc) of
			"ssh":sshopts -> do
				let (port, sshopts') = sshReadPort sshopts
				    userhost = either error id $ mkSshHost $ 
				    	takeWhile (/= ':') url
				return $ (Param "ssh":) <$> sshOptions ConsumeStdin
					(userhost, port) gc
					(map Param $ loginopt ++ sshopts')
			"rsh":rshopts -> return $ pure $ map Param $ "rsh" :
				loginopt ++ rshopts
			rsh -> giveup $ "Unknown Rsync transport: "
				++ unwords rsh
	| otherwise = return (pure [], url)
  where
	login = case separate (=='@') url of
		(_h, "") -> Nothing
		(l, _)  -> Just l
	loginopt = maybe [] (\l -> ["-l",l]) login
	fromNull as xs = if null xs then as else xs

rsyncSetup :: SetupStage -> Maybe UUID -> Maybe CredPair -> RemoteConfig -> RemoteGitConfig -> Annex (RemoteConfig, UUID)
rsyncSetup _ mu _ c gc = do
	u <- maybe (liftIO genUUID) return mu
	-- verify configuration is sane
	let url = fromMaybe (giveup "Specify rsyncurl=") $
		M.lookup "rsyncurl" c
	(c', _encsetup) <- encryptionSetup c gc

	-- The rsyncurl is stored in git config, not only in this remote's
	-- persistant state, so it can vary between hosts.
	gitConfigSpecialRemote u c' [("rsyncurl", url)]
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
store o k src meterupdate = storeGeneric o meterupdate basedest populatedest
  where
	basedest = Prelude.head (keyPaths k)
	populatedest dest = liftIO $ if canrename
		then do
			rename src dest
			return True
		else createLinkOrCopy src dest
	{- If the key being sent is encrypted or chunked, the file
	 - containing its content is a temp file, and so can be
	 - renamed into place. Otherwise, the file is the annexed
	 - object file, and has to be copied or hard linked into place. -}
	canrename = isEncKey k || isChunkKey k

storeGeneric :: RsyncOpts -> MeterUpdate -> FilePath -> (FilePath -> Annex Bool) -> Annex Bool
storeGeneric o meterupdate basedest populatedest = withRsyncScratchDir $ \tmp -> do
	let dest = tmp </> basedest
	liftIO $ createDirectoryIfMissing True $ parentDir dest
	ok <- populatedest dest
	ps <- sendParams
	if ok
		then showResumable $ rsyncRemote Upload o (Just meterupdate) $ ps ++
			Param "--recursive" : partialParams ++
			-- tmp/ to send contents of tmp dir
			[ File $ addTrailingPathSeparator tmp
			, Param $ rsyncUrl o
			]
		else return False

retrieve :: RsyncOpts -> FilePath -> Key -> MeterUpdate -> Annex ()
retrieve o f k p = 
	unlessM (rsyncRetrieveKey o k f (Just p)) $
		giveup "rsync failed"

retrieveCheap :: RsyncOpts -> Key -> AssociatedFile -> FilePath -> Annex Bool
retrieveCheap o k _af f = ifM (preseedTmp k f) ( rsyncRetrieveKey o k f Nothing , return False )

remove :: RsyncOpts -> Remover
remove o k = removeGeneric o includes
  where
	includes = concatMap use dirHashes
	use h = let dir = h def k in
		[ parentDir dir
		, dir
		-- match content directory and anything in it
		, dir </> keyFile k </> "***"
		]

{- An empty directory is rsynced to make it delete. Everything is excluded,
 - except for the specified includes. Due to the way rsync traverses
 - directories, the includes must match both the file to be deleted, and
 - its parent directories, but not their other contents. -}
removeGeneric :: RsyncOpts -> [String] -> Annex Bool
removeGeneric o includes = do
	ps <- sendParams
	opts <- rsyncOptions o
	withRsyncScratchDir $ \tmp -> liftIO $ do
		{- Send an empty directory to rysnc to make it delete. -}
		rsync $ opts ++ ps ++
			map (\s -> Param $ "--include=" ++ s) includes ++
			[ Param "--exclude=*" -- exclude everything else
			, Param "--quiet", Param "--delete", Param "--recursive"
			] ++ partialParams ++ 
			[ Param $ addTrailingPathSeparator tmp
			, Param $ rsyncUrl o
			]

checkKey :: Git.Repo -> RsyncOpts -> CheckPresent
checkKey r o k = do
	showChecking r
	checkPresentGeneric o (rsyncUrls o k)

checkPresentGeneric :: RsyncOpts -> [RsyncUrl] -> Annex Bool
checkPresentGeneric o rsyncurls = do
	opts <- rsyncOptions o
	-- note: Does not currently differentiate between rsync failing
	-- to connect, and the file not being present.
	untilTrue rsyncurls $ \u -> 
		liftIO $ catchBoolIO $ do
			withQuietOutput createProcessSuccess $
				proc "rsync" $ toCommand $ opts ++ [Param u]
			return True

storeExportM :: RsyncOpts -> FilePath -> Key -> ExportLocation -> MeterUpdate -> Annex Bool
storeExportM o src _k loc meterupdate =
	storeGeneric o meterupdate basedest populatedest
  where
	basedest = fromExportLocation loc
	populatedest = liftIO . createLinkOrCopy src

retrieveExportM :: RsyncOpts -> Key -> ExportLocation -> FilePath -> MeterUpdate -> Annex Bool
retrieveExportM o _k loc dest p = rsyncRetrieve o [rsyncurl] dest (Just p)
  where
	rsyncurl = mkRsyncUrl o (fromExportLocation loc)

checkPresentExportM :: RsyncOpts -> Key -> ExportLocation -> Annex Bool
checkPresentExportM o _k loc = checkPresentGeneric o [rsyncurl]
  where
	rsyncurl = mkRsyncUrl o (fromExportLocation loc)

removeExportM :: RsyncOpts -> Key -> ExportLocation -> Annex Bool
removeExportM o _k loc =
	removeGeneric o (includes (fromExportLocation loc))
  where
	includes f = f : case upFrom f of
		Nothing -> []
		Just f' -> includes f'

removeExportDirectoryM :: RsyncOpts -> ExportDirectory -> Annex Bool
removeExportDirectoryM o ed = removeGeneric o (allbelow d : includes d)
  where
	d = fromExportDirectory ed
	allbelow f = f </> "***"
	includes f = f : case upFrom f of
		Nothing -> []
		Just f' -> includes f'

renameExportM :: RsyncOpts -> Key -> ExportLocation -> ExportLocation -> Annex (Maybe Bool)
renameExportM _ _ _ _ = return Nothing

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

rsyncRetrieve :: RsyncOpts -> [RsyncUrl] -> FilePath -> Maybe MeterUpdate -> Annex Bool
rsyncRetrieve o rsyncurls dest meterupdate =
	showResumable $ untilTrue rsyncurls $ \u -> rsyncRemote Download o meterupdate
		-- use inplace when retrieving to support resuming
		[ Param "--inplace"
		, Param u
		, File dest
		]

rsyncRetrieveKey :: RsyncOpts -> Key -> FilePath -> Maybe MeterUpdate -> Annex Bool
rsyncRetrieveKey o k dest meterupdate = rsyncRetrieve o (rsyncUrls o k) dest meterupdate

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
	opts <- mkopts
	let ps = opts ++ Param "--progress" : params
	case m of
		Nothing -> liftIO $ rsync ps
		Just meter -> do
			oh <- mkOutputHandler
			liftIO $ rsyncProgress oh meter ps
  where
	mkopts
		| direction == Download = rsyncDownloadOptions o
		| otherwise = rsyncUploadOptions o
