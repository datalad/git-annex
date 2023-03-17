{- git-annex remote access with ssh and git-annex-shell
 -
 - Copyright 2011-2022 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Remote.Helper.Ssh where

import Annex.Common
import qualified Annex
import qualified Git
import qualified Git.Url
import Annex.UUID
import Annex.Ssh
import CmdLine.GitAnnexShell.Fields (Field, fieldName)
import qualified CmdLine.GitAnnexShell.Fields as Fields
import Remote.Helper.Messages
import Utility.Metered
import Utility.Rsync
import Utility.SshHost
import Types.Remote
import Types.Transfer
import Config
import qualified P2P.Protocol as P2P
import qualified P2P.IO as P2P
import qualified P2P.Annex as P2P

import Control.Concurrent.STM

toRepo :: ConsumeStdin -> Git.Repo -> RemoteGitConfig -> SshCommand -> Annex (FilePath, [CommandParam])
toRepo cs r gc remotecmd = do
	let host = maybe
		(giveup "bad ssh url")
		(either error id . mkSshHost)
		(Git.Url.hostuser r)
	sshCommand cs (host, Git.Url.port r) gc remotecmd

{- Generates parameters to run a git-annex-shell command on a remote
 - repository. -}
git_annex_shell :: ConsumeStdin -> Git.Repo -> String -> [CommandParam] -> [(Field, String)] -> Annex (Maybe (FilePath, [CommandParam]))
git_annex_shell cs r command params fields
	| not $ Git.repoIsUrl r = do
		shellopts <- getshellopts
		return $ Just (shellcmd, shellopts ++ fieldopts)
	| Git.repoIsSsh r = do
		gc <- Annex.getRemoteGitConfig r
		u <- getRepoUUID r
		shellopts <- getshellopts
		let sshcmd = unwords $
			fromMaybe shellcmd (remoteAnnexShell gc)
				: map shellEscape (toCommand shellopts) ++
			uuidcheck u ++
			map shellEscape (toCommand fieldopts)
		Just <$> toRepo cs r gc sshcmd
	| otherwise = return Nothing
  where
	dir = Git.repoPath r
	shellcmd = "git-annex-shell"
	getshellopts = do
		debugenabled <- Annex.getRead Annex.debugenabled
		let params' = if debugenabled
			then Param "--debug" : params
			else params
		return (Param command : File (fromRawFilePath dir) : params')
	uuidcheck NoUUID = []
	uuidcheck u@(UUID _) = ["--uuid", fromUUID u]
	fieldopts
		| null fields = []
		| otherwise = fieldsep : map fieldopt fields ++ [fieldsep]
	fieldsep = Param "--"
	fieldopt (field, value) = Param $
		fieldName field ++ "=" ++ value

{- Uses a supplied function (such as boolSystem) to run a git-annex-shell
 - command on a remote.
 -
 - Or, if the remote does not support running remote commands, returns
 - a specified error value. -}
onRemote 
	:: ConsumeStdin
	-> Git.Repo
	-> (FilePath -> [CommandParam] -> Annex a, Annex a)
	-> String
	-> [CommandParam]
	-> [(Field, String)]
	-> Annex a
onRemote cs r (with, errorval) command params fields = do
	s <- git_annex_shell cs r command params fields
	case s of
		Just (c, ps) -> with c ps
		Nothing -> errorval

{- Checks if a remote contains a key. -}
inAnnex :: Git.Repo -> Key -> Annex Bool
inAnnex r k = onRemote NoConsumeStdin r (runcheck, cantCheck r) "inannex"
	[Param $ serializeKey k] []
  where
	runcheck c p = liftIO $ dispatch =<< safeSystem c p
	dispatch ExitSuccess = return True
	dispatch (ExitFailure 1) = return False
	dispatch _ = cantCheck r

{- Removes a key from a remote. -}
dropKey :: Git.Repo -> Key -> Annex ()
dropKey r key = unlessM (dropKey' r key) $
	giveup "unable to remove key from remote"

dropKey' :: Git.Repo -> Key -> Annex Bool
dropKey' r key = onRemote NoConsumeStdin r (\f p -> liftIO (boolSystem f p), return False) "dropkey"
	[ Param "--quiet", Param "--force"
	, Param $ serializeKey key
	]
	[]

rsyncHelper :: OutputHandler -> Maybe MeterUpdate -> [CommandParam] -> Annex Bool
rsyncHelper oh m params = do
	unless (quietMode oh) $
		showOutput -- make way for progress bar
	a <- case m of
		Nothing -> return $ rsync params
		Just meter -> return $ rsyncProgress oh meter params
	ifM (liftIO a)
		( return True
		, do
			showLongNote "rsync failed -- run git annex again to resume file transfer"
			return False
		)

{- Generates rsync parameters that ssh to the remote and asks it
 - to either receive or send the key's content. -}
rsyncParamsRemote :: Remote -> Direction -> Key -> FilePath -> Annex [CommandParam]
rsyncParamsRemote r direction key file = do
	u <- getUUID
	repo <- getRepo r
	Just (shellcmd, shellparams) <- git_annex_shell ConsumeStdin repo
		(if direction == Download then "sendkey" else "recvkey")
		[ Param $ serializeKey key ]
		[(Fields.remoteUUID, fromUUID u)]
	-- Convert the ssh command into rsync command line.
	let eparam = rsyncShell (Param shellcmd:shellparams)
	o <- rsyncParams r direction
	return $ if direction == Download
		then o ++ rsyncopts eparam dummy (File file)
		else o ++ rsyncopts eparam (File file) dummy
  where
	rsyncopts ps source dest
		| end ps == [dashdash] = ps ++ [source, dest]
		| otherwise = ps ++ [dashdash, source, dest]
	dashdash = Param "--"
	{- The rsync shell parameter controls where rsync
	 - goes, so the source/dest parameter can be a dummy value,
	 - that just enables remote rsync mode.
	 - For maximum compatibility with some patched rsyncs,
	 - the dummy value needs to still contain a hostname,
	 - even though this hostname will never be used. -}
	dummy = Param "dummy:"

-- --inplace to resume partial files
--
-- Only use --perms when not on a crippled file system, as rsync
-- will fail trying to restore file perms onto a filesystem that does not
-- support them.
rsyncParams :: Remote -> Direction -> Annex [CommandParam]
rsyncParams r direction = do
	crippled <- crippledFileSystem
	return $ map Param $ catMaybes
		[ Just "--progress"
		, Just "--inplace"
		, if crippled then Nothing else Just "--perms"
		] 
		++ remoteAnnexRsyncOptions gc ++ dps
  where
	dps
		| direction == Download = remoteAnnexRsyncDownloadOptions gc
		| otherwise = remoteAnnexRsyncUploadOptions gc
	gc = gitconfig r

-- A connection over ssh to git-annex shell speaking the P2P protocol.
type P2PSshConnection = P2P.ClosableConnection 
	(P2P.RunState, P2P.P2PConnection, ProcessHandle)

closeP2PSshConnection :: P2PSshConnection -> IO (P2PSshConnection, Maybe ExitCode)
closeP2PSshConnection P2P.ClosedConnection = return (P2P.ClosedConnection, Nothing)
closeP2PSshConnection (P2P.OpenConnection (_st, conn, pid)) =
	-- mask async exceptions, avoid cleanup being interrupted
	uninterruptibleMask_ $ do
		P2P.closeConnection conn
		exitcode <- waitForProcess pid
		return (P2P.ClosedConnection, Just exitcode)

-- Pool of connections over ssh to git-annex-shell p2pstdio.
type P2PSshConnectionPool = TVar (Maybe P2PSshConnectionPoolState)

data P2PSshConnectionPoolState
	= P2PSshConnections [P2PSshConnection]
	-- Remotes using an old version of git-annex-shell don't support P2P
	| P2PSshUnsupported

mkP2PSshConnectionPool :: Annex P2PSshConnectionPool
mkP2PSshConnectionPool = liftIO $ newTVarIO Nothing

-- Takes a connection from the pool, if any are available, otherwise
-- tries to open a new one.
getP2PSshConnection :: Remote -> P2PSshConnectionPool -> Annex (Maybe P2PSshConnection)
getP2PSshConnection r connpool = getexistingconn >>= \case
	Nothing -> return Nothing
	Just Nothing -> openP2PSshConnection r connpool
	Just (Just c) -> return (Just c)
  where
	getexistingconn = liftIO $ atomically $ readTVar connpool >>= \case
		Just P2PSshUnsupported -> return Nothing
		Just (P2PSshConnections (c:cs)) -> do
			writeTVar connpool (Just (P2PSshConnections cs))
			return (Just (Just c))
		Just (P2PSshConnections []) -> return (Just Nothing)
		Nothing -> return (Just Nothing)

-- Add a connection to the pool, unless it's closed.
storeP2PSshConnection :: P2PSshConnectionPool -> P2PSshConnection -> IO ()
storeP2PSshConnection _ P2P.ClosedConnection = return ()
storeP2PSshConnection connpool conn = atomically $ modifyTVar' connpool $ \case
	Just (P2PSshConnections cs) -> Just (P2PSshConnections (conn:cs))
	_ -> Just (P2PSshConnections [conn])

-- Try to open a P2PSshConnection.
-- The new connection is not added to the pool, so it's available
-- for the caller to use.
-- If the remote does not support the P2P protocol, that's remembered in 
-- the connection pool.
openP2PSshConnection :: Remote -> P2PSshConnectionPool -> Annex (Maybe P2PSshConnection)
openP2PSshConnection r connpool = do
	u <- getUUID
	let ps = [Param (fromUUID u)]
	repo <- getRepo r
	git_annex_shell ConsumeStdin repo "p2pstdio" ps [] >>= \case
		Nothing -> do
			liftIO $ rememberunsupported
			return Nothing
		Just (cmd, params) -> start cmd params =<< getRepo r
  where
	start cmd params repo = liftIO $ do
		(Just from, Just to, Nothing, pid) <- createProcess $
			(proc cmd (toCommand params))
				{ std_in = CreatePipe
				, std_out = CreatePipe
				}
		pidnum <- getPid pid
		let conn = P2P.P2PConnection
			{ P2P.connRepo = repo
			, P2P.connCheckAuth = const False
			, P2P.connIhdl = to
			, P2P.connOhdl = from
			, P2P.connIdent = P2P.ConnIdent $
				Just $ "ssh connection " ++ show pidnum
			}
		runst <- P2P.mkRunState P2P.Client
		let c = P2P.OpenConnection (runst, conn, pid)
		-- When the connection is successful, the remote
		-- will send an AUTH_SUCCESS with its uuid.
		let proto = P2P.postAuth $
			P2P.negotiateProtocolVersion P2P.maxProtocolVersion
		tryNonAsync (P2P.runNetProto runst conn proto) >>= \case
			Right (Right (Just theiruuid)) | theiruuid == uuid r ->
				return $ Just c
			_ -> do
				(cclosed, exitcode) <- closeP2PSshConnection c
				-- ssh exits 255 when unable to connect to
				-- server.
				if exitcode == Just (ExitFailure 255)
					then return (Just cclosed)
					else do
						rememberunsupported
						return Nothing
	rememberunsupported = atomically $
		modifyTVar' connpool $
			maybe (Just P2PSshUnsupported) Just

-- Runs a P2P Proto action on a remote when it supports that,
-- otherwise the fallback action.
runProto :: Remote -> P2PSshConnectionPool -> Annex a -> P2P.Proto a -> Annex (Maybe a)
runProto r connpool onerr proto = Just <$>
	(getP2PSshConnection r connpool >>= maybe onerr go)
  where
	go c = do
		(c', v) <- runProtoConn proto c
		case v of
			Just res -> do
				liftIO $ storeP2PSshConnection connpool c'
				return res
			Nothing -> onerr

runProtoConn :: P2P.Proto a -> P2PSshConnection -> Annex (P2PSshConnection, Maybe a)
runProtoConn _ P2P.ClosedConnection = return (P2P.ClosedConnection, Nothing)
runProtoConn a conn@(P2P.OpenConnection (runst, c, _)) = do
	P2P.runFullProto runst c a >>= \case
		Right r -> return (conn, Just r)
		-- When runFullProto fails, the connection is no longer
		-- usable, so close it.
		Left e -> do
			warning $ "Lost connection (" ++ P2P.describeProtoFailure e ++ ")"
			conn' <- fst <$> liftIO (closeP2PSshConnection conn)
			return (conn', Nothing)

-- Allocates a P2P ssh connection from the pool, and runs the action with it,
-- returning the connection to the pool once the action is done.
--
-- If the remote does not support the P2P protocol, runs the fallback
-- action instead.
withP2PSshConnection
	:: Remote
	-> P2PSshConnectionPool
	-> Annex a
	-> (P2PSshConnection -> Annex (P2PSshConnection, a))
	-> Annex a
withP2PSshConnection r connpool fallback a = bracketOnError get cache go
  where
	get = getP2PSshConnection r connpool
	cache (Just conn) = liftIO $ storeP2PSshConnection connpool conn
	cache Nothing = return ()
	go (Just conn) = do
		(conn', res) <- a conn
		cache (Just conn')
		return res
	go Nothing = fallback
