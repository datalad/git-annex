{- git-annex ssh interface, with connection caching
 -
 - Copyright 2012-2017 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Annex.Ssh (
	ConsumeStdin(..),
	SshCommand,
	sshCommand,
	sshOptions,
	sshCacheDir,
	sshReadPort,
	forceSshCleanup,
	sshOptionsEnv,
	sshOptionsTo,
	inRepoWithSshOptionsTo,
	runSshOptions,
	sshAskPassEnv,
	runSshAskPass
) where

import Annex.Common
import Annex.LockFile
import qualified Build.SysConfig as SysConfig
import qualified Annex
import qualified Git
import qualified Git.Url
import Config
import Annex.Path
import Utility.Env
import Utility.FileSystemEncoding
import Utility.Hash
import Types.CleanupActions
import Types.Concurrency
import Git.Env
import Git.Ssh
#ifndef mingw32_HOST_OS
import Annex.Perms
import Annex.LockPool
#endif

import Control.Concurrent.STM

{- Some ssh commands are fed stdin on a pipe and so should be allowed to
 - consume it. But ssh commands that are not piped stdin should generally
 - not be allowed to consume the process's stdin. -}
data ConsumeStdin = ConsumeStdin | NoConsumeStdin

{- Generates a command to ssh to a given host (or user@host) on a given
 - port. This includes connection caching parameters, and any ssh-options.
 - If GIT_SSH or GIT_SSH_COMMAND is enabled, they are used instead. -}
sshCommand :: ConsumeStdin -> (SshHost, Maybe SshPort) -> RemoteGitConfig -> SshCommand -> Annex (FilePath, [CommandParam])
sshCommand cs (host, port) gc remotecmd = ifM (liftIO safe_GIT_SSH)
	( maybe go return
		=<< liftIO (gitSsh' host port remotecmd (consumeStdinParams cs))
	, go
	)
  where
	go = do
		ps <- sshOptions cs (host, port) gc []
		return ("ssh", Param (fromSshHost host):ps++[Param remotecmd])

{- Generates parameters to ssh to a given host (or user@host) on a given
 - port. This includes connection caching parameters, and any
 - ssh-options. Note that the host to ssh to and the command to run
 - are not included in the returned options. -}
sshOptions :: ConsumeStdin -> (SshHost, Maybe Integer) -> RemoteGitConfig -> [CommandParam] -> Annex [CommandParam]
sshOptions cs (host, port) gc opts = go =<< sshCachingInfo (host, port)
  where
	go (Nothing, params) = return $ mkparams cs params
	go (Just socketfile, params) = do
		prepSocket socketfile gc host (mkparams NoConsumeStdin params)
			
		return $ mkparams cs params
	mkparams cs' ps = concat
		[ ps
		, map Param (remoteAnnexSshOptions gc)
		, opts
		, portParams port
		, consumeStdinParams cs'
		, [Param "-T"]
		]

{- Due to passing -n to GIT_SSH and GIT_SSH_COMMAND, some settings
 - of those that expect exactly git's parameters will break. So only
 - use those if the user set GIT_ANNEX_USE_GIT_SSH to say it's ok. -}
safe_GIT_SSH :: IO Bool
safe_GIT_SSH = (== Just "1") <$> getEnv "GIT_ANNEX_USE_GIT_SSH"

consumeStdinParams :: ConsumeStdin -> [CommandParam]
consumeStdinParams ConsumeStdin = []
consumeStdinParams NoConsumeStdin = [Param "-n"]

{- Returns a filename to use for a ssh connection caching socket, and
 - parameters to enable ssh connection caching. -}
sshCachingInfo :: (SshHost, Maybe Integer) -> Annex (Maybe FilePath, [CommandParam])
sshCachingInfo (host, port) = go =<< sshCacheDir
  where
	go Nothing = return (Nothing, [])
	go (Just dir) = do
		r <- liftIO $ bestSocketPath $ dir </> hostport2socket host port
		return $ case r of
			Nothing -> (Nothing, [])
			Just socketfile -> (Just socketfile, sshConnectionCachingParams socketfile)

{- Given an absolute path to use for a socket file,
 - returns whichever is shorter of that or the relative path to the same
 - file.
 -
 - If no path can be constructed that is a valid socket, returns Nothing. -}
bestSocketPath :: FilePath -> IO (Maybe FilePath)
bestSocketPath abssocketfile = do
	relsocketfile <- liftIO $ relPathCwdToFile abssocketfile
	let socketfile = if length abssocketfile <= length relsocketfile
		then abssocketfile
		else relsocketfile
	return $ if valid_unix_socket_path (socketfile ++ sshgarbage)
			then Just socketfile
			else Nothing
  where
	-- ssh appends a 16 char extension to the socket when setting it
	-- up, which needs to be taken into account when checking
	-- that a valid socket was constructed.
	sshgarbage = replicate (1+16) 'X'

sshConnectionCachingParams :: FilePath -> [CommandParam]
sshConnectionCachingParams socketfile = 
	[ Param "-S", Param socketfile
	, Param "-o", Param "ControlMaster=auto"
	, Param "-o", Param "ControlPersist=yes"
	]

{- ssh connection caching creates sockets, so will not work on a
 - crippled filesystem. A GIT_ANNEX_TMP_DIR can be provided to use
 - a different filesystem. -}
sshCacheDir :: Annex (Maybe FilePath)
sshCacheDir
	| SysConfig.sshconnectioncaching = 
		ifM (fromMaybe True . annexSshCaching <$> Annex.getGitConfig)
			( ifM crippledFileSystem
				( maybe (return Nothing) usetmpdir =<< gettmpdir
				, Just <$> fromRepo gitAnnexSshDir 
				)
			, return Nothing
			)
	| otherwise = return Nothing
  where
	gettmpdir = liftIO $ getEnv "GIT_ANNEX_TMP_DIR"
	usetmpdir tmpdir = liftIO $ catchMaybeIO $ do
		let socktmp = tmpdir </> "ssh"
		createDirectoryIfMissing True socktmp
		return socktmp

portParams :: Maybe Integer -> [CommandParam]
portParams Nothing = []
portParams (Just port) = [Param "-p", Param $ show port]

{- Prepare to use a socket file for ssh connection caching.
 -
 - When concurrency is enabled, this blocks until a ssh connection
 - has been made to the host. So, any password prompting by ssh will
 - happen in this call, and only one ssh process will prompt at a time.
 -
 - Locks the socket lock file to prevent other git-annex processes from
 - stopping the ssh multiplexer on this socket.
 -}
prepSocket :: FilePath -> RemoteGitConfig -> SshHost -> [CommandParam] -> Annex ()
prepSocket socketfile gc sshhost sshparams = do
	-- There could be stale ssh connections hanging around
	-- from a previous git-annex run that was interrupted.
	-- This must run only once, before we have made any ssh connection,
	-- and any other prepSocket calls must block while it's run.
	tv <- Annex.getState Annex.sshstalecleaned
	join $ liftIO $ atomically $ do
		cleaned <- takeTMVar tv
		if cleaned
			then do
				putTMVar tv cleaned
				return noop
			else return $ do
				sshCleanup
				liftIO $ atomically $ putTMVar tv True
	-- Cleanup at shutdown.
	Annex.addCleanup SshCachingCleanup sshCleanup
	
	liftIO $ createDirectoryIfMissing True $ parentDir socketfile
	let socketlock = socket2lock socketfile

	c <- Annex.getState Annex.concurrency
	case c of
		Concurrent {}
			| annexUUID (remoteGitConfig gc) /= NoUUID ->
				makeconnection socketlock
		_ -> return ()
	
	lockFileCached socketlock
  where
	-- When the LockCache already has the socketlock in it,
	-- the connection has already been started. Otherwise,
	-- get the connection started now.
	makeconnection socketlock =
		whenM (isNothing <$> fromLockCache socketlock) $ do
			let startps = Param (fromSshHost sshhost) :
				sshparams ++ startSshConnection gc
			-- When we can start the connection in batch mode,
			-- ssh won't prompt to the console.
			(_, connected) <- liftIO $ processTranscript "ssh"
				(["-o", "BatchMode=true"]
				++ toCommand startps)
				Nothing
			unless connected $ do
				ok <- prompt $ liftIO $
					boolSystem "ssh" startps
				unless ok $
					warning $ "Unable to run git-annex-shell on remote " ++
						Git.repoDescribe (gitConfigRepo (remoteGitConfig gc))

-- Parameters to get ssh connected to the remote host,
-- by asking it to run a no-op command.
--
-- Could simply run "true", but the remote host may only
-- allow git-annex-shell to run. So, run git-annex-shell inannex
-- with the path to the remote repository and no other parameters,
-- which is a no-op supported by all versions of git-annex-shell.
startSshConnection :: RemoteGitConfig -> [CommandParam]
startSshConnection gc =
	[ Param "git-annex-shell"
	, Param "inannex"
	, File $ Git.repoPath $ gitConfigRepo $
		remoteGitConfig gc
	]

{- Find ssh socket files.
 -
 - The check that the lock file exists makes only socket files
 - that were set up by prepSocket be found. On some NFS systems,
 - a deleted socket file may linger for a while under another filename;
 - and this check makes such files be skipped since the corresponding lock
 - file won't exist.
 -}
enumSocketFiles :: Annex [FilePath]
enumSocketFiles = liftIO . go =<< sshCacheDir
  where
	go Nothing = return []
	go (Just dir) = filterM (doesFileExist . socket2lock)
		=<< filter (not . isLock)
		<$> catchDefaultIO [] (dirContents dir)

{- Stop any unused ssh connection caching processes. -}
sshCleanup :: Annex ()
sshCleanup = mapM_ cleanup =<< enumSocketFiles
  where
	cleanup socketfile = do
#ifndef mingw32_HOST_OS
		-- Drop any shared lock we have, and take an
		-- exclusive lock, without blocking. If the lock
		-- succeeds, nothing is using this ssh, and it can
		-- be stopped.
		--
		-- After ssh is stopped cannot remove the lock file;
		-- other processes may be waiting on our exclusive
		-- lock to use it.
		let lockfile = socket2lock socketfile
		unlockFile lockfile
		mode <- annexFileMode
		v <- noUmask mode $ tryLockExclusive (Just mode) lockfile
		case v of
			Nothing -> noop
			Just lck -> do
				forceStopSsh socketfile
				liftIO $ dropLock lck
#else
		forceStopSsh socketfile
#endif

{- Stop all ssh connection caching processes, even when they're in use. -}
forceSshCleanup :: Annex ()
forceSshCleanup = mapM_ forceStopSsh =<< enumSocketFiles

forceStopSsh :: FilePath -> Annex ()
forceStopSsh socketfile = do
	let (dir, base) = splitFileName socketfile
	let params = sshConnectionCachingParams base
	-- "ssh -O stop" is noisy on stderr even with -q
	void $ liftIO $ catchMaybeIO $
		withQuietOutput createProcessSuccess $
			(proc "ssh" $ toCommand $
				[ Param "-O", Param "stop" ] ++ 
				params ++ [Param "localhost"])
				{ cwd = Just dir }
	liftIO $ nukeFile socketfile

{- This needs to be as short as possible, due to limitations on the length
 - of the path to a socket file. At the same time, it needs to be unique
 - for each host.
 -}
hostport2socket :: SshHost -> Maybe Integer -> FilePath
hostport2socket host Nothing = hostport2socket' $ fromSshHost host
hostport2socket host (Just port) = hostport2socket' $
	fromSshHost host ++ "!" ++ show port
hostport2socket' :: String -> FilePath
hostport2socket' s
	| length s > lengthofmd5s = show $ md5 $ encodeBS s
	| otherwise = s
  where
	lengthofmd5s = 32

socket2lock :: FilePath -> FilePath
socket2lock socket = socket ++ lockExt

isLock :: FilePath -> Bool
isLock f = lockExt `isSuffixOf` f

lockExt :: String
lockExt = ".lock"

{- This is the size of the sun_path component of sockaddr_un, which
 - is the limit to the total length of the filename of a unix socket.
 -
 - On Linux, this is 108. On OSX, 104. TODO: Probe
 -}
sizeof_sockaddr_un_sun_path :: Int
sizeof_sockaddr_un_sun_path = 100

{- Note that this looks at the true length of the path in bytes, as it will
 - appear on disk. -}
valid_unix_socket_path :: FilePath -> Bool
valid_unix_socket_path f = length (decodeW8 f) < sizeof_sockaddr_un_sun_path

{- Parses the SSH port, and returns the other OpenSSH options. If
 - several ports are found, the last one takes precedence. -}
sshReadPort :: [String] -> (Maybe Integer, [String])
sshReadPort params = (port, reverse args)
  where
	(port,args) = aux (Nothing, []) params
	aux (p,ps) [] = (p,ps)
	aux (_,ps) ("-p":p:rest) = aux (readPort p, ps) rest
	aux (p,ps) (q:rest) | "-p" `isPrefixOf` q = aux (readPort $ drop 2 q, ps) rest
			    | otherwise = aux (p,q:ps) rest
	readPort p = fmap fst $ listToMaybe $ reads p

{- When this env var is set, git-annex runs ssh with the specified
 - options. (The options are separated by newlines.)
 -
 - This is a workaround for GIT_SSH not being able to contain
 - additional parameters to pass to ssh. (GIT_SSH_COMMAND can,
 - but is not supported by older versions of git.) -}
sshOptionsEnv :: String
sshOptionsEnv = "GIT_ANNEX_SSHOPTION"

toSshOptionsEnv :: [CommandParam] -> String
toSshOptionsEnv = unlines . toCommand

fromSshOptionsEnv :: String -> [CommandParam]
fromSshOptionsEnv = map Param . lines

{- Enables ssh caching for git push/pull to a particular
 - remote git repo. (Can safely be used on non-ssh remotes.)
 -
 - Also propigates any configured ssh-options.
 -
 - Like inRepo, the action is run with the local git repo.
 - But here it's a modified version, with gitEnv to set GIT_SSH=git-annex,
 - and sshOptionsEnv set so that git-annex will know what socket
 - file to use. -}
inRepoWithSshOptionsTo :: Git.Repo -> RemoteGitConfig -> (Git.Repo -> IO a) -> Annex a
inRepoWithSshOptionsTo remote gc a =
	liftIO . a =<< sshOptionsTo remote gc =<< gitRepo

{- To make any git commands be run with ssh caching enabled,
 - and configured ssh-options alters the local Git.Repo's gitEnv
 - to set GIT_SSH=git-annex, and set sshOptionsEnv when running git
 - commands.
 -
 - If GIT_SSH or GIT_SSH_COMMAND are enabled, this has no effect. -}
sshOptionsTo :: Git.Repo -> RemoteGitConfig -> Git.Repo -> Annex Git.Repo
sshOptionsTo remote gc localr
	| not (Git.repoIsUrl remote) || Git.repoIsHttp remote = unchanged
	| otherwise = case Git.Url.hostuser remote of
		Nothing -> unchanged
		Just host -> ifM (liftIO $ safe_GIT_SSH <&&> gitSshEnvSet)
			( unchanged
			, do
				let port = Git.Url.port remote
				let sshhost = either error id (mkSshHost host)
				(msockfile, cacheparams) <- sshCachingInfo (sshhost, port)
				case msockfile of
					Nothing -> use []
					Just sockfile -> do
						prepSocket sockfile gc sshhost $ concat
							[ cacheparams
							, map Param (remoteAnnexSshOptions gc)
							, portParams port
							, consumeStdinParams NoConsumeStdin
							, [Param "-T"]
							]
						use cacheparams
			)
  where
	unchanged = return localr

	use opts = do
		let sshopts = concat
			[ opts
			, map Param (remoteAnnexSshOptions gc)
			]
		if null sshopts
			then unchanged
			else do
				command <- liftIO programPath
				liftIO $ do
					localr' <- addGitEnv localr sshOptionsEnv
						(toSshOptionsEnv sshopts)
					addGitEnv localr' gitSshEnv command

runSshOptions :: [String] -> String -> IO ()
runSshOptions args s = do
	let args' = toCommand (fromSshOptionsEnv s) ++ args
	let p = proc "ssh" args'
	exitWith =<< waitForProcess . processHandle =<< createProcess p

{- When this env var is set, git-annex is being used as a ssh-askpass
 - program, and should read the password from the specified location,
 - and output it for ssh to read. -}
sshAskPassEnv :: String
sshAskPassEnv = "GIT_ANNEX_SSHASKPASS"

runSshAskPass :: FilePath -> IO ()
runSshAskPass passfile = putStrLn =<< readFile passfile
