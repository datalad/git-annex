{- git-annex ssh interface, with connection caching
 -
 - Copyright 2012-2015 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Annex.Ssh (
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

import qualified Data.Map as M
import Data.Hash.MD5
import System.Exit

import Common.Annex
import Annex.LockFile
import qualified Build.SysConfig as SysConfig
import qualified Annex
import qualified Git
import qualified Git.Url
import Config
import Annex.Path
import Utility.Env
import Types.CleanupActions
import Annex.Index (addGitEnv)
#ifndef mingw32_HOST_OS
import Annex.Perms
import Utility.LockPool
#endif

{- Generates parameters to ssh to a given host (or user@host) on a given
 - port. This includes connection caching parameters, and any ssh-options. -}
sshOptions :: (String, Maybe Integer) -> RemoteGitConfig -> [CommandParam] -> Annex [CommandParam]
sshOptions (host, port) gc opts = go =<< sshCachingInfo (host, port)
  where
	go (Nothing, params) = ret params
	go (Just socketfile, params) = do
		prepSocket socketfile
		ret params
	ret ps = return $ concat
		[ ps
		, map Param (remoteAnnexSshOptions gc)
		, opts
		, portParams port
		, [Param "-T"]
		]

{- Returns a filename to use for a ssh connection caching socket, and
 - parameters to enable ssh connection caching. -}
sshCachingInfo :: (String, Maybe Integer) -> Annex (Maybe FilePath, [CommandParam])
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
	| SysConfig.sshconnectioncaching = ifM crippledFileSystem
		( maybe (return Nothing) usetmpdir =<< gettmpdir
		, ifM (fromMaybe True . annexSshCaching <$> Annex.getGitConfig)
			( Just <$> fromRepo gitAnnexSshDir
			, return Nothing
			)
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

{- Prepare to use a socket file. Locks a lock file to prevent
 - other git-annex processes from stopping the ssh on this socket. -}
prepSocket :: FilePath -> Annex ()
prepSocket socketfile = do
	-- If the lock pool is empty, this is the first ssh of this
	-- run. There could be stale ssh connections hanging around
	-- from a previous git-annex run that was interrupted.
	whenM (not . any isLock . M.keys <$> getLockCache)
		sshCleanup
	-- Cleanup at end of this run.
	Annex.addCleanup SshCachingCleanup sshCleanup

	liftIO $ createDirectoryIfMissing True $ parentDir socketfile
	lockFileCached $ socket2lock socketfile

enumSocketFiles :: Annex [FilePath]
enumSocketFiles = go =<< sshCacheDir
  where
	go Nothing = return []
	go (Just dir) = liftIO $ filter (not . isLock)
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
		v <- liftIO $ noUmask mode $ tryLockExclusive (Just mode) lockfile
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
hostport2socket :: String -> Maybe Integer -> FilePath
hostport2socket host Nothing = hostport2socket' host
hostport2socket host (Just port) = hostport2socket' $ host ++ "!" ++ show port
hostport2socket' :: String -> FilePath
hostport2socket' s
	| length s > lengthofmd5s = md5s (Str s)
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
 - additional parameters to pass to ssh. -}
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
 - to set GIT_SSH=git-annex, and sets sshOptionsEnv. -}
sshOptionsTo :: Git.Repo -> RemoteGitConfig -> Git.Repo -> Annex Git.Repo
sshOptionsTo remote gc g 
	| not (Git.repoIsUrl remote) || Git.repoIsHttp remote = unchanged
	| otherwise = case Git.Url.hostuser remote of
		Nothing -> unchanged
		Just host -> do
			(msockfile, _) <- sshCachingInfo (host, Git.Url.port remote)
			case msockfile of
				Nothing -> use []
				Just sockfile -> do
					prepSocket sockfile
					use (sshConnectionCachingParams sockfile)
  where
	unchanged = return g

	use opts = do
		let sshopts = 
			[ opts
			, map Param (remoteAnnexSshOptions gc)
			]
		if null sshopts
			then unchanged
			else do
				let val = toSshOptionsEnv (concat sshopts)
				command <- liftIO programPath
				liftIO $ do
					g' <- addGitEnv g sshOptionsEnv val
					addGitEnv g' "GIT_SSH" command

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
