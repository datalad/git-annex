{- git-annex ssh interface, with connection caching
 -
 - Copyright 2012,2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Annex.Ssh (
	sshCachingOptions,
	sshCleanup,
	sshCacheDir,
	sshReadPort,
) where

import qualified Data.Map as M
import Data.Hash.MD5
import System.Process (cwd)

import Common.Annex
import Annex.LockPool
import qualified Build.SysConfig as SysConfig
import qualified Annex
import Config
import Utility.Env
#ifndef mingw32_HOST_OS
import Annex.Perms
#endif

{- Generates parameters to ssh to a given host (or user@host) on a given
 - port, with connection caching. -}
sshCachingOptions :: (String, Maybe Integer) -> [CommandParam] -> Annex [CommandParam]
sshCachingOptions (host, port) opts = go =<< sshInfo (host, port)
  where
	go (Nothing, params) = ret params
	go (Just socketfile, params) = do
		cleanstale
		liftIO $ createDirectoryIfMissing True $ parentDir socketfile
		lockFile $ socket2lock socketfile
		ret params
	ret ps = return $ ps ++ opts ++ portParams port ++ [Param "-T"]
	-- If the lock pool is empty, this is the first ssh of this
	-- run. There could be stale ssh connections hanging around
	-- from a previous git-annex run that was interrupted.
	cleanstale = whenM (not . any isLock . M.keys <$> getPool)
		sshCleanup

{- Returns a filename to use for a ssh connection caching socket, and
 - parameters to enable ssh connection caching. -}
sshInfo :: (String, Maybe Integer) -> Annex (Maybe FilePath, [CommandParam])
sshInfo (host, port) = go =<< sshCacheDir
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
	, Params "-o ControlMaster=auto -o ControlPersist=yes"
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
		createDirectoryIfMissing True tmpdir
		return tmpdir

portParams :: Maybe Integer -> [CommandParam]
portParams Nothing = []
portParams (Just port) = [Param "-p", Param $ show port]

{- Stop any unused ssh processes. -}
sshCleanup :: Annex ()
sshCleanup = go =<< sshCacheDir
  where
	go Nothing = noop
	go (Just dir) = do
		sockets <- liftIO $ filter (not . isLock)
			<$> catchDefaultIO [] (dirContents dir)
		forM_ sockets cleanup
	cleanup socketfile = do
#ifndef mingw32_HOST_OS
		-- Drop any shared lock we have, and take an
		-- exclusive lock, without blocking. If the lock
		-- succeeds, nothing is using this ssh, and it can
		-- be stopped.
		let lockfile = socket2lock socketfile
		unlockFile lockfile
		mode <- annexFileMode
		fd <- liftIO $ noUmask mode $
			openFd lockfile ReadWrite (Just mode) defaultFileFlags
		v <- liftIO $ tryIO $
			setLock fd (WriteLock, AbsoluteSeek, 0, 0)
		case v of
			Left _ -> noop
			Right _ -> stopssh socketfile
		liftIO $ closeFd fd
#else
		stopssh socketfile
#endif
	stopssh socketfile = do
		let (dir, base) = splitFileName socketfile
		let params = sshConnectionCachingParams base
		-- "ssh -O stop" is noisy on stderr even with -q
		void $ liftIO $ catchMaybeIO $
			withQuietOutput createProcessSuccess $
				(proc "ssh" $ toCommand $
					[ Params "-O stop"
					] ++ params ++ [Param "any"])
					{ cwd = Just dir }
		-- Cannot remove the lock file; other processes may
		-- be waiting on our exclusive lock to use it.

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
