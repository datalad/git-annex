{- git-annex-shell command
 -
 - Copyright 2014 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.NotifyChanges where

import Command
import Utility.DirWatcher
import Utility.DirWatcher.Types
import qualified Git
import Git.Sha
import RemoteDaemon.Transport.Ssh.Types

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM

cmd :: Command
cmd = noCommit $ 
	command "notifychanges" SectionPlumbing
		"sends notification when git refs are changed"
		paramNothing (withParams seek)

seek :: CmdParams -> CommandSeek
seek = withNothing start

start :: CommandStart
start = do
	-- This channel is used to accumulate notifcations,
	-- because the DirWatcher might have multiple threads that find
	-- changes at the same time.
	chan <- liftIO newTChanIO
	
	g <- gitRepo
	let refdir = Git.localGitDir g </> "refs"
	liftIO $ createDirectoryIfMissing True refdir

	let notifyhook = Just $ notifyHook chan
	let hooks = mkWatchHooks
		{ addHook = notifyhook
		, modifyHook = notifyhook
		}

	void $ liftIO $ watchDir refdir (const False) True hooks id

	let sender = do
		send READY
		forever $ send . CHANGED =<< drain chan
	
	-- No messages need to be received from the caller,
	-- but when it closes the connection, notice and terminate.
	let receiver = forever $ void getLine
	void $ liftIO $ concurrently sender receiver
	stop

notifyHook :: TChan Git.Sha -> FilePath -> Maybe FileStatus -> IO ()
notifyHook chan reffile _
	| ".lock" `isSuffixOf` reffile = noop
	| otherwise = void $ do
		sha <- catchDefaultIO Nothing $
			extractSha <$> readFile reffile
		maybe noop (atomically . writeTChan chan) sha

-- When possible, coalesce ref writes that occur closely together
-- in time. Delay up to 0.05 seconds to get more ref writes.
drain :: TChan Git.Sha -> IO [Git.Sha]
drain chan = do
	r <- atomically $ readTChan chan
	threadDelay 50000
	rs <- atomically $ drain' chan
	return (r:rs)

drain' :: TChan Git.Sha -> STM [Git.Sha]
drain' chan = loop []
  where
	loop rs = maybe (return rs) (\r -> loop (r:rs)) =<< tryReadTChan chan

send :: Notification -> IO ()
send n = do
	putStrLn $ unwords $ formatMessage n
	hFlush stdout
