{- git-annex-shell command
 -
 - Copyright 2014 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.NotifyChanges where

import Command
import Annex.ChangedRefs
import RemoteDaemon.Transport.Ssh.Types
import Utility.SimpleProtocol

import Control.Concurrent.Async

cmd :: Command
cmd = noCommit $ 
	command "notifychanges" SectionPlumbing
		"sends notification when git refs are changed"
		paramNothing (withParams seek)

seek :: CmdParams -> CommandSeek
seek = withNothing start

start :: CommandStart
start = go =<< watchChangedRefs
  where
	go (Just h) = do
		-- No messages need to be received from the caller,
		-- but when it closes the connection, notice and terminate.
		let receiver = forever $ void $ getProtocolLine stdin
		let sender = forever $ send . CHANGED =<< waitChangedRefs h

		liftIO $ send READY
		void $ liftIO $ concurrently sender receiver
		liftIO $ stopWatchingChangedRefs h
		stop
	go Nothing = stop

send :: Notification -> IO ()
send n = do
	putStrLn $ unwords $ formatMessage n
	hFlush stdout
