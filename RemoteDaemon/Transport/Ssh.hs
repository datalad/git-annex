{- git-remote-daemon, git-annex-shell over ssh transport
 -
 - Copyright 2014 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module RemoteDaemon.Transport.Ssh (transport) where

import Common.Annex
import RemoteDaemon.Types
import RemoteDaemon.Common
import Remote.Helper.Ssh
import qualified RemoteDaemon.Transport.Ssh.Types as SshRemote
import Utility.SimpleProtocol
import qualified Git
import Git.Command
import Utility.ThreadScheduler

import Control.Concurrent.Chan
import Control.Concurrent.Async
import System.Process (std_in, std_out, std_err)

transport :: Transport
transport r url transporthandle ichan ochan = do
	v <- liftAnnex transporthandle $ git_annex_shell r "notifychanges" [] []
	case v of
		Nothing -> noop
		Just (cmd, params) -> robustly 1 $
			connect cmd (toCommand params)
  where
	connect cmd params = do
		(Just toh, Just fromh, Just errh, pid) <-
			createProcess (proc cmd params)
			{ std_in = CreatePipe
			, std_out = CreatePipe
			, std_err = CreatePipe
			}
		
		-- Run all threads until one finishes and get the status
		-- of the first to finish. Cancel the rest.
		status <- catchDefaultIO (Right ConnectionClosed) $
			handlestderr errh
				`race` handlestdout fromh
					`race` handlecontrol

		send (DISCONNECTED url)
		hClose toh
		hClose fromh
		void $ waitForProcess pid

		return $ either (either id id) id status

	send msg = writeChan ochan msg

	fetch = do
		send (SYNCING url)
		ok <- inLocalRepo transporthandle $
			runBool [Param "fetch", Param $ Git.repoDescribe r]
		send (DONESYNCING url ok)
		
	handlestdout fromh = do
		l <- hGetLine fromh
		case parseMessage l of
			Just SshRemote.READY -> do
				send (CONNECTED url)
				handlestdout fromh
			Just (SshRemote.CHANGED shas) -> do
				whenM (checkNewShas transporthandle shas) $
					fetch
				handlestdout fromh
			-- avoid reconnect on protocol error
			Nothing -> return Stopping
	
	handlecontrol = do
		msg <- readChan ichan
		case msg of
			STOP -> return Stopping
			_ -> handlecontrol

	-- Old versions of git-annex-shell that do not support
	-- the notifychanges command will exit with a not very useful
	-- error message. Detect that error, and avoid reconnecting.
	-- Propigate all stderr.
	handlestderr errh = do
		s <- hGetSomeString errh 1024
		hPutStr stderr s
		hFlush stderr
		if "git-annex-shell: git-shell failed" `isInfixOf` s
			then do
				send $ WARNING url $ unwords
					[ "Remote", Git.repoDescribe r
					, "needs its git-annex upgraded"
					, "to 5.20140405 or newer"
					]
				return Stopping
			else handlestderr errh

data Status = Stopping | ConnectionClosed

{- Make connection robustly, with exponentioal backoff on failure. -}
robustly :: Int -> IO Status -> IO ()
robustly backoff a = handle =<< catchDefaultIO ConnectionClosed a
  where
	handle Stopping = return ()
	handle ConnectionClosed = do
		threadDelaySeconds (Seconds backoff)
		robustly increasedbackoff a

	increasedbackoff
		| b2 > maxbackoff = maxbackoff
		| otherwise = b2
	  where
	  	b2 = backoff * 2
		maxbackoff = 3600 -- one hour
