{- git-remote-daemon, git-annex-shell over ssh transport
 -
 - Copyright 2014 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module RemoteDaemon.Transport.Ssh (transport, transportUsingCmd) where

import Annex.Common
import Annex.Ssh
import RemoteDaemon.Types
import RemoteDaemon.Common
import Remote.Helper.Ssh
import qualified RemoteDaemon.Transport.Ssh.Types as SshRemote
import Utility.SimpleProtocol
import qualified Git
import Git.Command
import Annex.ChangedRefs

import Control.Concurrent.STM
import Control.Concurrent.Async

transport :: Transport
transport rr@(RemoteRepo r _) url h ichan ochan = do
	v <- liftAnnex h $ git_annex_shell ConsumeStdin r "notifychanges" [] []
	case v of
		Nothing -> noop
		Just (cmd, params) -> transportUsingCmd cmd params rr url h ichan ochan

transportUsingCmd :: FilePath -> [CommandParam] -> Transport
transportUsingCmd cmd params rr@(RemoteRepo r gc) url h@(TransportHandle (LocalRepo g) s) ichan ochan = do
	-- enable ssh connection caching wherever inLocalRepo is called
	g' <- liftAnnex h $ sshOptionsTo r gc g
	let transporthandle = TransportHandle (LocalRepo g') s
	transportUsingCmd' cmd params rr url transporthandle ichan ochan

transportUsingCmd' :: FilePath -> [CommandParam] -> Transport
transportUsingCmd' cmd params (RemoteRepo r gc) url transporthandle ichan ochan =
	robustConnection 1 $ do
		(Just toh, Just fromh, Just errh, pid) <-
			createProcess (proc cmd (toCommand params))
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
  where
	send msg = atomically $ writeTChan ochan msg

	fetch = do
		send (SYNCING url)
		ok <- inLocalRepo transporthandle $
			runBool [Param "fetch", Param $ Git.repoDescribe r]
		send (DONESYNCING url ok)
		
	handlestdout fromh = do
		ml <- getProtocolLine fromh
		case parseMessage =<< ml of
			Just SshRemote.READY -> do
				send (CONNECTED url)
				handlestdout fromh
			Just (SshRemote.CHANGED (ChangedRefs shas)) -> do
				whenM (checkShouldFetch gc transporthandle shas) $
					fetch
				handlestdout fromh
			-- avoid reconnect on protocol error
			Nothing -> return ConnectionStopping
	
	handlecontrol = do
		msg <- atomically $ readTChan ichan
		case msg of
			STOP -> return ConnectionStopping
			LOSTNET -> return ConnectionStopping
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
				return ConnectionStopping
			else handlestderr errh
