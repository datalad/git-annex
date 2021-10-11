{- git-remote-daemon, git-annex-shell over ssh transport
 -
 - Copyright 2014 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
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
transportUsingCmd cmd params rr@(RemoteRepo r gc) url h@(TransportHandle (LocalRepo g) st rd) ichan ochan = do
	-- enable ssh connection caching wherever inLocalRepo is called
	g' <- liftAnnex h $ sshOptionsTo r gc g
	let transporthandle = TransportHandle (LocalRepo g') st rd
	transportUsingCmd' cmd params rr url transporthandle ichan ochan

transportUsingCmd' :: FilePath -> [CommandParam] -> Transport
transportUsingCmd' cmd params (RemoteRepo r gc) url transporthandle ichan ochan =
	robustConnection 1 $ withCreateProcess p go
  where
	p = (proc cmd (toCommand params))
		{ std_in = CreatePipe
		, std_out = CreatePipe
		}

	go (Just toh) (Just fromh) Nothing pid = do
		-- Run all threads until one finishes and get the status
		-- of the first to finish. Cancel the rest.
		status <- catchDefaultIO (Right ConnectionClosed) $
				handlestdout fromh
					`race` handlecontrol

		send (DISCONNECTED url)
		hClose toh
		hClose fromh
		void $ waitForProcess pid

		return $ either id id status
	go _ _ _ _ = error "internal"

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
