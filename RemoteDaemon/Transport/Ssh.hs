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
import Git.Command

import Control.Concurrent.Chan
import Control.Concurrent.Async
import System.Process (std_in, std_out)

transport :: Transport
transport r remotename transporthandle ichan ochan = do
	v <- liftAnnex transporthandle $ git_annex_shell r "notifychanges" [] []
	case v of
		Nothing -> noop
		Just (cmd, params) -> go cmd (toCommand params)
  where
	go cmd params = do
		(Just toh, Just fromh, _, pid) <- createProcess (proc cmd params)
			{ std_in = CreatePipe
			, std_out = CreatePipe
			}
		
		let shutdown = do
			hClose toh
			hClose fromh
			void $ waitForProcess pid
			send DISCONNECTED

		let fromshell = forever $ do
			l <- hGetLine fromh
			case parseMessage l of
				Just SshRemote.READY -> send CONNECTED
				Just (SshRemote.CHANGED shas) ->
					whenM (checkNewShas transporthandle shas) $
						fetch
				Nothing -> shutdown

		-- The only control message that matters is STOP.
		--
		-- Note that a CHANGED control message is not handled;
		-- we don't push to the ssh remote. The assistant
		-- and git-annex sync both handle pushes, so there's no
		-- need to do it here.
		let handlecontrol = forever $ do
			msg <- readChan ichan
			case msg of
				STOP -> ioError (userError "done")
				_ -> noop

		-- Run both threads until one finishes.
		void $ tryIO $ concurrently fromshell handlecontrol
		shutdown

	send msg = writeChan ochan (msg remotename)

	fetch = do
		send SYNCING
		ok <- inLocalRepo transporthandle $
			runBool [Param "fetch", Param remotename]
		send (DONESYNCING ok)
