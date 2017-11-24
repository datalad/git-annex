{- git-remote-daemon, gcrypt transport
 -
 - Copyright 2015 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module RemoteDaemon.Transport.GCrypt (transport) where

import Annex.Common
import RemoteDaemon.Types
import RemoteDaemon.Common
import RemoteDaemon.Transport.Ssh (transportUsingCmd)
import Git.GCrypt
import Remote.Helper.Ssh
import Remote.GCrypt (accessShellConfig)
import Annex.Ssh

transport :: Transport
transport rr@(RemoteRepo r gc) url h@(TransportHandle (LocalRepo g) _) ichan ochan
	| accessShellConfig gc = do
		r' <- encryptedRemote g r
		v <- liftAnnex h $ git_annex_shell ConsumeStdin r' "notifychanges" [] []
		case v of
			Nothing -> noop
			Just (cmd, params) -> 
				transportUsingCmd cmd params rr url h ichan ochan
	| otherwise = noop
