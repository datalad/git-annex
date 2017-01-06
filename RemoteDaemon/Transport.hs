{- git-remote-daemon transports
 -
 - Copyright 2014 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module RemoteDaemon.Transport where

import RemoteDaemon.Types
import qualified RemoteDaemon.Transport.Ssh
import qualified RemoteDaemon.Transport.GCrypt
import qualified RemoteDaemon.Transport.Tor
import qualified Git.GCrypt
import P2P.Address (torAnnexScheme)

import qualified Data.Map as M

-- Corresponds to uriScheme
type TransportScheme = String

remoteTransports :: M.Map TransportScheme Transport
remoteTransports = M.fromList
	[ ("ssh:", RemoteDaemon.Transport.Ssh.transport)
	, (Git.GCrypt.urlScheme, RemoteDaemon.Transport.GCrypt.transport)
	, (torAnnexScheme, RemoteDaemon.Transport.Tor.transport)
	]

remoteServers :: [Server]
remoteServers = [RemoteDaemon.Transport.Tor.server]
