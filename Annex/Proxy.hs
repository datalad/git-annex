{- proxying
 -
 - Copyright 2024 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Annex.Proxy where

import Annex.Common
import P2P.Proxy
import P2P.Protocol
import P2P.IO
import qualified Remote
import Remote.Helper.Ssh (openP2PShellConnection', closeP2PShellConnection)

-- FIXME: Support special remotes.
proxySshRemoteSide :: ProtocolVersion -> Bypass -> Remote -> Annex RemoteSide
proxySshRemoteSide clientmaxversion bypass remote = 
	mkRemoteSide (Remote.uuid remote) $
		openP2PShellConnection' remote clientmaxversion bypass >>= \case
			Just conn@(OpenConnection (remoterunst, remoteconn, _)) ->
				return $ Just 
					( remoterunst
					, remoteconn
					, void $ liftIO $ closeP2PShellConnection conn
					)
			_  -> return Nothing
