{- P2P protocol over HTTP, running client actions
 -
 - https://git-annex.branchable.com/design/p2p_protocol_over_http/
 -
 - Copyright 2024 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module P2P.Http.Client where

import Types
import Annex.Url

#ifdef WITH_SERVANT
import Annex.UUID
import Types.Remote
import P2P.Protocol (ProtocolVersion(..))
import P2P.Http.Types
import P2P.Http.Url
import Servant.Client
#endif

p2pHttpClient
	:: Remote
	-> (String -> Annex a)
#ifdef WITH_SERVANT
	-> (ClientEnv -> ProtocolVersion -> B64UUID ServerSide -> B64UUID ClientSide -> [B64UUID Bypass] -> Maybe Auth -> Annex a)
#endif
	-> Annex a
#ifdef WITH_SERVANT
p2pHttpClient rmt fallback httpaction =
	case p2pHttpBaseUrl <$> remoteAnnexP2PHttpUrl (gitconfig rmt) of
		Nothing -> error "internal"
		Just baseurl -> do
			myuuid <- getUUID
			mgr <- httpManager <$> getUrlOptions
			let clientenv = mkClientEnv mgr baseurl
			-- TODO: try other protocol versions
			-- TODO: authentication
			-- TODO: catch 404 etc
			httpaction clientenv 
				(ProtocolVersion 3)
				(B64UUID (uuid rmt))
				(B64UUID myuuid)
				[]
				Nothing
#else
runP2PHttpClient rmt fallback = fallback "This remote uses an annex+http url, but this version of git-annex is not build with support for that."
#endif
