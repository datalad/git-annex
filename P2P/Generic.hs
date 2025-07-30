{- P2P protocol, generic transports.
 -
 - See doc/design/generic_p2p_transport.mdwn
 -
 - Copyright 2025 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module P2P.Generic where

import Common
import P2P.Address

genericP2PCommand :: P2PNetName -> String
genericP2PCommand (P2PNetName netname) = "git-annex-p2p-" ++ netname

connectGenericP2P :: P2PNetName -> UnderlyingP2PAddress -> CreateProcess
connectGenericP2P netname (UnderlyingP2PAddress address) =
	(proc (genericP2PCommand netname) [address])
		{ std_in = CreatePipe
		, std_out = CreatePipe
		}

socketGenericP2P :: P2PNetName -> UnderlyingP2PAddress -> CreateProcess
socketGenericP2P netname (UnderlyingP2PAddress address) =
	(proc (genericP2PCommand netname) ["socket", address])
		{ std_out = CreatePipe
		}

addressGenericP2P :: P2PNetName -> CreateProcess
addressGenericP2P netname =
	(proc (genericP2PCommand netname) ["address"])
		{ std_out = CreatePipe
		}

getSocketGenericP2P :: P2PNetName -> UnderlyingP2PAddress -> IO (Maybe (OsPath, ProcessHandle))
getSocketGenericP2P netname address = do
	(Nothing, Just hin, Nothing, pid) <- createProcess $
		socketGenericP2P netname address
	hGetLineUntilExitOrEOF pid hin >>= \case
		Just l | not (null l) -> return $ Just (toOsPath l, pid)
		_ -> return Nothing

getAddressGenericP2P :: P2PNetName -> IO [P2PAddress]
getAddressGenericP2P netname = do
	(Nothing, Just hin, Nothing, pid) <- createProcess $
		addressGenericP2P netname
	go [] hin pid
  where
	go addrs hin pid = hGetLineUntilExitOrEOF pid hin >>= \case
		Just l
			| not (null l) -> 
				let addr = P2PAnnex netname (UnderlyingP2PAddress l)
				in go (addr:addrs) hin pid
			| otherwise -> go addrs hin pid
		Nothing -> return addrs
