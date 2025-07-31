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
import Annex.ExternalAddonProcess

genericP2PCommand :: P2PNetName -> String
genericP2PCommand (P2PNetName netname) = "git-annex-p2p-" ++ netname

connectGenericP2P :: P2PNetName -> UnderlyingP2PAddress -> IO (Handle, Handle, ProcessHandle)
connectGenericP2P netname (UnderlyingP2PAddress address) =
	startExternalAddonProcess
		(\p -> p 
			{ std_in = CreatePipe
			, std_out = CreatePipe
			})
		(genericP2PCommand netname) [Param address]
		>>= \case
			Right (_, (Just hin, Just hout, Nothing, pid)) ->
				return (hin, hout, pid)
			Right _ -> giveup "internal"
			Left (ProgramNotInstalled msg) -> giveup msg
			Left (ProgramFailure msg) -> giveup msg

socketGenericP2P :: P2PNetName -> UnderlyingP2PAddress -> OsPath -> IO ProcessHandle
socketGenericP2P netname (UnderlyingP2PAddress address) socketfile = do
	startExternalAddonProcess id
		(genericP2PCommand netname) [Param address, File (fromOsPath socketfile)]
		>>= \case
			Right (_, (Nothing, Nothing, Nothing, pid)) ->
				return pid
			Right _ -> giveup "internal"
			Left (ProgramNotInstalled msg) -> giveup msg
			Left (ProgramFailure msg) -> giveup msg

getAddressGenericP2P :: P2PNetName -> IO [P2PAddress]
getAddressGenericP2P netname =
	startExternalAddonProcess
		(\p -> p { std_out = CreatePipe })
		(genericP2PCommand netname) [Param "address"]
		>>= \case
			Right (_, (Nothing, Just hin, Nothing, pid)) ->
				go [] hin pid
			Right _ -> giveup "internal"
			Left (ProgramNotInstalled msg) -> giveup msg
			Left (ProgramFailure msg) -> giveup msg
  where
	go addrs hin pid = hGetLineUntilExitOrEOF pid hin >>= \case
		Just l
			| not (null l) -> 
				let addr = P2PAnnex netname (UnderlyingP2PAddress l)
				in go (addr:addrs) hin pid
			| otherwise -> go addrs hin pid
		Nothing -> do
			waitForProcess pid >>= \case
				ExitSuccess -> return addrs
				ExitFailure _ -> giveup $ genericP2PCommand netname ++ " failed"
