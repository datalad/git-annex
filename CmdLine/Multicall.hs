{- git-annex multicall binary
 -
 - Copyright 2024 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module CmdLine.Multicall where

import qualified Data.Map as M

-- Commands besides git-annex that can be run by the multicall binary.
--
-- The reason git-annex itself is not included here is because the program
-- can be renamed to any other name than these and will behave the same as
-- git-annex.
data OtherMultiCallCommand
	= GitAnnexShell
	| GitRemoteAnnex
	| GitRemoteTorAnnex

otherMulticallCommands :: M.Map String OtherMultiCallCommand
otherMulticallCommands = M.fromList
	[ ("git-annex-shell", GitAnnexShell)
	, ("git-remote-annex", GitRemoteAnnex)
	, ("git-remote-tor-annex", GitRemoteTorAnnex)
	]

