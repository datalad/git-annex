{- git-remote-annex program
 -
 - Copyright 2024 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module CmdLine.GitRemoteAnnex where

import Common
import qualified Annex
import qualified Git.CurrentRepo
import Annex.UUID
import Annex.Action

run :: [String] -> IO ()
run (_remotename:address:[]) = forever $
	getLine >>= \case
		l -> giveup $ "gitremote-helpers protocol error at " ++ show l
run (_remotename:[]) = giveup "remote address not configured"
run _ = giveup "expected remote name and address parameters"
