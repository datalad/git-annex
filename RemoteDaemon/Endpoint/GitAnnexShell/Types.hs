{- git-remote-daemon, git-annex-shell endpoint, datatypes
 -
 - Copyright 2014 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module RemoteDaemon.EndPoint.GitAnnexShell.Types where

import Common.Annex
import qualified Git.Types as Git
import qualified Utility.SimpleProtocol as Proto
import RemoteDaemon.Types (RemoteName, RefList)

data Notifications
	= CHANGED RemoteName RefList

instance Proto.Sendable Notifications where
	formatMessage (CHANGED remote refs) =
		["CHANGED"
		, Proto.serialize remote
		, Proto.serialize refs
		]

instance Proto.Receivable Notifications where
	parseCommand "CHANGED" = Proto.parse2 CHANGED
