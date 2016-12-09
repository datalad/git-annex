{- git-remote-daemon, git-annex-shell notifychanges protocol types
 -
 - Copyright 2014 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module RemoteDaemon.Transport.Ssh.Types (
	Notification(..),
	Proto.serialize,
	Proto.deserialize,
	Proto.formatMessage,
) where

import qualified Utility.SimpleProtocol as Proto
import Annex.ChangedRefs (ChangedRefs)

data Notification
	= READY
	| CHANGED ChangedRefs

instance Proto.Sendable Notification where
	formatMessage READY = ["READY"]
	formatMessage (CHANGED shas) = ["CHANGED", Proto.serialize shas]

instance Proto.Receivable Notification where
	parseCommand "READY" = Proto.parse0 READY
	parseCommand "CHANGED" = Proto.parse1 CHANGED
	parseCommand _ = Proto.parseFail
