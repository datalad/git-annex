{- git-remote-daemon data types.
 -
 - Copyright 2014 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module RemoteDaemon.Types where

import Common
import qualified Annex
import qualified Git.Types as Git
import qualified Utility.SimpleProtocol as Proto
import Types.GitConfig
import Annex.ChangedRefs (ChangedRefs)

import Network.URI
import Control.Concurrent
import Control.Concurrent.STM

-- The URI of a remote is used to uniquely identify it (names change..)
newtype RemoteURI = RemoteURI URI
	deriving (Show)

-- A Transport for a particular git remote consumes some messages
-- from a Chan, and emits others to another Chan.
type Transport = RemoteRepo -> RemoteURI -> TransportHandle -> TChan Consumed -> TChan Emitted -> IO ()

-- A server for a Transport consumes some messages from a Chan in
-- order to learn about network changes, reloads, etc.
type Server = TChan Consumed -> TransportHandle -> IO ()

data RemoteRepo = RemoteRepo Git.Repo RemoteGitConfig
newtype LocalRepo = LocalRepo Git.Repo

-- All Transports share a single AnnexState MVar
--
-- Different TransportHandles may have different versions of the LocalRepo.
-- (For example, the ssh transport modifies it to enable ssh connection
-- caching.)
data TransportHandle = TransportHandle LocalRepo (MVar Annex.AnnexState)

-- Messages that the daemon emits.
data Emitted
	= CONNECTED RemoteURI
	| DISCONNECTED RemoteURI
	| SYNCING RemoteURI
	| DONESYNCING RemoteURI Bool
	| WARNING RemoteURI String
	deriving (Show)

-- Messages that the deamon consumes.
data Consumed
	= PAUSE
	| LOSTNET
	| RESUME
	| CHANGED ChangedRefs
	| RELOAD
	| STOP
	deriving (Show)

instance Proto.Sendable Emitted where
	formatMessage (CONNECTED remote) =
		["CONNECTED", Proto.serialize remote]
	formatMessage (DISCONNECTED remote) =
		["DISCONNECTED", Proto.serialize remote]
	formatMessage (SYNCING remote) =
		["SYNCING", Proto.serialize remote]
	formatMessage (DONESYNCING remote status) =
		["DONESYNCING", Proto.serialize remote, Proto.serialize status]
	formatMessage (WARNING remote message) =
		["WARNING", Proto.serialize remote, Proto.serialize message]

instance Proto.Sendable Consumed where
	formatMessage PAUSE = ["PAUSE"]
	formatMessage LOSTNET = ["LOSTNET"]
	formatMessage RESUME = ["RESUME"]
	formatMessage (CHANGED refs) =["CHANGED", Proto.serialize refs]
	formatMessage RELOAD = ["RELOAD"]
	formatMessage STOP = ["STOP"]

instance Proto.Receivable Emitted where
	parseCommand "CONNECTED" = Proto.parse1 CONNECTED
	parseCommand "DISCONNECTED" = Proto.parse1 DISCONNECTED
	parseCommand "SYNCING" = Proto.parse1 SYNCING
	parseCommand "DONESYNCING" = Proto.parse2 DONESYNCING
	parseCommand "WARNING" = Proto.parse2 WARNING
	parseCommand _ = Proto.parseFail

instance Proto.Receivable Consumed where
	parseCommand "PAUSE" = Proto.parse0 PAUSE
	parseCommand "LOSTNET" = Proto.parse0 LOSTNET
	parseCommand "RESUME" = Proto.parse0 RESUME
	parseCommand "CHANGED" = Proto.parse1 CHANGED
	parseCommand "RELOAD" = Proto.parse0 RELOAD
	parseCommand "STOP" = Proto.parse0 STOP
	parseCommand _ = Proto.parseFail

instance Proto.Serializable RemoteURI where
	serialize (RemoteURI u) = show u
	deserialize = RemoteURI <$$> parseURI

instance Proto.Serializable Bool where
	serialize False = "0"
	serialize True = "1"

	deserialize "0" = Just False
	deserialize "1" = Just True
	deserialize _ = Nothing
