{- git-remote-daemon data types.
 -
 - Copyright 2014 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module RemoteDaemon.Types where

import Common.Annex
import qualified Git.Types as Git
import qualified Utility.SimpleProtocol as Proto

-- Messages that the daemon emits.
data Emitted
	= CHANGED RemoteName RefList
	| STATUS RemoteName UserMessage
	| ERROR RemoteName UserMessage

-- Messages that the deamon consumes.
data Consumed
	= PAUSE
	| RESUME
	| PUSH RemoteName
	| RELOAD

type RemoteName = String
type UserMessage = String
type RefList = [Git.Ref]

instance Proto.Sendable Emitted where
	formatMessage (CHANGED remote refs) =
		["CHANGED"
		, Proto.serialize remote
		, Proto.serialize refs
		]
	formatMessage (STATUS remote msg) =
		["STATUS"
		, Proto.serialize remote
		, Proto.serialize msg
		]
	formatMessage (ERROR remote msg) =
		["ERROR"
		, Proto.serialize remote
		, Proto.serialize msg
		]

instance Proto.Sendable Consumed where
	formatMessage PAUSE = ["PAUSE"]
	formatMessage RESUME = ["RESUME"]
	formatMessage (PUSH remote) = ["PUSH", Proto.serialize remote]
	formatMessage RELOAD = ["RELOAD"]

instance Proto.Receivable Emitted where
	parseCommand "CHANGED" = Proto.parse2 CHANGED
	parseCommand "STATUS" = Proto.parse2 STATUS
	parseCommand "ERROR" = Proto.parse2 ERROR
	parseCommand _ = Proto.parseFail

instance Proto.Receivable Consumed where
	parseCommand "PAUSE" = Proto.parse0 PAUSE
	parseCommand "RESUME" = Proto.parse0 RESUME
	parseCommand "PUSH" = Proto.parse1 PUSH
	parseCommand "RELOAD" = Proto.parse0 RELOAD
	parseCommand _ = Proto.parseFail

instance Proto.Serializable [Char] where
	serialize = id
	deserialize = Just

instance Proto.Serializable RefList where
	serialize = unwords . map Git.fromRef
	deserialize = Just . map Git.Ref . words
