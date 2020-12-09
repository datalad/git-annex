{- protocol used by "git-annex transferrer"
 -
 - Copyright 2020 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Types.Transferrer where

import Annex.Common
import Types.Transfer
import Types.Messages
import Git.Types (RemoteName)

data TransferRequest = TransferRequest TransferRequestLevel Direction (Either UUID RemoteName) KeyData AssociatedFile
	deriving (Show, Read)

data TransferRequestLevel = AnnexLevel | AssistantLevel
	deriving (Show, Read)

data TransferResponse
	= TransferOutput SerializedOutput
	| TransferResult Bool
	deriving (Show, Read)
