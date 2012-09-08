{- git-annex assistant repo pairing
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.Pairing where

import Assistant.Common
import Utility.Verifiable

import Network.Socket (HostName)

{- "I'd like to pair with somebody who knows a secret." -}
data PairReq = PairReq (Verifiable PairData)
	deriving (Eq, Read, Show)

{- "I've checked your PairReq, and like it.
 - I set up your ssh key already. Here's mine for you to set up." -}
data PairAck = PairAck (Verifiable PairData)
	deriving (Eq, Read, Show)

data PairMsg
	= PairReqM PairReq
	| PairAckM PairAck
	deriving (Eq, Read, Show)

data PairData = PairData
	{ hostName :: HostName
	, userName :: UserName
	, sshPubKey :: Maybe SshPubKey
	}
	deriving (Eq, Read, Show)

type SshPubKey = String
type UserName = String
