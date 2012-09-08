{- git-annex assistant repo pairing, core data types
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.Pairing where

import Utility.Verifiable

import Control.Concurrent
import Network.Socket

{- "I'll pair with anybody who shares the secret that can be used to verify
 - this request." -}
data PairReq = PairReq (Verifiable PairData)
	deriving (Eq, Read, Show)

{- "I've verified your request, and you can verify mine to see that I know
 - the secret. I set up your ssh key already. Here's mine for you to set up." -}
data PairAck = PairAck (Verifiable PairData)
	deriving (Eq, Read, Show)

fromPairReq :: PairReq -> Verifiable PairData
fromPairReq (PairReq v) = v

fromPairAck :: PairAck -> Verifiable PairData
fromPairAck (PairAck v) = v

data PairMsg
	= PairReqM PairReq
	| PairAckM PairAck
	deriving (Eq, Read, Show)

data PairData = PairData
	-- uname -n output, not a full domain name
	{ remoteHostName :: Maybe HostName
	, remoteAddress :: SomeAddr
	, remoteUserName :: UserName
	, sshPubKey :: SshPubKey
	}
	deriving (Eq, Read, Show)

type SshPubKey = String
type UserName = String

{- A pairing that is in progress has a secret, and a thread that is
 - broadcasting pairing requests. -}
data PairingInProgress = PairingInProgress Secret ThreadId

data SomeAddr = IPv4Addr HostAddress | IPv6Addr HostAddress6
	deriving (Ord, Eq, Read, Show)
