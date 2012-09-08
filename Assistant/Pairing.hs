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

{- Messages sent in pairing are all verifiable using a secret that
 - should be shared between the systems being paired. -}
type PairMsg = Verifiable (PairStage, HostInfo, SshPubKey)

mkPairMsg :: Secret -> PairStage -> HostInfo -> SshPubKey -> PairMsg
mkPairMsg secret pairstage hostinfo sshkey = mkVerifiable
	(pairstage, hostinfo, sshkey) secret

data PairStage
	{- "I'd like to pair with somebody who knows a secret.
	 - Here's my ssh key, and hostinfo." -}
	= PairRequest
	{- "I've checked your PairRequest, and like it; I set up
	 - your ssh key already. Here's mine." -}
	| PairAck
	deriving (Eq, Read, Show)

data HostInfo = HostInfo
	{ hostName :: HostName
	, userName :: UserName
	}
	deriving (Eq, Read, Show)

type SshPubKey = String
type UserName = String
