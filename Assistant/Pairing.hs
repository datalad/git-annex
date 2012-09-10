{- git-annex assistant repo pairing, core data types
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.Pairing where

import Utility.Verifiable
import Assistant.Ssh

import Control.Concurrent
import Network.Socket

data PairStage
	{- "I'll pair with anybody who shares the secret that can be used
	 - to verify this request." -}
	 = PairReq
	{- "I've verified your request, and you can verify this to see
	 - that I know the secret. I set up your ssh key already.
	 - Here's mine for you to set up." -}
	| PairAck
	{- "I saw your PairAck; you can stop sending them." -}
	| PairDone
	deriving (Eq, Read, Show)

newtype PairMsg = PairMsg (Verifiable (PairStage, PairData))
	deriving (Eq, Read, Show)

fromPairMsg :: PairMsg -> (Verifiable (PairStage, PairData))
fromPairMsg (PairMsg m) = m

pairMsgStage :: PairMsg -> PairStage
pairMsgStage (PairMsg (Verifiable (s, _) _)) = s

data PairData = PairData
	-- uname -n output, not a full domain name
	{ remoteHostName :: Maybe HostName
	-- the address is included so that it can be verified, avoiding spoofing
	, remoteAddress :: SomeAddr
	, remoteUserName :: UserName
	, remoteDirectory :: FilePath
	, remoteSshPubKey :: SshPubKey
	}
	deriving (Eq, Read, Show)

type SshPubKey = String
type UserName = String

{- A pairing that is in progress has a secret, and a thread that is
 - broadcasting pairing requests. -}
data PairingInProgress = PairingInProgress
	{ inProgressSecret :: Secret
	, inProgressThreadId :: ThreadId
	, inProgressSshKeyPair :: SshKeyPair
	}

data SomeAddr = IPv4Addr HostAddress | IPv6Addr HostAddress6
	deriving (Ord, Eq, Read, Show)
