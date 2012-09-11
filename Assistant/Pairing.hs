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

newtype PairMsg = PairMsg (Verifiable (PairStage, PairData, SomeAddr))
	deriving (Eq, Read, Show)

fromPairMsg :: PairMsg -> (Verifiable (PairStage, PairData, SomeAddr))
fromPairMsg (PairMsg m) = m

pairMsgStage :: PairMsg -> PairStage
pairMsgStage (PairMsg (Verifiable (s, _, _) _)) = s

pairMsgData :: PairMsg -> PairData
pairMsgData (PairMsg (Verifiable (_, d, _) _)) = d

pairMsgAddr :: PairMsg -> SomeAddr
pairMsgAddr (PairMsg (Verifiable (_, _, a) _)) = a

data PairData = PairData
	-- uname -n output, not a full domain name
	{ remoteHostName :: Maybe HostName
	, remoteUserName :: UserName
	, remoteDirectory :: FilePath
	, remoteSshPubKey :: SshPubKey
	}
	deriving (Eq, Read, Show)

type UserName = String

{- A pairing that is in progress has a secret, a thread that is
 - broadcasting pairing messages, and a SshKeyPair that has not yet been
 - set up on disk. -}
data PairingInProgress = PairingInProgress
	{ inProgressSecret :: Secret
	, inProgressThreadId :: Maybe ThreadId
	, inProgressSshKeyPair :: SshKeyPair
	, inProgressPairData :: PairData
	}

data SomeAddr = IPv4Addr HostAddress | IPv6Addr HostAddress6
	deriving (Ord, Eq, Read, Show)
