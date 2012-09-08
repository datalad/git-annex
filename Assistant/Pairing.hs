{- git-annex assistant repo pairing
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Assistant.Pairing where

import Assistant.Common

import Network.Socket (HostName)

type SshPubKey = String
type HMACDigest = String
type UserName = String
type Secret = String

data HostInfo = HostInfo
	{ hostName :: HostName
	, userName :: UserName
	}
	deriving (Eq, Read, Show)

data PairStage
	{- "I'd like to pair with somebody who knows a secret.
	 - Here's my ssh key, and hostinfo, both verifiable with
	 - our shared secret." -}
	= PairRequest
	{- "I've checked your PairRequest, and like it; I set up
	 - your ssh key already. Here's mine, also verified, please set it
	 - up too, and start syncing!" -}
	| PairAck
	deriving (Eq, Read, Show)

type PairMsg = Verifiable (PairStage, HostInfo, SshPubKey)

mkPairMsg :: Secret -> PairStage -> HostInfo -> SshPubKey -> PairMsg
mkPairMsg secret pairstage hostinfo sshkey = mkVerifiable
	(pairstage, hostinfo, sshkey) secret

{- A value, verifiable using a HMAC digest to encrypt using a shared secret. -}
data Verifiable a = Verifiable
	{ val :: a
	, digest :: HMACDigest
	}
	deriving (Eq, Read, Show)

mkVerifiable :: Show a => a -> Secret -> Verifiable a
mkVerifiable a secret = Verifiable a (calcDigest (show a) secret)

verified :: (Eq a, Show a) => Verifiable a -> Secret -> Bool
verified v secret = v == mkVerifiable (val v) secret

calcDigest :: String -> Secret -> HMACDigest
calcDigest = undefined -- TODO
