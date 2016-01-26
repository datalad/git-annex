{- git-annex assistant repo pairing, core data types
 -
 - Copyright 2012 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Assistant.Pairing where

import Annex.Common
import Utility.Verifiable
import Assistant.Ssh

import Control.Concurrent
import Network.Socket
import Data.Char
import qualified Data.Text as T

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
	deriving (Eq, Read, Show, Ord, Enum)

newtype PairMsg = PairMsg (Verifiable (PairStage, PairData, SomeAddr))
	deriving (Eq, Read, Show)

verifiedPairMsg :: PairMsg -> PairingInProgress -> Bool
verifiedPairMsg (PairMsg m) pip = verify m $ inProgressSecret pip

fromPairMsg :: PairMsg -> Verifiable (PairStage, PairData, SomeAddr)
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
	, pairUUID :: UUID
	}
	deriving (Eq, Read, Show)

checkSane :: PairData -> Bool
checkSane p = all (not . any isControl)
	[ fromMaybe "" (remoteHostName p)
	, remoteUserName p
	, remoteDirectory p
	, remoteSshPubKey p
	, fromUUID (pairUUID p)
	]

type UserName = String

{- A pairing that is in progress has a secret, a thread that is
 - broadcasting pairing messages, and a SshKeyPair that has not yet been
 - set up on disk. -}
data PairingInProgress = PairingInProgress
	{ inProgressSecret :: Secret
	, inProgressThreadId :: Maybe ThreadId
	, inProgressSshKeyPair :: SshKeyPair
	, inProgressPairData :: PairData
	, inProgressPairStage :: PairStage
	}
	deriving (Show)

data AddrClass = IPv4AddrClass | IPv6AddrClass

data SomeAddr = IPv4Addr HostAddress
{- My Android build of the Network library does not currently have IPV6
 - support. -}
#ifndef __ANDROID__
	| IPv6Addr HostAddress6
#endif
	deriving (Ord, Eq, Read, Show)

{- This contains the whole secret, just lightly obfuscated to make it not
 - too obvious. It's only displayed in the user's web browser. -}
newtype SecretReminder = SecretReminder [Int]
	deriving (Show, Eq, Ord, Read)

toSecretReminder :: T.Text -> SecretReminder
toSecretReminder = SecretReminder . map ord . T.unpack

fromSecretReminder :: SecretReminder -> T.Text
fromSecretReminder (SecretReminder s) = T.pack $ map chr s
