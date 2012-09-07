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

{- "I'd like to pair with somebody. My name is requestingHost
 - and my user is requestingUser" -}
data RequestPair = RequestPair
	{ requestingHost :: HostName
	, requestingUser :: UserName
	}
	deriving (Eq, Read, Show)

{- "I'll pair with you! My name is respondingHost
 - and my user is respondingUser" -}
data StartPair = StartPair
	{ respondingHost :: HostName
	, respondingUser :: UserName
 	, requestPair :: RequestPair
	}
	deriving (Eq, Read, Show)

{- Sent to authenticate a pair request.
 - The digest is of startPair + sshPubKey, using a shared secret. -}
data AuthPair = AuthPair
	{ sshPubKey :: SshPubKey
	, digest :: HMACDigest
	, startPair :: StartPair
	}
	deriving (Eq, Read, Show)

{- Acknowledges authentication of a pair request, and indicates that one side
 - of the pairing is done. -}
data AckPair = AckPair { ackAuthPair :: AuthPair }
	deriving (Eq, Read, Show)
-- ... Or authentication failed.
data NackPair = NackPair { nackAuthPair :: AuthPair }
	deriving (Eq, Read, Show)

data PairMsg
	= RequestPairM RequestPair
	| StartPairM StartPair
	| AuthPairM AuthPair
	| AckPairM AckPair
	| NackPairM NackPair
	deriving (Eq, Read, Show)

{- All the information needed to hold a conversation. -}
data PairInfo = PairInfo
	{ myHostName :: HostName
	, myUserName :: UserName
	, mySshPubKey :: SshPubKey
	, mySecret :: Secret
	}

{- Given a message from the other side, returns any response. -}
response :: PairInfo -> PairMsg -> Maybe PairMsg
response i (RequestPairM v) = Just $ StartPairM $ StartPair
	{ respondingHost = myHostName i
	, respondingUser = myUserName i
	, requestPair = v
	}
response i (StartPairM v) = Just $ AuthPairM $ AuthPair
	{ sshPubKey = mySshPubKey i
	, digest = calcDigest v i
	, startPair = v
	}
response i (AuthPairM v)
	| goodAuth v (mySecret i) = Just $ AckPairM $ AckPair { ackAuthPair = v }
	| otherwise = Just $ NackPairM $ NackPair { nackAuthPair = v }
response i (AckPairM v) = Nothing
response i (NackPairM v) = Nothing

calcDigest :: StartPair -> PairInfo -> HMACDigest
calcDigest = undefined -- TODO

goodAuth :: AuthPair -> Secret -> Bool
goodAuth = undefined

{- State machine to handle pairing.
 - 
 - The send action is responsible for repeating the message as necessary
 - until its receipt is acked.
 - 
 - The receive action should block until a message is received, and ack
 - its receipt. It may time out, and return Nothing.
 -
 - Returns our AckPairM/NAckPairM, and the remote's AckPairM/NAckPairM
 -}
runPair :: Monad m
	=> PairInfo
	-> (PairMsg -> m ())
	-> (m (Maybe PairMsg))
	-> m (Maybe PairMsg, Maybe PairMsg)
runPair i send receive = do
	send initialrequest
	go Nothing Nothing
	where
		initialrequest = RequestPairM $ RequestPair
			{ requestingHost = myHostName i
			, requestingUser = myUserName i
			}
		go local_ack@(Just _) remote_ack@(Just _) =
			return (local_ack, remote_ack)
		go local_ack remote_ack = do
			mr <- receive
			case mr of
				Nothing -> return (local_ack, remote_ack)
				Just r -> case response i r of
					Just resp@(AckPairM _) -> do
						send resp
						go (Just resp) remote_ack
					Just resp@(NackPairM _) -> do
						send resp
						go (Just resp) remote_ack
					Just resp -> do
						send resp
						go local_ack remote_ack
					Nothing -> go local_ack (Just r)

{- A sample conversation between two hosts, Left and Right.
 -
 - The order of some messages can vary, as there are really two independant
 - threads of conversation here, one started by leftreq and the other by
 - rightreq. -}
sample :: [Either PairMsg PairMsg]
sample =
	[ Left $ RequestPairM $ leftreq
	, Right $ RequestPairM $ rightreq
	, Right $ StartPairM $ StartPair "foo" "bar" leftreq
	, Left $ StartPairM $ StartPair "gnu" "joey" rightreq
	, Left $ AuthPairM $ AuthPair "ssh-key-left" "digestleft" $
		StartPair "foo" "bar" leftreq
	, Right $ AuthPairM $ AuthPair "ssh-key-right" "digestright" $
		StartPair "gnu" "joey" rightreq
	, Right $ AckPairM $ AckPair $
		AuthPair "ssh-key-left" "digestleft" $
 	               StartPair "foo" "bar" leftreq
	, Left $ AckPairM $ AckPair $
		AuthPair "ssh-key-right" "digestright" $
        	        StartPair "gnu" "joey" rightreq
	]
	where
		leftreq = RequestPair "gnu" "joey"
		rightreq = RequestPair "foo" "bar"
