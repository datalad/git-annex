{- git-annex assistant pairing
 -
 - Copyright 2016 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Assistant.WebApp.Pairing where

import Assistant.Common
import qualified Utility.MagicWormhole as Wormhole
import Command.P2P (wormholePairing, PairingResult(..))
import P2P.Address
import Annex.Concurrent
import Git.Types

import Control.Concurrent
import Control.Concurrent.Async
import Control.Concurrent.STM
import qualified Data.Map as M

data PairingWith = PairingWithSelf | PairingWithFriend
	deriving (Eq, Show, Read)

type WormholePairingState = TVar (M.Map WormholePairingId WormholePairingHandle)

type WormholePairingHandle = (PairingWith, RemoteName, MVar Wormhole.CodeObserver, MVar Wormhole.Code, Async (Annex PairingResult))

newtype WormholePairingId = WormholePairingId Int
	deriving (Ord, Eq, Show, Read)

newWormholePairingState :: IO WormholePairingState
newWormholePairingState = newTVarIO M.empty

addWormholePairingState :: WormholePairingHandle -> WormholePairingState -> IO WormholePairingId
addWormholePairingState h tv = atomically $ do
	m <- readTVar tv
	-- use of head is safe because allids is infinite
	let i = Prelude.head $ filter (`notElem` M.keys m) allids
	writeTVar tv (M.insertWith' const i h m)
	return i
  where
	allids = map WormholePairingId [1..]

-- | Starts the wormhole pairing processes.
startWormholePairing :: PairingWith -> RemoteName -> [P2PAddress] -> Assistant WormholePairingHandle
startWormholePairing pairingwith remotename ouraddrs = do
	observerrelay <- liftIO newEmptyMVar
	producerrelay <- liftIO newEmptyMVar
	-- wormholePairing needs to run in the Annex monad, and is a
	-- long-duration action. So, don't just liftAnnex to run it;
	-- fork the Annex state.
	runner <- liftAnnex $ forkState $ 
		wormholePairing remotename ouraddrs $ \observer producer -> do
			putMVar observerrelay observer
			theircode <- takeMVar producerrelay
			Wormhole.sendCode producer theircode
	tid <- liftIO $ async runner
	return (pairingwith, remotename, observerrelay, producerrelay, tid)

-- | Call after sendTheirWormholeCode. This can take some time to return.
finishWormholePairing :: WormholePairingHandle -> Assistant PairingResult
finishWormholePairing (_, _, _, _, tid) = liftAnnex =<< liftIO (wait tid)

-- | Waits for wormhole to produce our code. Can be called repeatedly, safely.
getOurWormholeCode :: WormholePairingHandle -> IO Wormhole.Code
getOurWormholeCode (_, _, observerrelay, _, _) =
	readMVar observerrelay >>= Wormhole.waitCode

-- | Sends their code to wormhole. If their code has already been sent,
-- avoids blocking and returns False.
sendTheirWormholeCode :: WormholePairingHandle -> Wormhole.Code -> IO Bool
sendTheirWormholeCode (_, _, _, producerrelay, _) = tryPutMVar producerrelay

withPairingWith :: WormholePairingHandle -> (PairingWith -> a) -> a
withPairingWith (pairingwith, _, _, _, _) a = a pairingwith

withRemoteName :: WormholePairingHandle -> (RemoteName -> a) -> a
withRemoteName (_, remotename, _, _, _) a = a remotename
