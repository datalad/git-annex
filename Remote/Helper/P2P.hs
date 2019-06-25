{- Helpers for remotes using the git-annex P2P protocol.
 -
 - Copyright 2016-2018 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE RankNTypes #-}

module Remote.Helper.P2P where

import Annex.Common
import qualified P2P.Protocol as P2P
import P2P.IO
import Types.Remote
import Annex.Content
import Messages.Progress
import Utility.Metered
import Types.NumCopies

import Control.Concurrent

-- Runs a Proto action using a connection it sets up.
type ProtoRunner a = P2P.Proto a -> Annex (Maybe a)

-- Runs a Proto action using a ClosableConnection.
type ProtoConnRunner c = forall a. P2P.Proto a -> ClosableConnection c -> Annex (ClosableConnection c, Maybe a)

-- Runs an Annex action with a connection from the pool, adding it back to
-- the pool when done.
type WithConn a c = (ClosableConnection c -> Annex (ClosableConnection c, a)) -> Annex a

store :: (MeterUpdate -> ProtoRunner Bool) -> Key -> AssociatedFile -> MeterUpdate -> Annex Bool
store runner k af p = do
	let sizer = KeySizer k (fmap fst <$> prepSendAnnex k)
	metered (Just p) sizer $ \_ p' -> 
		fromMaybe False
			<$> runner p' (P2P.put k af p')

retrieve :: (MeterUpdate -> ProtoRunner (Bool, Verification)) -> Key -> AssociatedFile -> FilePath -> MeterUpdate -> Annex (Bool, Verification)
retrieve runner k af dest p =
	metered (Just p) k $ \m p' -> 
		fromMaybe (False, UnVerified)
			<$> runner p' (P2P.get dest k af m p')

remove :: ProtoRunner Bool -> Key -> Annex Bool
remove runner k = fromMaybe False <$> runner (P2P.remove k)

checkpresent :: ProtoRunner Bool -> Key -> Annex Bool
checkpresent runner k = maybe unavail return =<< runner (P2P.checkPresent k)
  where
	unavail = giveup "can't connect to remote"

lock :: WithConn a c -> ProtoConnRunner c -> UUID -> Key -> (VerifiedCopy -> Annex a) -> Annex a
lock withconn connrunner u k callback = withconn $ \conn -> do
	connv <- liftIO $ newMVar conn
	let runproto d p = do
		c <- liftIO $ takeMVar connv
		(c', mr) <- connrunner p c
		liftIO $ putMVar connv c'
		return (fromMaybe d mr)
	r <- P2P.lockContentWhile runproto k go
	conn' <- liftIO $ takeMVar connv
	return (conn', r)
  where
	go False = giveup "can't lock content"
	go True = withVerifiedCopy LockedCopy u (return True) callback
