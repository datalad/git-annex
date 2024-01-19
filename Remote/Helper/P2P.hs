{- Helpers for remotes using the git-annex P2P protocol.
 -
 - Copyright 2016-2021 Joey Hess <id@joeyh.name>
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
import Utility.Tuple
import Types.NumCopies
import Annex.Verify

import Control.Concurrent

-- Runs a Proto action using a connection it sets up.
type ProtoRunner a = P2P.Proto a -> Annex (Maybe a)

-- Runs a Proto action using a ClosableConnection.
type ProtoConnRunner c = forall a. P2P.Proto a -> ClosableConnection c -> Annex (ClosableConnection c, Maybe a)

-- Runs an Annex action with a connection from the pool, adding it back to
-- the pool when done.
type WithConn a c = (ClosableConnection c -> Annex (ClosableConnection c, a)) -> Annex a

store :: RemoteGitConfig -> ProtoRunner Bool -> Key -> AssociatedFile -> MeterUpdate -> Annex ()
store gc runner k af p = do
	let sizer = KeySizer k (fmap (toRawFilePath . fst3) <$> prepSendAnnex k)
	let bwlimit = remoteAnnexBwLimitUpload gc <|> remoteAnnexBwLimit gc
	metered (Just p) sizer bwlimit $ \_ p' ->
		runner (P2P.put k af p') >>= \case
			Just True -> return ()
			Just False -> giveup "Transfer failed"
			Nothing -> remoteUnavail

retrieve :: RemoteGitConfig -> (ProtoRunner (Bool, Verification)) -> Key -> AssociatedFile -> FilePath -> MeterUpdate -> VerifyConfig -> Annex Verification
retrieve gc runner k af dest p verifyconfig = do
	iv <- startVerifyKeyContentIncrementally verifyconfig k
	let bwlimit = remoteAnnexBwLimitDownload gc <|> remoteAnnexBwLimit gc
	metered (Just p) k bwlimit $ \m p' -> 
		runner (P2P.get dest k iv af m p') >>= \case
			Just (True, v) -> return v
			Just (False, _) -> giveup "Transfer failed"
			Nothing -> remoteUnavail

remove :: ProtoRunner Bool -> Key -> Annex ()
remove runner k = runner (P2P.remove k) >>= \case
	Just True -> return ()
	Just False -> giveup "removing content from remote failed"
	Nothing -> remoteUnavail

checkpresent :: ProtoRunner Bool -> Key -> Annex Bool
checkpresent runner k = maybe remoteUnavail return =<< runner (P2P.checkPresent k)

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

remoteUnavail :: a
remoteUnavail = giveup "can't connect to remote"
