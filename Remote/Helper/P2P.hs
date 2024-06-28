{- Helpers for remotes using the git-annex P2P protocol.
 -
 - Copyright 2016-2024 Joey Hess <id@joeyh.name>
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
import Logs.Location
import Utility.SafeOutput

import Control.Concurrent

-- Runs a Proto action using a connection it sets up.
type ProtoRunner a = P2P.Proto a -> Annex (Maybe a)

-- Runs a Proto action using a ClosableConnection.
type ProtoConnRunner c = forall a. P2P.Proto a -> ClosableConnection c -> Annex (ClosableConnection c, Maybe a)

-- Runs an Annex action with a connection from the pool, adding it back to
-- the pool when done.
type WithConn a c = (ClosableConnection c -> Annex (ClosableConnection c, a)) -> Annex a

store :: UUID -> RemoteGitConfig -> ProtoRunner (Maybe [UUID]) -> Key -> AssociatedFile -> MeterUpdate -> Annex ()
store remoteuuid gc runner k af p = do
	let sizer = KeySizer k (fmap (toRawFilePath . fst3) <$> prepSendAnnex k)
	let bwlimit = remoteAnnexBwLimitUpload gc <|> remoteAnnexBwLimit gc
	metered (Just p) sizer bwlimit $ \_ p' ->
		runner (P2P.put k af p') >>= \case
			Just (Just fanoutuuids) -> do
				-- Storing on the remote can cause it
				-- to be stored on additional UUIDs, 
				-- so record those.
				forM_ fanoutuuids $ \u ->
					when (u /= remoteuuid) $
						logChange k u InfoPresent
			Just Nothing -> giveup "Transfer failed"
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

remove :: UUID -> ProtoRunner (Bool, Maybe [UUID]) -> Key -> Annex ()
remove remoteuuid runner k = runner (P2P.remove k) >>= \case
	Just (True, alsoremoveduuids) -> note alsoremoveduuids
	Just (False, alsoremoveduuids) -> do
		note alsoremoveduuids
		giveup "removing content from remote failed"
	Nothing -> remoteUnavail
  where
	-- The remote reports removal from other UUIDs than its own,
	-- so record those.
	note alsoremoveduuids = 
		forM_ (fromMaybe [] alsoremoveduuids) $ \u ->
			when (u /= remoteuuid) $
				logChange k u InfoMissing

checkpresent :: ProtoRunner (Either String Bool) -> Key -> Annex Bool
checkpresent runner k =
	runner (P2P.checkPresent k)
		>>= \case
			Nothing -> remoteUnavail
			Just (Right b) -> return b
			Just (Left err) -> giveup (safeOutput err)

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
