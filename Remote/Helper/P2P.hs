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
import Utility.HumanTime

import Control.Concurrent
import Data.Time.Clock.POSIX

-- Runs a Proto action using a connection it sets up.
type ProtoRunner a = P2P.Proto a -> Annex (Maybe a)

-- Runs a Proto action using a ClosableConnection.
type ProtoConnRunner c = forall a. P2P.Proto a -> ClosableConnection c -> Annex (ClosableConnection c, Maybe a)

-- Runs an Annex action with a connection from the pool, adding it back to
-- the pool when done.
type WithConn a c = (ClosableConnection c -> Annex (ClosableConnection c, a)) -> Annex a

store :: UUID -> RemoteGitConfig -> ProtoRunner (Maybe [UUID]) -> Key -> AssociatedFile -> Maybe FilePath -> MeterUpdate -> Annex ()
store remoteuuid gc runner k af o p = do
	let sizer = KeySizer k (fmap (toRawFilePath . fst3) <$> prepSendAnnex k o)
	let bwlimit = remoteAnnexBwLimitUpload gc <|> remoteAnnexBwLimit gc
	metered (Just p) sizer bwlimit $ \_ p' ->
		runner (P2P.put k af p') >>= \case
			Just (Just fanoutuuids) -> 
				storeFanout NoLiveUpdate k InfoPresent remoteuuid fanoutuuids
			Just Nothing -> giveup "Transfer failed"
			Nothing -> remoteUnavail

storeFanout :: LiveUpdate -> Key -> LogStatus -> UUID -> [UUID] -> Annex ()
storeFanout lu k logstatus remoteuuid us = 
	forM_ us $ \u ->
		when (u /= remoteuuid) $
			logChange lu k u logstatus

retrieve :: RemoteGitConfig -> (ProtoRunner (Bool, Verification)) -> Key -> AssociatedFile -> FilePath -> MeterUpdate -> VerifyConfig -> Annex Verification
retrieve gc runner k af dest p verifyconfig = do
	iv <- startVerifyKeyContentIncrementally verifyconfig k
	let bwlimit = remoteAnnexBwLimitDownload gc <|> remoteAnnexBwLimit gc
	metered (Just p) k bwlimit $ \m p' -> 
		runner (P2P.get dest k iv af m p') >>= \case
			Just (True, v) -> return v
			Just (False, _) -> giveup "Transfer failed"
			Nothing -> remoteUnavail

remove :: UUID -> ProtoRunner (Either String Bool, Maybe [UUID]) -> Maybe SafeDropProof -> Key -> Annex ()
remove remoteuuid runner proof k = runner (P2P.remove proof k) >>= \case
	Just (Right True, alsoremoveduuids) -> 
		storeFanout NoLiveUpdate k InfoMissing remoteuuid
			(fromMaybe [] alsoremoveduuids)
	Just (Right False, alsoremoveduuids) -> do
		storeFanout NoLiveUpdate k InfoMissing remoteuuid
			(fromMaybe [] alsoremoveduuids)
		giveup "removing content from remote failed"
	Just (Left err, _) -> do
		giveup (safeOutput err)
	Nothing -> remoteUnavail

checkpresent :: ProtoRunner (Either String Bool) -> Key -> Annex Bool
checkpresent runner k =
	runner (P2P.checkPresent k)
		>>= \case
			Nothing -> remoteUnavail
			Just (Right b) -> return b
			Just (Left err) -> giveup (safeOutput err)

{- Locks the content on the remote while running an action with a
 - LockedCopy.
 -
 - Note that this only guarantees that the content is locked as long as the
 - connection to the peer remains up. If the connection is unexpectededly
 - dropped, the peer will then unlock the content.
 -}
lock :: WithConn a c -> ProtoConnRunner c -> UUID -> Key -> (VerifiedCopy -> Annex a) -> Annex a
lock withconn connrunner u k callback = withconn $ \conn -> do
	starttime <- liftIO getPOSIXTime
	connv <- liftIO $ newMVar conn
	let runproto d p = do
		c <- liftIO $ takeMVar connv
		(c', mr) <- connrunner p c
		liftIO $ putMVar connv c'
		return (fromMaybe d mr)
	r <- P2P.lockContentWhile runproto k (go starttime)
	conn' <- liftIO $ takeMVar connv
	return (conn', r)
  where
	go _ False = giveup "can't lock content"
	go starttime True = do
		let check = return $ Left $ starttime + retentionduration
		withVerifiedCopy LockedCopy u check callback
	retentionduration = fromIntegral $
		durationSeconds p2pDefaultLockContentRetentionDuration

remoteUnavail :: a
remoteUnavail = giveup "can't connect to remote"
