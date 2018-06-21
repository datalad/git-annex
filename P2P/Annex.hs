{- P2P protocol, Annex implementation
 -
 - Copyright 2016-2018 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE RankNTypes, FlexibleContexts #-}

module P2P.Annex
	( RunState(..)
	, mkRunState
	, P2PConnection(..)
	, runFullProto
	) where

import Annex.Common
import Annex.Content
import Annex.Transfer
import Annex.ChangedRefs
import P2P.Protocol
import P2P.IO
import Logs.Location
import Types.NumCopies
import Types.Remote (RetrievalSecurityPolicy(..))
import Utility.Metered

import Control.Monad.Free

-- Full interpreter for Proto, that can receive and send objects.
runFullProto :: RunState -> P2PConnection -> Proto a -> Annex (Either String a)
runFullProto runst conn = go
  where
	go :: RunProto Annex
	go (Pure v) = return (Right v)
	go (Free (Net n)) = runNet runst conn go n
	go (Free (Local l)) = runLocal runst go l

runLocal :: RunState -> RunProto Annex -> LocalF (Proto a) -> Annex (Either String a)
runLocal runst runner a = case a of
	TmpContentSize k next -> do
		tmp <- fromRepo $ gitAnnexTmpObjectLocation k
		size <- liftIO $ catchDefaultIO 0 $ getFileSize tmp
		runner (next (Len size))
	FileSize f next -> do
		size <- liftIO $ catchDefaultIO 0 $ getFileSize f
		runner (next (Len size))
	ContentSize k next -> do
		let getsize = liftIO . catchMaybeIO . getFileSize
		size <- inAnnex' isJust Nothing getsize k
		runner (next (Len <$> size))
	ReadContent k af o sender next -> do
		v <- tryNonAsync $ prepSendAnnex k
		case v of
			Right (Just (f, checkchanged)) -> do
				v' <- tryNonAsync $
					transfer upload k af $
						sinkfile f o checkchanged sender
				case v' of
					Left e -> return (Left (show e))
					Right (Left e) -> return (Left (show e))
					Right (Right ok) -> runner (next ok)
			-- content not available
 			Right Nothing -> runner (next False)
			Left e -> return (Left (show e))
	StoreContent k af o l getb validitycheck next -> do
		-- This is the same as the retrievalSecurityPolicy of
		-- Remote.P2P and Remote.Git. 
		let rsp = RetrievalAllKeysSecure
		ok <- flip catchNonAsync (const $ return False) $
			transfer download k af $ \p ->
				getViaTmp rsp DefaultVerify k $ \tmp -> do
					storefile tmp o l getb validitycheck p
		runner (next ok)
	StoreContentTo dest o l getb validitycheck next -> do
		res <- flip catchNonAsync (const $ return (False, UnVerified)) $
			storefile dest o l getb validitycheck nullMeterUpdate
		runner (next res)
	SetPresent k u next -> do
		v <- tryNonAsync $ logChange k u InfoPresent
		case v of
			Left e -> return (Left (show e))
			Right () -> runner next
	CheckContentPresent k next -> do
		v <- tryNonAsync $ inAnnex k
		case v of
			Left e -> return (Left (show e))
			Right result -> runner (next result)
	RemoveContent k next -> do
		v <- tryNonAsync $
			ifM (Annex.Content.inAnnex k)
				( lockContentForRemoval k $ \contentlock -> do
					removeAnnex contentlock
					logStatus k InfoMissing
					return True
				, return True
				)
		case v of
			Left e -> return (Left (show e))
			Right result -> runner (next result)
	TryLockContent k protoaction next -> do
		v <- tryNonAsync $ lockContentShared k $ \verifiedcopy -> 
			case verifiedcopy of
				LockedCopy _ -> runner (protoaction True)
				_ -> runner (protoaction False)
		-- If locking fails, lockContentShared throws an exception.
		-- Let the peer know it failed.
		case v of
			Left _ -> runner $ do
				protoaction False
				next
			Right _ -> runner next
	WaitRefChange next -> case runst of
		Serving _ (Just h) _ -> do
			v <- tryNonAsync $ liftIO $ waitChangedRefs h
			case v of
				Left e -> return (Left (show e))
				Right changedrefs -> runner (next changedrefs)
		_ -> return $ Left "change notification not available"
	UpdateMeterTotalSize m sz next -> do
		liftIO $ setMeterTotalSize m sz
		runner next
	RunValidityCheck check next -> runner . next =<< check
  where
	transfer mk k af ta = case runst of
		-- Update transfer logs when serving.
		-- Using noRetry because we're the sender.
		Serving theiruuid _ _ -> 
			mk theiruuid k af noRetry ta noNotification
		-- Transfer logs are updated higher in the stack when
		-- a client.
		Client _ -> ta nullMeterUpdate
	
	storefile dest (Offset o) (Len l) getb validitycheck p = do
		let p' = offsetMeterUpdate p (toBytesProcessed o)
		v <- runner getb
		case v of
			Right b -> do
				liftIO $ withBinaryFile dest ReadWriteMode $ \h -> do
					when (o /= 0) $
						hSeek h AbsoluteSeek o
					meteredWrite p' h b
				rightsize <- do
					sz <- liftIO $ getFileSize dest
					return (toInteger sz == l + o)
					
				runner validitycheck >>= \case
					Right (Just Valid) ->
						return (rightsize, UnVerified)
					_ -> do
						-- Invalid, or old protocol
						-- version. Validity is not
						-- known. Force content
						-- verification.
						return (rightsize, MustVerify)
			Left e -> error e
	
	sinkfile f (Offset o) checkchanged sender p = bracket setup cleanup go
	  where
		setup = liftIO $ openBinaryFile f ReadMode
		cleanup = liftIO . hClose
		go h = do
			let p' = offsetMeterUpdate p (toBytesProcessed o)
			when (o /= 0) $
				liftIO $ hSeek h AbsoluteSeek o
			b <- liftIO $ hGetContentsMetered h p'
			let validitycheck = local $ runValidityCheck $ 
				checkchanged >>= return . \case
					False -> Invalid
					True -> Valid
			runner (sender b validitycheck)
