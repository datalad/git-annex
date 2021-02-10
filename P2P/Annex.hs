{- P2P protocol, Annex implementation
 -
 - Copyright 2016-2021 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
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
import Utility.Metered
import Types.Backend (IncrementalVerifier(..))
import Backend

import Control.Monad.Free
import Control.Concurrent.STM
import qualified Data.ByteString as S

-- Full interpreter for Proto, that can receive and send objects.
runFullProto :: RunState -> P2PConnection -> Proto a -> Annex (Either ProtoFailure a)
runFullProto runst conn = go
  where
	go :: RunProto Annex
	go (Pure v) = return (Right v)
	go (Free (Net n)) = runNet runst conn go n
	go (Free (Local l)) = runLocal runst go l

runLocal :: RunState -> RunProto Annex -> LocalF (Proto a) -> Annex (Either ProtoFailure a)
runLocal runst runner a = case a of
	TmpContentSize k next -> do
		tmp <- fromRepo $ gitAnnexTmpObjectLocation k
		size <- liftIO $ catchDefaultIO 0 $ getFileSize tmp
		runner (next (Len size))
	FileSize f next -> do
		size <- liftIO $ catchDefaultIO 0 $ getFileSize (toRawFilePath f)
		runner (next (Len size))
	ContentSize k next -> do
		let getsize = liftIO . catchMaybeIO . getFileSize
		size <- inAnnex' isJust Nothing getsize k
		runner (next (Len <$> size))
	ReadContent k af o sender next -> do
		let proceed c = do
			r <- tryNonAsync c
			case r of
				Left e -> return $ Left $ ProtoFailureException e
				Right (Left e) -> return $ Left e
				Right (Right ok) -> runner (next ok)
		-- If the content is not present, or the transfer doesn't
		-- run for any other reason, the sender action still must
		-- be run, so is given empty and Invalid data.
		let fallback = runner (sender mempty (return Invalid))
		v <- tryNonAsync $ prepSendAnnex k
		case v of
			Right (Just (f, checkchanged)) -> proceed $ do
				-- alwaysUpload to allow multiple uploads of the same key.
				let runtransfer ti = transfer alwaysUpload k af Nothing $ \p ->
					sinkfile f o checkchanged sender p ti
				checktransfer runtransfer fallback
 			Right Nothing -> proceed fallback
			Left e -> return $ Left $ ProtoFailureException e
	StoreContent k af o l getb validitycheck next -> do
		-- This is the same as the retrievalSecurityPolicy of
		-- Remote.P2P and Remote.Git.
		let rsp = RetrievalAllKeysSecure
		v <- tryNonAsync $ do
			iv <- startVerifyKeyContentIncrementally DefaultVerify k
			let runtransfer ti = 
				Right <$> transfer download' k af Nothing (\p ->
					logStatusAfter k $ getViaTmp rsp DefaultVerify k af $ \tmp ->
						storefile (fromRawFilePath tmp) o l getb iv validitycheck p ti)
			let fallback = return $ Left $
				ProtoFailureMessage "transfer already in progress, or unable to take transfer lock"
			checktransfer runtransfer fallback
		case v of
			Left e -> return $ Left $ ProtoFailureException e
			Right (Left e) -> return $ Left e
			Right (Right ok) -> runner (next ok)
	StoreContentTo dest iv o l getb validitycheck next -> do
		v <- tryNonAsync $ do
			let runtransfer ti = Right
				<$> storefile dest o l getb iv validitycheck nullMeterUpdate ti
			let fallback = return $ Left $
				ProtoFailureMessage "transfer failed"
			checktransfer runtransfer fallback
		case v of
			Left e -> return $ Left $ ProtoFailureException e
			Right (Left e) -> return $ Left e
			Right (Right ok) -> runner (next ok)
	SetPresent k u next -> do
		v <- tryNonAsync $ logChange k u InfoPresent
		case v of
			Left e -> return $ Left $ ProtoFailureException e
			Right () -> runner next
	CheckContentPresent k next -> do
		v <- tryNonAsync $ inAnnex k
		case v of
			Left e -> return $ Left $ ProtoFailureException e
			Right result -> runner (next result)
	RemoveContent k next -> do
		let cleanup = do
			logStatus k InfoMissing
			return True
		v <- tryNonAsync $
			ifM (Annex.Content.inAnnex k)
				( lockContentForRemoval k cleanup $ \contentlock -> do
					removeAnnex contentlock
					cleanup
				, return True
				)
		case v of
			Left e -> return $ Left $ ProtoFailureException e
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
				Left e -> return $ Left $ ProtoFailureException e
				Right changedrefs -> runner (next changedrefs)
		_ -> return $ Left $
			ProtoFailureMessage "change notification not available"
	UpdateMeterTotalSize m sz next -> do
		liftIO $ setMeterTotalSize m sz
		runner next
	RunValidityCheck checkaction next -> runner . next =<< checkaction
  where
	transfer mk k af sd ta = case runst of
		-- Update transfer logs when serving.
		-- Using noRetry because we're the sender.
		Serving theiruuid _ _ -> 
			mk theiruuid k af sd noRetry ta noNotification
		-- Transfer logs are updated higher in the stack when
		-- a client.
		Client _ -> ta nullMeterUpdate
	
	resumefromoffset o incrementalverifier p h
		| o /= 0 = do
			p' <- case incrementalverifier of
				Just iv -> do
					go iv o
					return p
				_ -> return $ offsetMeterUpdate p (toBytesProcessed o)
			-- Make sure the handle is seeked to the offset.
			-- (Reading the file probably left it there
			-- when that was done, but let's be sure.)
			hSeek h AbsoluteSeek o
			return p'
		| otherwise = return p
	  where
		go iv n
			| n == 0 = return ()
			| otherwise = do
				let c = if n > fromIntegral defaultChunkSize
					then defaultChunkSize
					else fromIntegral n
				b <- S.hGet h c
				updateIncremental iv b
				unless (b == S.empty) $
					go iv (n - fromIntegral (S.length b))

	storefile dest (Offset o) (Len l) getb incrementalverifier validitycheck p ti = do
		v <- runner getb
		case v of
			Right b -> do
				liftIO $ withBinaryFile dest ReadWriteMode $ \h -> do
					p' <- resumefromoffset o incrementalverifier p h
					let writechunk = case incrementalverifier of
						Nothing -> \c -> S.hPut h c
						Just iv -> \c -> do
							S.hPut h c
							updateIncremental iv c
					meteredWrite p' writechunk b
				indicatetransferred ti

				rightsize <- do
					sz <- liftIO $ getFileSize (toRawFilePath dest)
					return (toInteger sz == l + o)
					
				runner validitycheck >>= \case
					Right (Just Valid) -> case incrementalverifier of
						Just iv -> ifM (liftIO (finalizeIncremental iv) <&&> pure rightsize)
							( return (True, Verified)
							, return (False, UnVerified)
							)
						Nothing -> return (rightsize, UnVerified)
					Right (Just Invalid) | l == 0 ->
						-- Special case, for when
						-- content was not
						-- available to send, 
						-- which is indicated by
						-- sending 0 bytes and 
						-- Invalid.
						return (False, UnVerified)
					_ -> do
						-- Invalid, or old protocol
						-- version. Validity is not
						-- known. Force content
						-- verification.
						return (rightsize, MustVerify)
			Left e -> error $ describeProtoFailure e
	
	sinkfile f (Offset o) checkchanged sender p ti = bracket setup cleanup go
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
			r <- runner (sender b validitycheck)
			indicatetransferred ti
			return r
	
	-- This allows using actions like download and viaTmp
	-- that may abort a transfer, and clean up the protocol after them.
	--
	-- Runs an action that may make a transfer, passing a transfer
	-- indicator. The action should call indicatetransferred on it,
	-- only after it's actually sent/received the all data.
	--
	-- If the action ends without having called indicatetransferred,
	-- runs the fallback action, which can close the protoocol
	-- connection or otherwise clean up after the transfer not having
	-- occurred.
	--
	-- If the action throws an exception, the fallback is not run.
	checktransfer ta fallback = do
		ti <- liftIO $ newTVarIO False
		r <- ta ti
		ifM (liftIO $ atomically $ readTVar ti)
			( return r
			, fallback
			)

	indicatetransferred ti = liftIO $ atomically $ writeTVar ti True
