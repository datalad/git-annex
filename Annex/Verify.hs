{- verification
 -
 - Copyright 2010-2021 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Annex.Verify (
	shouldVerify,
	verifyKeyContentPostRetrieval,
	verifyKeyContent,
	Verification(..),
	unVerified,
	warnUnverifiableInsecure,
	isVerifiable,
	startVerifyKeyContentIncrementally,
	IncrementalVerifier(..),
	tailVerify,
) where

import Annex.Common
import qualified Annex
import qualified Types.Remote
import Types.Remote (VerifyConfigA(..))
import qualified Types.Backend
import Types.Backend (IncrementalVerifier(..))
import qualified Backend
import Types.Remote (unVerified, Verification(..), RetrievalSecurityPolicy(..))
import Annex.WorkerPool
import Types.WorkerPool
import Types.Key

import Control.Concurrent.STM
#if WITH_INOTIFY
import qualified System.INotify as INotify
import qualified Data.ByteString as S
import qualified System.FilePath.ByteString as P
#endif

shouldVerify :: VerifyConfig -> Annex Bool
shouldVerify AlwaysVerify = return True
shouldVerify NoVerify = return False
shouldVerify DefaultVerify = annexVerify <$> Annex.getGitConfig
shouldVerify (RemoteVerify r) = 
	(shouldVerify DefaultVerify
			<&&> pure (remoteAnnexVerify (Types.Remote.gitconfig r)))
	-- Export remotes are not key/value stores, so always verify
	-- content from them even when verification is disabled.
	<||> Types.Remote.isExportSupported r

{- Verifies that a file is the expected content of a key.
 -
 - Configuration can prevent verification, for either a
 - particular remote or always, unless the RetrievalSecurityPolicy
 - requires verification.
 -
 - Most keys have a known size, and if so, the file size is checked.
 -
 - When the key's backend allows verifying the content (via checksum),
 - it is checked. 
 -
 - If the RetrievalSecurityPolicy requires verification and the key's
 - backend doesn't support it, the verification will fail.
 -}
verifyKeyContentPostRetrieval :: RetrievalSecurityPolicy -> VerifyConfig -> Verification -> Key -> RawFilePath -> Annex Bool
verifyKeyContentPostRetrieval rsp v verification k f = case (rsp, verification) of
	(_, Verified) -> return True
	(RetrievalVerifiableKeysSecure, _) -> ifM (isVerifiable k)
		( verify
		, ifM (annexAllowUnverifiedDownloads <$> Annex.getGitConfig)
			( verify
			, warnUnverifiableInsecure k >> return False
			)
		)
	(_, UnVerified) -> ifM (shouldVerify v)
		( verify
		, return True
		)
	(_, MustVerify) -> verify
	(_, IncompleteVerify _) -> ifM (shouldVerify v)
		( verify
		, return True
		)
  where
	verify = enteringStage VerifyStage $
		case verification of
			IncompleteVerify iv -> resumeVerifyKeyContent k f iv
			_ -> verifyKeyContent k f

verifyKeyContent :: Key -> RawFilePath -> Annex Bool
verifyKeyContent k f = verifyKeySize k f <&&> verifyKeyContent' k f

verifyKeyContent' :: Key -> RawFilePath -> Annex Bool
verifyKeyContent' k f = 
	Backend.maybeLookupBackendVariety (fromKey keyVariety k) >>= \case
		Nothing -> return True
		Just b -> case Types.Backend.verifyKeyContent b of
			Nothing -> return True
			Just verifier -> verifier k f

resumeVerifyKeyContent :: Key -> RawFilePath -> IncrementalVerifier -> Annex Bool
resumeVerifyKeyContent k f iv = liftIO (positionIncremental iv) >>= \case
	Nothing -> fallback
	Just endpos -> do
		fsz <- liftIO $ catchDefaultIO 0 $ getFileSize f
		if fsz < endpos
			then fallback
			else case fromKey keySize k of
				Just size | fsz /= size -> return False
				_ -> go fsz endpos
  where
	fallback = verifyKeyContent k f

	go fsz endpos
		| fsz == endpos =
			liftIO $ catchDefaultIO False $
				finalizeIncremental iv
		| otherwise = do
			showAction (descVerify iv)
			liftIO $ catchDefaultIO False $
				withBinaryFile (fromRawFilePath f) ReadMode $ \h -> do
					hSeek h AbsoluteSeek endpos
					feedincremental h
					finalizeIncremental iv
	
	feedincremental h = do
		b <- S.hGetSome h chunk
		if S.null b
			then return ()
			else do
				updateIncremental iv b
				feedincremental h

	chunk = 65536

verifyKeySize :: Key -> RawFilePath -> Annex Bool
verifyKeySize k f = case fromKey keySize k of
	Just size -> do
		size' <- liftIO $ catchDefaultIO 0 $ getFileSize f
		return (size' == size)
	Nothing -> return True

warnUnverifiableInsecure :: Key -> Annex ()
warnUnverifiableInsecure k = warning $ unwords
	[ "Getting " ++ kv ++ " keys with this remote is not secure;"
	, "the content cannot be verified to be correct."
	, "(Use annex.security.allow-unverified-downloads to bypass"
	, "this safety check.)"
	]
  where
	kv = decodeBS (formatKeyVariety (fromKey keyVariety k))

isVerifiable :: Key -> Annex Bool
isVerifiable k = maybe False (isJust . Types.Backend.verifyKeyContent) 
	<$> Backend.maybeLookupBackendVariety (fromKey keyVariety k)

startVerifyKeyContentIncrementally :: VerifyConfig -> Key -> Annex (Maybe IncrementalVerifier)
startVerifyKeyContentIncrementally verifyconfig k = 
	ifM (shouldVerify verifyconfig)
		( Backend.maybeLookupBackendVariety (fromKey keyVariety k) >>= \case
			Just b -> case Types.Backend.verifyKeyContentIncrementally b of
				Just v -> Just <$> v k
				Nothing -> return Nothing
			Nothing -> return Nothing
		, return Nothing
		)

-- | Reads the file as it grows, and feeds it to the incremental verifier.
-- 
-- The TMVar must start out empty, and be filled once whatever is
-- writing to the file finishes. Once the writer finishes, this returns
-- quickly. It may not feed the entire content of the file to the
-- incremental verifier.
--
-- The file does not need to exist yet when this is called. It will wait
-- for the file to appear before opening it and starting verification.
--
-- This is not supported for all OSs, and on OS's where it is not
-- supported, verification will fail.
--
-- The writer probably needs to be another process. If the file is being
-- written directly by git-annex, the haskell RTS will prevent opening it
-- for read at the same time, and verification will fail.
--
-- Note that there are situations where the file may fail to verify despite
-- having the correct content. For example, when the file is written out
-- of order, or gets replaced part way through. To deal with such cases,
-- when verification fails, it should not be treated as if the file's
-- content is known to be incorrect, but instead as an indication that the
-- file should be verified again, once it's done being written to.
--
-- (It is also possible, in theory, for a file to verify despite having
-- incorrect content. For that to happen, the file would need to have
-- the right content when this checks it, but then the content gets
-- changed later by whatever is writing to the file.)
--
-- This should be fairly efficient, reading from the disk cache,
-- as long as the writer does not get very far ahead of it. However,
-- there are situations where it would be much less expensive to verify
-- chunks as they are being written. For example, when resuming with
-- a lot of content in the file, all that content needs to be read,
-- and if the disk is slow, the reader may never catch up to the writer,
-- and the disk cache may never speed up reads. So this should only be
-- used when there's not a better way to incrementally verify.
tailVerify :: IncrementalVerifier -> RawFilePath -> TMVar () -> IO ()
#if WITH_INOTIFY
tailVerify iv f finished = 
	tryNonAsync go >>= \case
		Right r -> return r
		Left _ -> failIncremental iv
  where
	-- Watch the directory containing the file, and wait for
	-- the file to be modified. It's possible that the file already
	-- exists before the downloader starts, but it replaces it instead
	-- of resuming, and waiting for modification deals with such
	-- situations.
	inotifydirchange i cont =
		INotify.addWatch i [INotify.Modify] dir $ \case
			-- Ignore changes to other files in the directory.
			INotify.Modified { INotify.maybeFilePath = fn }
				| fn == Just basef -> cont
			_ -> noop
	  where
		(dir, basef) = P.splitFileName f
	
	inotifyfilechange i = INotify.addWatch i [INotify.Modify] f . const

	go = INotify.withINotify $ \i -> do
		modified <- newEmptyTMVarIO
		let signalmodified = atomically $ void $ tryPutTMVar modified ()
		wd <- inotifydirchange i signalmodified
		let cleanup = void . tryNonAsync . INotify.removeWatch
		let stop w = do
			cleanup w
			failIncremental iv
		waitopen modified >>= \case
			Nothing -> stop wd
			Just h -> do
				cleanup wd
				wf <- inotifyfilechange i signalmodified
				tryNonAsync (follow h modified) >>= \case
					Left _ -> stop wf
					Right () -> cleanup wf
				hClose h

	waitopen modified = do
		v <- atomically $
			(Just <$> takeTMVar modified)
				`orElse` 
			((const Nothing) <$> takeTMVar finished)
		case v of
			Just () -> do
				r <- tryNonAsync $
					tryWhenExists (openBinaryFile (fromRawFilePath f) ReadMode) >>= \case
						Just h -> return (Just h)
						-- File does not exist, must have been
						-- deleted. Wait for next modification
						-- and try again.
						Nothing -> waitopen modified
				case r of
					Right r' -> return r'
					-- Permission error prevents
					-- reading, or this same process
					-- is writing to the file,
					-- and it cannot be read at the
					-- same time.
					Left _ -> return Nothing
			-- finished without the file being modified
			Nothing -> return Nothing
	
	follow h modified = do
		b <- S.hGetNonBlocking h chunk
		if S.null b
			then do
				-- We've caught up to the writer.
				-- Wait for the file to get modified again,
				-- or until we're told it is done being
				-- written.
				cont <- atomically $
					(const (follow h modified)
						<$> takeTMVar modified)
							`orElse`
					(const (return ())
						<$> takeTMVar finished)
				cont
			else do
				updateIncremental iv b
				atomically (tryTakeTMVar finished) >>= \case
					Nothing -> follow h modified
					Just () -> return ()

	chunk = 65536
#else
tailVerify iv _ _ = failIncremental iv
#endif
