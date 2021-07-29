{- verification
 -
 - Copyright 2010-2021 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Annex.Verify (
	VerifyConfig(..),
	shouldVerify,
	verifyKeyContentPostRetrieval,
	verifyKeyContent,
	Verification(..),
	unVerified,
	warnUnverifiableInsecure,
	isVerifiable,
	startVerifyKeyContentIncrementally,
	IncrementalVerifier(..),
) where

import Annex.Common
import qualified Annex
import qualified Types.Remote
import qualified Types.Backend
import Types.Backend (IncrementalVerifier(..))
import qualified Backend
import Types.Remote (unVerified, Verification(..), RetrievalSecurityPolicy(..))
import Annex.WorkerPool
import Types.WorkerPool
import Types.Key

data VerifyConfig = AlwaysVerify | NoVerify | RemoteVerify Remote | DefaultVerify

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
  where
	verify = enteringStage VerifyStage $ verifyKeyContent k f

verifyKeyContent :: Key -> RawFilePath -> Annex Bool
verifyKeyContent k f = verifysize <&&> verifycontent
  where
	verifysize = case fromKey keySize k of
		Nothing -> return True
		Just size -> do
			size' <- liftIO $ catchDefaultIO 0 $ getFileSize f
			return (size' == size)
	verifycontent = Backend.maybeLookupBackendVariety (fromKey keyVariety k) >>= \case
		Nothing -> return True
		Just b -> case Types.Backend.verifyKeyContent b of
			Nothing -> return True
			Just verifier -> verifier k f

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
