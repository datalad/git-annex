{- helpers for special remotes
 -
 - Copyright 2011-2021 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

module Remote.Helper.Special (
	findSpecialRemotes,
	gitConfigSpecialRemote,
	mkRetrievalVerifiableKeysSecure,
	Storer,
	Retriever,
	Remover,
	CheckPresent,
	ContentSource,
	fileStorer,
	byteStorer,
	fileRetriever,
	fileRetriever',
	byteRetriever,
	storeKeyDummy,
	retrieveKeyFileDummy,
	removeKeyDummy,
	checkPresentDummy,
	SpecialRemoteCfg(..),
	specialRemoteCfg,
	specialRemoteConfigParsers,
	specialRemoteType,
	specialRemote,
	specialRemote',
	lookupName,
	module X
) where

import Annex.Common
import Annex.SpecialRemote.Config
import Types.StoreRetrieve
import Types.Remote
import Annex.Verify
import Annex.UUID
import Annex.Perms
import Config
import Config.Cost
import Utility.Metered
import Remote.Helper.Chunked as X
import Remote.Helper.Encryptable as X
import Annex.Content
import Messages.Progress
import qualified Git
import qualified Git.Construct
import Git.Types

import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as L
import qualified Data.Map as M

{- Special remotes don't have a configured url, so Git.Repo does not
 - automatically generate remotes for them. This looks for a different
 - configuration key instead.
 -}
findSpecialRemotes :: String -> Annex [Git.Repo]
findSpecialRemotes s = do
	m <- fromRepo Git.config
	liftIO $ catMaybes <$> mapM construct (remotepairs m)
  where
	remotepairs = M.toList . M.filterWithKey match
	construct (k,_) = Git.Construct.remoteNamedFromKey k
		(pure Git.Construct.fromUnknown)
	match (ConfigKey k) _ =
		"remote." `S.isPrefixOf` k 
		&& (".annex-" <> encodeBS s) `S.isSuffixOf` k

{- Sets up configuration for a special remote in .git/config. -}
gitConfigSpecialRemote :: UUID -> RemoteConfig -> [(String, String)] -> Annex ()
gitConfigSpecialRemote u c cfgs = do
	forM_ cfgs $ \(k, v) -> 
		setConfig (remoteAnnexConfig c (encodeBS k)) v
	storeUUIDIn (remoteAnnexConfig c "uuid") u

-- RetrievalVerifiableKeysSecure unless overridden by git config.
--
-- Only looks at the RemoteGitConfig; the GitConfig's setting is
-- checked at the same place the RetrievalSecurityPolicy is checked.
mkRetrievalVerifiableKeysSecure :: RemoteGitConfig -> RetrievalSecurityPolicy
mkRetrievalVerifiableKeysSecure gc
	| remoteAnnexAllowUnverifiedDownloads gc = RetrievalAllKeysSecure
	| otherwise = RetrievalVerifiableKeysSecure

-- A Storer that expects to be provided with a file containing
-- the content of the key to store.
fileStorer :: (Key -> FilePath -> MeterUpdate -> Annex ()) -> Storer
fileStorer a k (FileContent f) m = a k f m
fileStorer a k (ByteContent b) m = withTmp k $ \f -> do
	let f' = fromRawFilePath f
	liftIO $ L.writeFile f' b
	a k f' m

-- A Storer that expects to be provided with a L.ByteString of
-- the content to store.
byteStorer :: (Key -> L.ByteString -> MeterUpdate -> Annex ()) -> Storer
byteStorer a k c m = withBytes c $ \b -> a k b m

-- A Retriever that generates a lazy ByteString containing the Key's
-- content, and passes it to a callback action which will fully consume it
-- before returning.
byteRetriever :: (Key -> (L.ByteString -> Annex a) -> Annex a) -> Key -> MeterUpdate -> RawFilePath -> Maybe IncrementalVerifier -> (ContentSource -> Annex a) -> Annex a
byteRetriever a k _m _dest _miv callback = a k (callback . ByteContent)

-- A Retriever that writes the content of a Key to a file.
-- The action is responsible for updating the progress meter as it 
-- retrieves data. The incremental verifier is updated in the background as
-- the action writes to the file, but may not be updated with the entire
-- content of the file.
fileRetriever :: (RawFilePath -> Key -> MeterUpdate -> Annex ()) -> Retriever
fileRetriever a = fileRetriever' $ \f k m miv -> 
	let retrieve = a f k m
	in tailVerify miv f retrieve

{- A Retriever that writes the content of a Key to a file.
 - The action is responsible for updating the progress meter and the 
 - incremental verifier as it retrieves data.
 -}
fileRetriever' :: (RawFilePath -> Key -> MeterUpdate -> Maybe IncrementalVerifier -> Annex ()) -> Retriever
fileRetriever' a k m dest miv callback = do
	createAnnexDirectory (parentDir dest)
	a dest k m miv
	pruneTmpWorkDirBefore dest (callback . FileContent . fromRawFilePath)

{- The base Remote that is provided to specialRemote needs to have
 - storeKey, retrieveKeyFile, removeKey, and checkPresent methods,
 - but they are never actually used (since specialRemote replaces them).
 - Here are some dummy ones.
 -}
storeKeyDummy :: Key -> AssociatedFile -> Maybe FilePath -> MeterUpdate -> Annex ()
storeKeyDummy _ _ _ _ = error "missing storeKey implementation"
retrieveKeyFileDummy :: Key -> AssociatedFile -> FilePath -> MeterUpdate -> VerifyConfig -> Annex Verification
retrieveKeyFileDummy _ _ _ _ _ = error "missing retrieveKeyFile implementation"
removeKeyDummy :: Maybe SafeDropProof -> Key -> Annex ()
removeKeyDummy _ _ = error "missing removeKey implementation"
checkPresentDummy :: Key -> Annex Bool
checkPresentDummy _ = error "missing checkPresent implementation"

type RemoteModifier
	= ParsedRemoteConfig
	-> Storer
	-> Retriever
	-> Remover
	-> CheckPresent
	-> Remote
	-> Remote

data SpecialRemoteCfg = SpecialRemoteCfg
	{ chunkConfig :: ChunkConfig
	, displayProgress :: Bool
	}

specialRemoteCfg :: ParsedRemoteConfig -> SpecialRemoteCfg
specialRemoteCfg c = SpecialRemoteCfg (getChunkConfig c) True

-- Modifies a base RemoteType to support chunking and encryption configs.
specialRemoteType :: RemoteType -> RemoteType
specialRemoteType r = r 
	{ configParser = \c -> addRemoteConfigParser specialRemoteConfigParsers
		<$> configParser r c
	}

specialRemoteConfigParsers :: [RemoteConfigFieldParser]
specialRemoteConfigParsers = chunkConfigParsers ++ encryptionConfigParsers

-- Modifies a base Remote to support both chunking and encryption,
-- which special remotes typically should support.
-- 
-- Handles progress displays when displayProgress is set.
specialRemote :: RemoteModifier
specialRemote c = specialRemote' (specialRemoteCfg c) c

specialRemote' :: SpecialRemoteCfg -> RemoteModifier
specialRemote' cfg c storer retriever remover checkpresent baser = encr
  where
	encr = baser
		{ storeKey = \k _af o p -> cip >>= storeKeyGen k o p
		, retrieveKeyFile = \k _f d p vc -> cip >>= retrieveKeyFileGen k d p vc
		, retrieveKeyFileCheap = case retrieveKeyFileCheap baser of
			Nothing -> Nothing
			Just a
				-- retrieval of encrypted keys is never cheap
				| isencrypted -> Nothing
				| otherwise -> Just $ \k f d -> a k f d
		-- When encryption is used, the remote could provide
		-- some other content encrypted by the user, and trick
		-- git-annex into decrypting it, leaking the decryption
		-- into the git-annex repository. Verifiable keys
		-- are the main protection against this attack.
		, retrievalSecurityPolicy = if isencrypted
			then mkRetrievalVerifiableKeysSecure (gitconfig baser)
			else retrievalSecurityPolicy baser
		, removeKey = \k proof -> cip >>= removeKeyGen k proof
		, checkPresent = \k -> cip >>= checkPresentGen k
		, cost = if isencrypted
			then cost baser + encryptedRemoteCostAdj
			else cost baser
		, getInfo = do
			l <- getInfo baser
			return $ l ++
				[ ("encryption", describeEncryption c)
				, ("chunking", describeChunkConfig (chunkConfig cfg))
				]
		, whereisKey = if noChunks (chunkConfig cfg) && not isencrypted
			then whereisKey baser
			else Nothing
		, exportActions = (exportActions baser)
			{ storeExport = \f k l p -> displayprogress uploadbwlimit p k (Just f) $
				storeExport (exportActions baser) f k l
			, retrieveExport = \k l f p -> displayprogress downloadbwlimit p k Nothing $
				retrieveExport (exportActions baser) k l f
			}
		}
	cip = cipherKey c (gitconfig baser)
	isencrypted = isEncrypted c

	-- chunk, then encrypt, then feed to the storer
	storeKeyGen k o p enc = sendAnnex k o rollback $ \src _sz ->
		displayprogress uploadbwlimit p k (Just src) $ \p' ->
			storeChunks (uuid baser) chunkconfig enck k src p'
				enc encr storer checkpresent
	  where
		rollback = void $ removeKey encr Nothing k
		enck = maybe id snd enc

	-- call retriever to get chunks; decrypt them; stream to dest file
	retrieveKeyFileGen k dest p vc enc =
		displayprogress downloadbwlimit p k Nothing $ \p' ->
			retrieveChunks retriever (uuid baser) vc
				chunkconfig enck k dest p' enc encr
	  where
		enck = maybe id snd enc

	removeKeyGen proof k enc = 
		removeChunks remover (uuid baser) chunkconfig enck proof k
	  where
		enck = maybe id snd enc

	checkPresentGen k enc = 
		checkPresentChunks checkpresent (uuid baser) chunkconfig enck k
	  where
		enck = maybe id snd enc

	chunkconfig = chunkConfig cfg

	downloadbwlimit = remoteAnnexBwLimitDownload (gitconfig baser)
		<|> remoteAnnexBwLimit (gitconfig baser)
	uploadbwlimit = remoteAnnexBwLimitUpload (gitconfig baser)
		<|> remoteAnnexBwLimit (gitconfig baser)

	displayprogress bwlimit p k srcfile a
		| displayProgress cfg = do
			metered (Just p) (KeySizer k (pure (fmap toRawFilePath srcfile))) bwlimit (const a)
		| otherwise = a p

withBytes :: ContentSource -> (L.ByteString -> Annex a) -> Annex a
withBytes (ByteContent b) a = a b
withBytes (FileContent f) a = a =<< liftIO (L.readFile f)
