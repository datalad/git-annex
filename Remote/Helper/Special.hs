{- helpers for special remotes
 -
 - Copyright 2011-2014 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Remote.Helper.Special (
	findSpecialRemotes,
	gitConfigSpecialRemote,
	Preparer,
	Storer,
	Retriever,
	Remover,
	CheckPresent,
	simplyPrepare,
	ContentSource,
	checkPrepare,
	resourcePrepare,
	fileStorer,
	byteStorer,
	fileRetriever,
	byteRetriever,
	storeKeyDummy,
	retreiveKeyFileDummy,
	removeKeyDummy,
	checkPresentDummy,
	SpecialRemoteCfg(..),
	specialRemoteCfg,
	specialRemote,
	specialRemote',
	module X
) where

import Annex.Common
import qualified Annex
import Types.StoreRetrieve
import Types.Remote
import Crypto
import Config
import Config.Cost
import Utility.Metered
import Remote.Helper.Chunked as X
import Remote.Helper.Encryptable as X
import Remote.Helper.Messages
import Annex.Content
import Messages.Progress
import qualified Git
import qualified Git.Construct

import qualified Data.ByteString.Lazy as L
import qualified Data.Map as M

{- Special remotes don't have a configured url, so Git.Repo does not
 - automatically generate remotes for them. This looks for a different
 - configuration key instead.
 -}
findSpecialRemotes :: String -> Annex [Git.Repo]
findSpecialRemotes s = do
	m <- fromRepo Git.config
	liftIO $ mapM construct $ remotepairs m
  where
	remotepairs = M.toList . M.filterWithKey match
	construct (k,_) = Git.Construct.remoteNamedFromKey k (pure Git.Construct.fromUnknown)
	match k _ = "remote." `isPrefixOf` k && (".annex-"++s) `isSuffixOf` k

{- Sets up configuration for a special remote in .git/config. -}
gitConfigSpecialRemote :: UUID -> RemoteConfig -> String -> String -> Annex ()
gitConfigSpecialRemote u c k v = do
	setConfig (remoteConfig remotename k) v
	setConfig (remoteConfig remotename "uuid") (fromUUID u)
  where
	remotename = fromJust (M.lookup "name" c)

-- Use when nothing needs to be done to prepare a helper.
simplyPrepare :: helper -> Preparer helper
simplyPrepare helper _ a = a $ Just helper

-- Use to run a check when preparing a helper.
checkPrepare :: (Key -> Annex Bool) -> helper -> Preparer helper
checkPrepare checker helper k a = ifM (checker k)
	( a (Just helper)
	, a Nothing
	)

-- Use to acquire a resource when preparing a helper.
resourcePrepare :: (Key -> (r -> Annex Bool) -> Annex Bool) -> (r -> helper) -> Preparer helper
resourcePrepare withr helper k a = withr k $ \r ->
	a (Just (helper r))

-- A Storer that expects to be provided with a file containing
-- the content of the key to store.
fileStorer :: (Key -> FilePath -> MeterUpdate -> Annex Bool) -> Storer
fileStorer a k (FileContent f) m = a k f m
fileStorer a k (ByteContent b) m = withTmp k $ \f -> do
	liftIO $ L.writeFile f b
	a k f m

-- A Storer that expects to be provided with a L.ByteString of
-- the content to store.
byteStorer :: (Key -> L.ByteString -> MeterUpdate -> Annex Bool) -> Storer
byteStorer a k c m = withBytes c $ \b -> a k b m

-- A Retriever that writes the content of a Key to a provided file.
-- It is responsible for updating the progress meter as it retrieves data.
fileRetriever :: (FilePath -> Key -> MeterUpdate -> Annex ()) -> Retriever
fileRetriever a k m callback = do
	f <- prepTmp k
	a f k m
	callback (FileContent f)

-- A Retriever that generates a lazy ByteString containing the Key's
-- content, and passes it to a callback action which will fully consume it
-- before returning.
byteRetriever :: (Key -> (L.ByteString -> Annex Bool) -> Annex Bool) -> Retriever
byteRetriever a k _m callback = a k (callback . ByteContent)

{- The base Remote that is provided to specialRemote needs to have
 - storeKey, retrieveKeyFile, removeKey, and checkPresent methods,
 - but they are never actually used (since specialRemote replaces them).
 - Here are some dummy ones.
 -}
storeKeyDummy :: Key -> AssociatedFile -> MeterUpdate -> Annex Bool
storeKeyDummy _ _ _ = return False
retreiveKeyFileDummy :: Key -> AssociatedFile -> FilePath -> MeterUpdate -> Annex (Bool, Verification)
retreiveKeyFileDummy _ _ _ _ = unVerified (return False)
removeKeyDummy :: Key -> Annex Bool
removeKeyDummy _ = return False
checkPresentDummy :: Key -> Annex Bool
checkPresentDummy _ = error "missing checkPresent implementation"

type RemoteModifier
	= RemoteConfig
	-> Preparer Storer
	-> Preparer Retriever
	-> Preparer Remover
	-> Preparer CheckPresent
	-> Remote
	-> Remote

data SpecialRemoteCfg = SpecialRemoteCfg
	{ chunkConfig :: ChunkConfig
	, displayProgress :: Bool
	}

specialRemoteCfg :: RemoteConfig -> SpecialRemoteCfg
specialRemoteCfg c = SpecialRemoteCfg (getChunkConfig c) True

-- Modifies a base Remote to support both chunking and encryption,
-- which special remotes typically should support.
specialRemote :: RemoteModifier
specialRemote c = specialRemote' (specialRemoteCfg c) c

specialRemote' :: SpecialRemoteCfg -> RemoteModifier
specialRemote' cfg c preparestorer prepareretriever prepareremover preparecheckpresent baser = encr
  where
	encr = baser
		{ storeKey = \k _f p -> cip >>= storeKeyGen k p
		, retrieveKeyFile = \k _f d p -> cip >>= unVerified . retrieveKeyFileGen k d p
		, retrieveKeyFileCheap = \k f d -> cip >>= maybe
			(retrieveKeyFileCheap baser k f d)
			-- retrieval of encrypted keys is never cheap
			(\_ -> return False)
		, removeKey = \k -> cip >>= removeKeyGen k
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
		}
	cip = cipherKey c (gitconfig baser)
	isencrypted = isJust (extractCipher c)

	safely a = catchNonAsync a (\e -> warning (show e) >> return False)

	-- chunk, then encrypt, then feed to the storer
	storeKeyGen k p enc = safely $ preparestorer k $ safely . go
	  where
		go (Just storer) = preparecheckpresent k $ safely . go' storer
		go Nothing = return False
		go' storer (Just checker) = sendAnnex k rollback $ \src ->
			displayprogress p k (Just src) $ \p' ->
				storeChunks (uuid baser) chunkconfig enck k src p'
					(storechunk enc storer)
					checker
		go' _ Nothing = return False
		rollback = void $ removeKey encr k
		enck = maybe id snd enc

	storechunk Nothing storer k content p = storer k content p
	storechunk (Just (cipher, enck)) storer k content p = do
		cmd <- gpgCmd <$> Annex.getGitConfig
		withBytes content $ \b ->
			encrypt cmd encr cipher (feedBytes b) $
				readBytes $ \encb ->
					storer (enck k) (ByteContent encb) p

	-- call retriever to get chunks; decrypt them; stream to dest file
	retrieveKeyFileGen k dest p enc =
		safely $ prepareretriever k $ safely . go
	  where
		go (Just retriever) = displayprogress p k Nothing $ \p' ->
			retrieveChunks retriever (uuid baser) chunkconfig
				enck k dest p' (sink dest enc encr)
		go Nothing = return False
		enck = maybe id snd enc

	removeKeyGen k enc = safely $ prepareremover k $ safely . go
	  where
		go (Just remover) = removeChunks remover (uuid baser) chunkconfig enck k
		go Nothing = return False
		enck = maybe id snd enc

	checkPresentGen k enc = preparecheckpresent k go
	  where
		go (Just checker) = checkPresentChunks checker (uuid baser) chunkconfig enck k
		go Nothing = cantCheck baser
		enck = maybe id snd enc

	chunkconfig = chunkConfig cfg

	displayprogress p k srcfile a
		| displayProgress cfg = metered (Just p) k (return srcfile) a
		| otherwise = a p

{- Sink callback for retrieveChunks. Stores the file content into the
 - provided Handle, decrypting it first if necessary.
 - 
 - If the remote did not store the content using chunks, no Handle
 - will be provided, and it's up to us to open the destination file.
 -
 - Note that when neither chunking nor encryption is used, and the remote
 - provides FileContent, that file only needs to be renamed
 - into place. (And it may even already be in the right place..)
 -}
sink
	:: LensGpgEncParams c
	=> FilePath
	-> Maybe (Cipher, EncKey)
	-> c
	-> Maybe Handle
	-> Maybe MeterUpdate
	-> ContentSource
	-> Annex Bool
sink dest enc c mh mp content = do
	case (enc, mh, content) of
		(Nothing, Nothing, FileContent f)
			| f == dest -> noop
			| otherwise -> liftIO $ moveFile f dest
		(Just (cipher, _), _, ByteContent b) -> do
			cmd <- gpgCmd <$> Annex.getGitConfig
			decrypt cmd c cipher (feedBytes b) $
				readBytes write
		(Just (cipher, _), _, FileContent f) -> do
			cmd <- gpgCmd <$> Annex.getGitConfig
			withBytes content $ \b ->
				decrypt cmd c cipher (feedBytes b) $
					readBytes write
			liftIO $ nukeFile f
		(Nothing, _, FileContent f) -> do
			withBytes content write
			liftIO $ nukeFile f
		(Nothing, _, ByteContent b) -> write b
	return True
  where
	write b = case mh of
		Just h -> liftIO $ b `streamto` h
		Nothing -> liftIO $ bracket opendest hClose (b `streamto`)
	streamto b h = case mp of
		Just p -> meteredWrite p h b
		Nothing -> L.hPut h b
	opendest = openBinaryFile dest WriteMode

withBytes :: ContentSource -> (L.ByteString -> Annex a) -> Annex a
withBytes (ByteContent b) a = a b
withBytes (FileContent f) a = a =<< liftIO (L.readFile f)
