{- git-annex command
 -
 - Copyright 2014-2020 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE RankNTypes #-}

module Command.TestRemote where

import Command
import qualified Annex
import qualified Remote
import qualified Types.Remote as Remote
import qualified Types.Backend as Backend
import Types.KeySource
import Annex.Content
import Annex.WorkTree
import Backend
import Logs.Location
import qualified Backend.Hash
import Utility.Tmp
import Utility.Metered
import Utility.DataUnits
import Utility.CopyFile
import Types.Messages
import Types.Export
import Types.RemoteConfig
import Types.ProposedAccepted
import Annex.SpecialRemote.Config (exportTreeField)
import Remote.Helper.Chunked
import Remote.Helper.Encryptable (describeEncryption, encryptionField, highRandomQualityField)
import Git.Types

import Test.Tasty
import Test.Tasty.Runners
import Test.Tasty.HUnit
import "crypto-api" Crypto.Random
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Map as M
import Control.Concurrent.STM hiding (check)

cmd :: Command
cmd = command "testremote" SectionTesting
	"test transfers to/from a remote"
	paramRemote (seek <$$> optParser)

data TestRemoteOptions = TestRemoteOptions
	{ testRemote :: RemoteName
	, sizeOption :: ByteSize
	, testReadonlyFile :: [FilePath]
	}

optParser :: CmdParamsDesc -> Parser TestRemoteOptions
optParser desc = TestRemoteOptions
	<$> argument str ( metavar desc )
	<*> option (str >>= maybe (fail "parse error") return . readSize dataUnits)
		( long "size" <> metavar paramSize
		<> value (1024 * 1024)
		<> help "base key size (default 1MiB)"
		)
	<*> many testreadonly
  where
	testreadonly = option str
		( long "test-readonly" <> metavar paramFile
		<> help "readonly test object"
		)

seek :: TestRemoteOptions -> CommandSeek
seek = commandAction . start 

start :: TestRemoteOptions -> CommandStart
start o = starting "testremote" (ActionItemOther (Just (testRemote o))) $ do
	fast <- Annex.getState Annex.fast
	r <- either giveup disableExportTree =<< Remote.byName' (testRemote o)
	ks <- case testReadonlyFile o of
		[] -> if Remote.readonly r
			then giveup "This remote is readonly, so you need to use the --test-readonly option."
			else do
				showAction "generating test keys"
				mapM randKey (keySizes basesz fast)
		fs -> mapM (getReadonlyKey r) fs
	let r' = if null (testReadonlyFile o)
		then r
		else r { Remote.readonly = True }
	rs <- if Remote.readonly r'
		then return [r']
		else do
			rs <- catMaybes <$> mapM (adjustChunkSize r') (chunkSizes basesz fast)
			concat <$> mapM encryptionVariants rs
	unavailrs  <- catMaybes <$> mapM Remote.mkUnavailable [r']
	exportr <- if Remote.readonly r'
		then return Nothing
		else exportTreeVariant r'
	perform rs unavailrs exportr ks
  where
	basesz = fromInteger $ sizeOption o

perform :: [Remote] -> [Remote] -> Maybe Remote -> [Key] -> CommandPerform
perform rs unavailrs exportr ks = do
	st <- liftIO . newTVarIO =<< Annex.getState id
	let tests = mkTestTree
		(runTestCase st) 
		(map (\r -> Described (descr r) (pure r)) rs)
		(map (\r -> Described (descr r) (pure r)) unavailrs)
		(fmap pure exportr)
		(map (\k -> Described (desck k) (pure k)) ks)
	ok <- case tryIngredients [consoleTestReporter] mempty tests of
		Nothing -> error "No tests found!?"
		Just act -> liftIO act
	next $ cleanup rs ks ok
  where
	descr r = intercalate "; " $ map unwords
		[ [ show (getChunkConfig (Remote.config r)) ]
		, ["encryption", describeEncryption (Remote.config r)]
		]
	desck k = unwords [ "key size", show (fromKey keySize k) ]

adjustChunkSize :: Remote -> Int -> Annex (Maybe Remote)
adjustChunkSize r chunksize = adjustRemoteConfig r $
	M.insert chunkField (Proposed (show chunksize))

-- Variants of a remote with no encryption, and with simple shared
-- encryption. Gpg key based encryption is not tested.
encryptionVariants :: Remote -> Annex [Remote]
encryptionVariants r = do
	noenc <- adjustRemoteConfig r $
		M.insert encryptionField (Proposed "none")
	sharedenc <- adjustRemoteConfig r $
		M.insert encryptionField (Proposed "shared") .
		M.insert highRandomQualityField (Proposed "false")
	return $ catMaybes [noenc, sharedenc]

-- Variant of a remote with exporttree disabled.
disableExportTree :: Remote -> Annex Remote
disableExportTree r = maybe (error "failed disabling exportree") return 
		=<< adjustRemoteConfig r (M.delete exportTreeField)

-- Variant of a remote with exporttree enabled.
exportTreeVariant :: Remote -> Annex (Maybe Remote)
exportTreeVariant r = ifM (Remote.isExportSupported r)
	( adjustRemoteConfig r $
		M.insert encryptionField (Proposed "none") . 
		M.insert exportTreeField (Proposed "yes")
	, return Nothing
	)

-- Regenerate a remote with a modified config.
adjustRemoteConfig :: Remote -> (Remote.RemoteConfig -> Remote.RemoteConfig) -> Annex (Maybe Remote)
adjustRemoteConfig r adjustconfig = do
	repo <- Remote.getRepo r
	let ParsedRemoteConfig _ origc = Remote.config r
	Remote.generate (Remote.remotetype r)
		repo
		(Remote.uuid r)
		(adjustconfig origc)
		(Remote.gitconfig r)
		(Remote.remoteStateHandle r)

data Described t = Described
	{ getDesc :: String
	, getVal :: t
	}

type RunAnnex = forall a. Annex a -> IO a

runTestCase :: TVar Annex.AnnexState -> RunAnnex
runTestCase stv a = do
	st <- atomically $ readTVar stv
	(r, st') <- Annex.run st $ do
		Annex.setOutput QuietOutput 
		a
	atomically $ writeTVar stv st'
	return r

-- Note that the same remotes and keys should be produced each time
-- the provided actions are called.
mkTestTree
	:: RunAnnex
	-> [Described (Annex Remote)]
	-> [Described (Annex Remote)]
	-> Maybe (Annex Remote)
	-> [Described (Annex Key)]
	-> TestTree
mkTestTree runannex mkrs mkunavailrs mkexportr mkks = testGroup "Remote Tests" $ concat
	[ [ testGroup "unavailable remote" (testUnavailable runannex (getVal mkr) (getVal (Prelude.head mkks))) | mkr <- mkunavailrs ]
	, [ testGroup (desc mkr mkk) (test runannex (getVal mkr) (getVal mkk)) | mkk <- mkks, mkr <- mkrs ]
	, [ testGroup (descexport mkk1 mkk2) (testExportTree runannex mkexportr (getVal mkk1) (getVal mkk2)) | mkk1 <- take 2 mkks, mkk2 <- take 2 (reverse mkks) ]
	]
   where
	desc r k = intercalate "; " $ map unwords
		[ [ getDesc k ]
		, [ getDesc r ]
		]
	descexport k1 k2 = intercalate "; " $ map unwords
		[ [ "exporttree=yes" ]
		, [ getDesc k1 ]
		, [ getDesc k2 ]
		]

test :: RunAnnex -> Annex Remote -> Annex Key -> [TestTree]
test runannex mkr mkk =
	[ check "removeKey when not present" $ \r k ->
		whenwritable r $ remove r k
	, check ("present " ++ show False) $ \r k ->
		whenwritable r $ present r k False
	, check "storeKey" $ \r k ->
		whenwritable r $ store r k
	, check ("present " ++ show True) $ \r k ->
		whenwritable r $ present r k True
	, check "storeKey when already present" $ \r k ->
		whenwritable r $ store r k
	, check ("present " ++ show True) $ \r k -> present r k True
	, check "retrieveKeyFile" $ \r k -> do
		lockContentForRemoval k removeAnnex
		get r k
	, check "fsck downloaded object" fsck
	, check "retrieveKeyFile resume from 33%" $ \r k -> do
		loc <- fromRawFilePath <$> Annex.calcRepo (gitAnnexLocation k)
		tmp <- prepTmp k
		partial <- liftIO $ bracket (openBinaryFile loc ReadMode) hClose $ \h -> do
			sz <- hFileSize h
			L.hGet h $ fromInteger $ sz `div` 3
		liftIO $ L.writeFile tmp partial
		lockContentForRemoval k removeAnnex
		get r k
	, check "fsck downloaded object" fsck
	, check "retrieveKeyFile resume from 0" $ \r k -> do
		tmp <- prepTmp k
		liftIO $ writeFile tmp ""
		lockContentForRemoval k removeAnnex
		get r k
	, check "fsck downloaded object" fsck
	, check "retrieveKeyFile resume from end" $ \r k -> do
		loc <- fromRawFilePath <$> Annex.calcRepo (gitAnnexLocation k)
		tmp <- prepTmp k
		void $ liftIO $ copyFileExternal CopyAllMetaData loc tmp
		lockContentForRemoval k removeAnnex
		get r k
	, check "fsck downloaded object" fsck
	, check "removeKey when present" $ \r k -> 
		whenwritable r $ remove r k
	, check ("present " ++ show False) $ \r k -> 
		whenwritable r $ present r k False
	]
  where
	whenwritable r a
		| Remote.readonly r = return True
		| otherwise = a
	check desc a = testCase desc $ do
		let a' = do
			r <- mkr
			k <- mkk
			a r k
		runannex a' @? "failed"
	present r k b = (== Right b) <$> Remote.hasKey r k
	fsck _ k = case maybeLookupBackendVariety (fromKey keyVariety k) of
		Nothing -> return True
		Just b -> case Backend.verifyKeyContent b of
			Nothing -> return True
			Just verifier -> verifier k (serializeKey k)
	get r k = getViaTmp (Remote.retrievalSecurityPolicy r) (RemoteVerify r) k $ \dest ->
		Remote.retrieveKeyFile r k (AssociatedFile Nothing)
			dest nullMeterUpdate
	store r k = Remote.storeKey r k (AssociatedFile Nothing) nullMeterUpdate
	remove r k = Remote.removeKey r k

testExportTree :: RunAnnex -> Maybe (Annex Remote) -> Annex Key -> Annex Key -> [TestTree]
testExportTree _ Nothing _ _ = []
testExportTree runannex (Just mkr) mkk1 mkk2 =
	[ check "check present export when not present" $ \ea k1 _k2 ->
		not <$> checkpresentexport ea k1
	, check "remove export when not present" $ \ea k1 _k2 -> 
		removeexport ea k1
	, check "store export" $ \ea k1 _k2 ->
		storeexport ea k1
	, check "check present export after store" $ \ea k1 _k2 ->
		checkpresentexport ea k1
	, check "store export when already present" $ \ea k1 _k2 ->
		storeexport ea k1
	, check "retrieve export" $ \ea k1 _k2 -> 
		retrieveexport ea k1
	, check "store new content to export" $ \ea _k1 k2 ->
		storeexport ea k2
	, check "check present export after store of new content" $ \ea _k1 k2 ->
		checkpresentexport ea k2
	, check "retrieve export new content" $ \ea _k1 k2 ->
		retrieveexport ea k2
	, check "remove export" $ \ea _k1 k2 -> 
		removeexport ea k2
	, check "check present export after remove" $ \ea _k1 k2 ->
		not <$> checkpresentexport ea k2
	, check "retrieve export fails after removal" $ \ea _k1 k2 ->
		not <$> retrieveexport ea k2
	, check "remove export directory" $ \ea _k1 _k2 ->
		removeexportdirectory ea
	, check "remove export directory that is already removed" $ \ea _k1 _k2 ->
		removeexportdirectory ea
	-- renames are not tested because remotes do not need to support them
	]
  where
	testexportdirectory = "testremote-export"
	testexportlocation = mkExportLocation (toRawFilePath (testexportdirectory </> "location"))
	check desc a = testCase desc $ do
		let a' = do
			ea <- Remote.exportActions <$> mkr
			k1 <- mkk1
			k2 <- mkk2
			a ea k1 k2
		runannex a' @? "failed"
	storeexport ea k = do
		loc <- fromRawFilePath <$> Annex.calcRepo (gitAnnexLocation k)
		Remote.storeExport ea loc k testexportlocation nullMeterUpdate
	retrieveexport ea k = withTmpFile "exported" $ \tmp h -> do
		liftIO $ hClose h
		ifM (Remote.retrieveExport ea k testexportlocation tmp nullMeterUpdate)
			( verifyKeyContent RetrievalAllKeysSecure AlwaysVerify UnVerified k tmp
			, return False
			)
	checkpresentexport ea k = Remote.checkPresentExport ea k testexportlocation
	removeexport ea k = Remote.removeExport ea k testexportlocation
	removeexportdirectory ea = case Remote.removeExportDirectory ea of
		Nothing -> return True
		Just a -> a (mkExportDirectory (toRawFilePath testexportdirectory))

testUnavailable :: RunAnnex -> Annex Remote -> Annex Key -> [TestTree]
testUnavailable runannex mkr mkk =
	[ check (== Right False) "removeKey" $ \r k ->
		Remote.removeKey r k
	, check (== Right False) "storeKey" $ \r k -> 
		Remote.storeKey r k (AssociatedFile Nothing) nullMeterUpdate
	, check (`notElem` [Right True, Right False]) "checkPresent" $ \r k ->
		Remote.checkPresent r k
	, check (== Right False) "retrieveKeyFile" $ \r k ->
		getViaTmp (Remote.retrievalSecurityPolicy r) (RemoteVerify r) k $ \dest ->
			Remote.retrieveKeyFile r k (AssociatedFile Nothing) dest nullMeterUpdate
	, check (== Right False) "retrieveKeyFileCheap" $ \r k ->
		getViaTmp (Remote.retrievalSecurityPolicy r) (RemoteVerify r) k $ \dest -> unVerified $
			Remote.retrieveKeyFileCheap r k (AssociatedFile Nothing) dest
	]
  where
	check checkval desc a = testCase desc $ do
		v <- runannex $ do
			r <- mkr
			k <- mkk
			either (Left  . show) Right <$> tryNonAsync (a r k)
		checkval v  @? ("(got: " ++ show v ++ ")")

cleanup :: [Remote] -> [Key] -> Bool -> CommandCleanup
cleanup rs ks ok
	| all Remote.readonly rs = return ok
	| otherwise = do
		forM_ rs $ \r -> forM_ ks (Remote.removeKey r)
		forM_ ks $ \k -> lockContentForRemoval k removeAnnex
		return ok

chunkSizes :: Int -> Bool -> [Int]
chunkSizes base False =
	[ 0 -- no chunking
	, base `div` 100
	, base `div` 1000
	, base
	]
chunkSizes _ True =
	[ 0
	]

keySizes :: Int -> Bool -> [Int]
keySizes base fast = filter want
	[ 0 -- empty key is a special case when chunking
	, base
	, base + 1
	, base - 1
	, base * 2
	]
  where
	want sz
		| fast = sz <= base && sz > 0
		| otherwise = sz > 0

randKey :: Int -> Annex Key
randKey sz = withTmpFile "randkey" $ \f h -> do
	gen <- liftIO (newGenIO :: IO SystemRandom)
	case genBytes sz gen of
		Left e -> error $ "failed to generate random key: " ++ show e
		Right (rand, _) -> liftIO $ B.hPut h rand
	liftIO $ hClose h
	let ks = KeySource
		{ keyFilename = toRawFilePath f
		, contentLocation = toRawFilePath f
		, inodeCache = Nothing
		}
	k <- fromMaybe (error "failed to generate random key")
		<$> Backend.getKey Backend.Hash.testKeyBackend ks nullMeterUpdate
	_ <- moveAnnex k f
	return k

getReadonlyKey :: Remote -> FilePath -> Annex Key
getReadonlyKey r f = lookupFile (toRawFilePath f) >>= \case
	Nothing -> giveup $ f ++ " is not an annexed file"
	Just k -> do
		unlessM (inAnnex k) $
			giveup $ f ++ " does not have its content locally present, cannot test it"
		unlessM ((Remote.uuid r `elem`) <$> loggedLocations k) $
			giveup $ f ++ " is not stored in the remote being tested, cannot test it"
		return k
