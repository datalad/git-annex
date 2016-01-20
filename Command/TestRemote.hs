{- git-annex command
 -
 - Copyright 2014 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.TestRemote where

import Command
import qualified Annex
import qualified Remote
import qualified Types.Remote as Remote
import Types.Backend (getKey, verifyKeyContent)
import Types.KeySource
import Annex.Content
import Backend
import qualified Backend.Hash
import Utility.Tmp
import Utility.Metered
import Utility.DataUnits
import Utility.CopyFile
import Types.Messages
import Remote.Helper.Chunked
import Git.Types

import Test.Tasty
import Test.Tasty.Runners
import Test.Tasty.HUnit
import "crypto-api" Crypto.Random
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Map as M

cmd :: Command
cmd = command "testremote" SectionTesting
	"test transfers to/from a remote"
	paramRemote (seek <$$> optParser)

data TestRemoteOptions = TestRemoteOptions
	{ testRemote :: RemoteName
	, sizeOption :: ByteSize
	}

optParser :: CmdParamsDesc -> Parser TestRemoteOptions
optParser desc = TestRemoteOptions
	<$> argument str ( metavar desc )
	<*> option (str >>= maybe (fail "parse error") return . readSize dataUnits)
		( long "size" <> metavar paramSize
		<> value (1024 * 1024)
		<> help "base key size (default 1MiB)"
		)

seek :: TestRemoteOptions -> CommandSeek
seek o = commandAction $ start (fromInteger $ sizeOption o) (testRemote o) 

start :: Int -> RemoteName -> CommandStart
start basesz name = do
	showStart "testremote" name
	r <- either error id <$> Remote.byName' name
	showAction "generating test keys"
	fast <- Annex.getState Annex.fast
	ks <- mapM randKey (keySizes basesz fast)
	rs <- catMaybes <$> mapM (adjustChunkSize r) (chunkSizes basesz fast)
	rs' <- concat <$> mapM encryptionVariants rs
	unavailrs  <- catMaybes <$> mapM Remote.mkUnavailable [r]
	next $ perform rs' unavailrs ks

perform :: [Remote] -> [Remote] -> [Key] -> CommandPerform
perform rs unavailrs ks = do
	st <- Annex.getState id
	let tests = testGroup "Remote Tests" $ concat
		[ [ testGroup "unavailable remote" (testUnavailable st r (Prelude.head ks)) | r <- unavailrs ]
		, [ testGroup (desc r k) (test st r k) | k <- ks, r <- rs ]
		]
	ok <- case tryIngredients [consoleTestReporter] mempty tests of
		Nothing -> error "No tests found!?"
		Just act -> liftIO act
	next $ cleanup rs ks ok
  where
	desc r' k = intercalate "; " $ map unwords
		[ [ "key size", show (keySize k) ]
		, [ show (getChunkConfig (Remote.config r')) ]
		, ["encryption", fromMaybe "none" (M.lookup "encryption" (Remote.config r'))]
		]

adjustChunkSize :: Remote -> Int -> Annex (Maybe Remote)
adjustChunkSize r chunksize = adjustRemoteConfig r
	(M.insert "chunk" (show chunksize))

-- Variants of a remote with no encryption, and with simple shared
-- encryption. Gpg key based encryption is not tested.
encryptionVariants :: Remote -> Annex [Remote]
encryptionVariants r = do
	noenc <- adjustRemoteConfig r (M.insert "encryption" "none")
	sharedenc <- adjustRemoteConfig r $
		M.insert "encryption" "shared" .
		M.insert "highRandomQuality" "false"
	return $ catMaybes [noenc, sharedenc]

-- Regenerate a remote with a modified config.
adjustRemoteConfig :: Remote -> (Remote.RemoteConfig -> Remote.RemoteConfig) -> Annex (Maybe Remote)
adjustRemoteConfig r adjustconfig = Remote.generate (Remote.remotetype r)
	(Remote.repo r)
	(Remote.uuid r)
	(adjustconfig (Remote.config r))
	(Remote.gitconfig r)

test :: Annex.AnnexState -> Remote -> Key -> [TestTree]
test st r k =
	[ check "removeKey when not present" remove
	, present False
	, check "storeKey" store
	, present True
	, check "storeKey when already present" store
	, present True
	, check "retrieveKeyFile" $ do
		lockContentForRemoval k removeAnnex
		get
	, check "fsck downloaded object" fsck
	, check "retrieveKeyFile resume from 33%" $ do
		loc <- Annex.calcRepo (gitAnnexLocation k)
		tmp <- prepTmp k
		partial <- liftIO $ bracket (openBinaryFile loc ReadMode) hClose $ \h -> do
			sz <- hFileSize h
			L.hGet h $ fromInteger $ sz `div` 3
		liftIO $ L.writeFile tmp partial
		lockContentForRemoval k removeAnnex
		get
	, check "fsck downloaded object" fsck
	, check "retrieveKeyFile resume from 0" $ do
		tmp <- prepTmp k
		liftIO $ writeFile tmp ""
		lockContentForRemoval k removeAnnex
		get
	, check "fsck downloaded object" fsck
	, check "retrieveKeyFile resume from end" $ do
		loc <- Annex.calcRepo (gitAnnexLocation k)
		tmp <- prepTmp k
		void $ liftIO $ copyFileExternal CopyAllMetaData loc tmp
		lockContentForRemoval k removeAnnex
		get
	, check "fsck downloaded object" fsck
	, check "removeKey when present" remove
	, present False
	]
  where
	check desc a = testCase desc $
		Annex.eval st (Annex.setOutput QuietOutput >> a) @? "failed"
	present b = check ("present " ++ show b) $
		(== Right b) <$> Remote.hasKey r k
	fsck = case maybeLookupBackendName (keyBackendName k) of
		Nothing -> return True
		Just b -> case verifyKeyContent b of
			Nothing -> return True
			Just verifier -> verifier k (key2file k)
	get = getViaTmp (RemoteVerify r) k $ \dest ->
		Remote.retrieveKeyFile r k Nothing dest nullMeterUpdate
	store = Remote.storeKey r k Nothing nullMeterUpdate
	remove = Remote.removeKey r k

testUnavailable :: Annex.AnnexState -> Remote -> Key -> [TestTree]
testUnavailable st r k =
	[ check (== Right False) "removeKey" $
		Remote.removeKey r k
	, check (== Right False) "storeKey" $
		Remote.storeKey r k Nothing nullMeterUpdate
	, check (`notElem` [Right True, Right False]) "checkPresent" $
		Remote.checkPresent r k
	, check (== Right False) "retrieveKeyFile" $
		getViaTmp (RemoteVerify r) k $ \dest ->
			Remote.retrieveKeyFile r k Nothing dest nullMeterUpdate
	, check (== Right False) "retrieveKeyFileCheap" $
		getViaTmp (RemoteVerify r) k $ \dest -> unVerified $
			Remote.retrieveKeyFileCheap r k Nothing dest
	]
  where
	check checkval desc a = testCase desc $ do
		v <- Annex.eval st $ do
			Annex.setOutput QuietOutput
			either (Left  . show) Right <$> tryNonAsync a
		checkval v  @? ("(got: " ++ show v ++ ")")

cleanup :: [Remote] -> [Key] -> Bool -> CommandCleanup
cleanup rs ks ok = do
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
		{ keyFilename = f
		, contentLocation = f
		, inodeCache = Nothing
		}
	k <- fromMaybe (error "failed to generate random key")
		<$> getKey Backend.Hash.testKeyBackend ks
	moveAnnex k f
	return k
