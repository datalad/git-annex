{- git-annex command
 -
 - Copyright 2014 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Command.TestRemote where

import Common
import Command
import qualified Annex
import qualified Remote
import qualified Types.Remote as Remote
import Types
import Types.Key (key2file, keyBackendName, keySize)
import Types.Backend (getKey, fsckKey)
import Types.KeySource
import Annex.Content
import Backend
import qualified Backend.Hash
import Utility.Tmp
import Utility.Metered
import Utility.DataUnits
import Utility.CopyFile
import Messages
import Types.Messages
import Remote.Helper.Chunked
import Locations

import Test.Tasty
import Test.Tasty.Runners
import Test.Tasty.HUnit
import Control.Exception
import "crypto-api" Crypto.Random
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Map as M

def :: [Command]
def = [ withOptions [sizeOption] $
		command "testremote" paramRemote seek SectionTesting
			"test transfers to/from a remote"]

sizeOption :: Option
sizeOption = fieldOption [] "size" paramSize "base key size (default 1MiB)"

seek :: CommandSeek
seek ps = do
	basesz <- fromInteger . fromMaybe (1024 * 1024)
		<$> getOptionField sizeOption (pure . getsize)
	withWords (start basesz) ps
  where
	getsize v = v >>= readSize dataUnits

start :: Int -> [String] -> CommandStart
start basesz ws = do
	let name = unwords ws
	showStart "testremote" name
	r <- either error id <$> Remote.byName' name
	showSideAction "generating test keys"
	ks <- mapM randKey (keySizes basesz)
	rs <- catMaybes <$> mapM (adjustChunkSize r) (chunkSizes basesz)
	next $ perform rs ks

perform :: [Remote] -> [Key] -> CommandPerform
perform rs ks = do
	st <- Annex.getState id
	let tests = testGroup "Remote Tests" $
		[ testGroup (desc r k) (test st r k) | k <- ks, r <- rs ]
	ok <- case tryIngredients [consoleTestReporter] mempty tests of
		Nothing -> error "No tests found!?"
		Just act -> liftIO act
	next $ cleanup rs ks ok
  where
	desc r' k = unwords
		[ "key size"
		, show (keySize k)
		, "chunk size"
		, show (chunkConfig (Remote.config r'))
		]

-- To adjust a Remote to use a new chunk size, have to re-generate it with
-- a modified config.
adjustChunkSize :: Remote -> Int -> Annex (Maybe Remote)
adjustChunkSize r chunksize = Remote.generate (Remote.remotetype r)
	(Remote.repo r)
	(Remote.uuid r)
	(M.insert "chunk" (show chunksize) (Remote.config r))
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
		removeAnnex k
		get
	, check "fsck downloaded object" fsck
	, check "retrieveKeyFile resume from 33%" $ do
		loc <- Annex.calcRepo (gitAnnexLocation k)
		tmp <- prepTmp k
		partial <- liftIO $ bracket (openBinaryFile loc ReadMode) hClose $ \h -> do
			sz <- hFileSize h
			L.hGet h $ fromInteger $ sz `div` 3
		liftIO $ L.writeFile tmp partial
		removeAnnex k
		get
	, check "fsck downloaded object" fsck
	, check "retrieveKeyFile resume from 0" $ do
		tmp <- prepTmp k
		liftIO $ writeFile tmp ""
		removeAnnex k
		get
	, check "fsck downloaded object" fsck
	, check "retrieveKeyFile resume from end" $ do
		loc <- Annex.calcRepo (gitAnnexLocation k)
		tmp <- prepTmp k
		void $ liftIO $ copyFileExternal loc tmp
		removeAnnex k
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
		Just b -> case fsckKey b of
			Nothing -> return True
			Just fscker -> fscker k (key2file k)
	get = getViaTmp k $ \dest ->
		Remote.retrieveKeyFile r k Nothing dest nullMeterUpdate
	store = Remote.storeKey r k Nothing nullMeterUpdate
	remove = Remote.removeKey r k

cleanup :: [Remote] -> [Key] -> Bool -> CommandCleanup
cleanup rs ks ok = do
	forM_ rs $ \r -> forM_ ks (Remote.removeKey r)
	forM_ ks removeAnnex
	return ok

chunkSizes :: Int -> [Int]
chunkSizes base = 
	[ 0 -- no chunking
	, base `div` 100
	, base `div` 1000
	, base
	]

keySizes :: Int -> [Int]
keySizes base = filter (>= 0)
	[ 0 -- empty key is a special case when chunking
	, base
	, base + 1
	, base - 1
	, base * 2
	]

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
