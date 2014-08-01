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
import Types
import Types.Key (key2file, keyBackendName, keySize)
import Types.Backend (getKey, fsckKey)
import Types.KeySource
import Annex.Content
import Backend
import qualified Backend.Hash
import Utility.Tmp
import Utility.Metered
import Messages
import Types.Messages

import Test.Tasty
import Test.Tasty.Runners
import Test.Tasty.HUnit
import "crypto-api" Crypto.Random
import qualified Data.ByteString as B

def :: [Command]
def = [ command "testremote" paramRemote seek SectionTesting
		"test transfers to/from a remote"]

seek :: CommandSeek
seek = withWords start

start :: [String] -> CommandStart
start ws = do
	let name = unwords ws
	showStart "testremote" name
	r <- either error id <$> Remote.byName' name
	showSideAction "generating test keys"
	ks <- testKeys
	next $ perform r ks

perform :: Remote -> [Key] -> CommandPerform
perform r ks = do
	st <- Annex.getState id
	let tests = testGroup "Remote Tests" $
		map (\k -> testGroup (descSize k) (testList st r k)) ks
	ok <- case tryIngredients [consoleTestReporter] mempty tests of
		Nothing -> error "No tests found!?"
		Just act -> liftIO act
	next $ cleanup r ks ok
  where
	descSize k = "key size " ++ show (keySize k)

testList :: Annex.AnnexState -> Remote -> Key -> [TestTree]
testList st r k =
	[ check "removeKey when not present" $
		Remote.removeKey r k
	, present False
	, check "storeKey" $
		Remote.storeKey r k Nothing nullMeterUpdate
	, present True
	, check "storeKey when already present" $
		Remote.storeKey r k Nothing nullMeterUpdate
	, present True
	, check "retrieveKeyFile" $ do
		removeAnnex k
		getViaTmp k $ \dest ->
			Remote.retrieveKeyFile r k Nothing dest nullMeterUpdate
	, check "fsck downloaded object" $ do
		case maybeLookupBackendName (keyBackendName k) of
			Nothing -> return True
			Just b -> case fsckKey b of
				Nothing -> return True
				Just fscker -> fscker k (key2file k)
	, check "removeKey when present" $
		Remote.removeKey r k
	, present False
	]
  where
	check desc a = testCase desc $
		Annex.eval st (Annex.setOutput QuietOutput >> a) @? "failed"
	present b = check ("present " ++ show b) $
		(== Right b) <$> Remote.hasKey r k

cleanup :: Remote -> [Key] -> Bool -> CommandCleanup
cleanup r ks ok = do
	forM_ ks (Remote.removeKey r)
	forM_ ks removeAnnex
	return ok

-- Generate random keys of several interesting sizes, assuming a chunk
-- size that is a uniform divisor of 1 MB.
testKeys :: Annex [Key]
testKeys = mapM randKey
	[ 0 -- empty key is a special case when chunking
	, mb
	, mb + 1
	, mb - 1
	, mb + mb
	]
  where
	mb = 1024 * 2014

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
