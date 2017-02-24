{- git-annex Key data type
 -
 - Copyright 2011-2016 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Types.Key (
	Key(..),
	AssociatedFile,
	stubKey,
	key2file,
	file2key,
	nonChunkKey,
	chunkKeyOffset,
	isChunkKey,
	isKeyPrefix,

	prop_isomorphic_key_encode,
	prop_isomorphic_key_decode
) where

import System.Posix.Types
import Data.Aeson
import Data.Char
import qualified Data.Text as T

import Common
import Utility.QuickCheck
import Utility.Bloom
import qualified Utility.SimpleProtocol as Proto

{- A Key has a unique name, which is derived from a particular backend,
 - and may contain other optional metadata. -}
data Key = Key
	{ keyName :: String
	, keyBackendName :: String
	, keySize :: Maybe Integer
	, keyMtime :: Maybe EpochTime
	, keyChunkSize :: Maybe Integer
	, keyChunkNum :: Maybe Integer
	} deriving (Eq, Ord, Read, Show)

{- A filename may be associated with a Key. -}
type AssociatedFile = Maybe FilePath

stubKey :: Key
stubKey = Key
	{ keyName = ""
	, keyBackendName = ""
	, keySize = Nothing
	, keyMtime = Nothing
	, keyChunkSize = Nothing
	, keyChunkNum = Nothing
	}

-- Gets the parent of a chunk key.
nonChunkKey :: Key -> Key
nonChunkKey k = k
	{ keyChunkSize = Nothing
	, keyChunkNum = Nothing
	}

-- Where a chunk key is offset within its parent.
chunkKeyOffset :: Key -> Maybe Integer
chunkKeyOffset k = (*)
	<$> keyChunkSize k
	<*> (pred <$> keyChunkNum k)

isChunkKey :: Key -> Bool
isChunkKey k = isJust (keyChunkSize k) && isJust (keyChunkNum k)

-- Checks if a string looks like at least the start of a key.
isKeyPrefix :: String -> Bool
isKeyPrefix s = [fieldSep, fieldSep] `isInfixOf` s

fieldSep :: Char
fieldSep = '-'

{- Converts a key to a string that is suitable for use as a filename.
 - The name field is always shown last, separated by doubled fieldSeps,
 - and is the only field allowed to contain the fieldSep. -}
key2file :: Key -> FilePath
key2file Key { keyBackendName = b, keySize = s, keyMtime = m, keyChunkSize = cs, keyChunkNum = cn, keyName = n } =
	b +++ ('s' ?: s) +++ ('m' ?: m) +++ ('S' ?: cs) +++ ('C' ?: cn) +++ (fieldSep : n)
  where
	"" +++ y = y
	x +++ "" = x
	x +++ y = x ++ fieldSep:y
	f ?: (Just v) = f : show v
	_ ?: _ = ""

file2key :: FilePath -> Maybe Key
file2key s
	| key == Just stubKey || (keyName <$> key) == Just "" || (keyBackendName <$> key) == Just "" = Nothing
	| otherwise = key
  where
	key = startbackend stubKey s

	startbackend k v = sepfield k v addbackend
		
	sepfield k v a = case span (/= fieldSep) v of
		(v', _:r) -> findfields r $ a k v'
		_ -> Nothing

	findfields (c:v) (Just k)
		| c == fieldSep = Just $ k { keyName = v }
		| otherwise = sepfield k v $ addfield c
	findfields _ v = v

	addbackend k v = Just k { keyBackendName = v }

	-- This is a strict parser for security reasons; a key
	-- can contain only 4 fields, which all consist only of numbers.
	-- Any key containing other fields, or non-numeric data is
	-- rejected with Nothing.
	--
	-- If a key contained non-numeric fields, they could be used to
	-- embed data used in a SHA1 collision attack, which would be a
	-- problem since the keys are committed to git.
	addfield _ _ v | not (all isDigit v) = Nothing
	addfield 's' k v = do
		sz <- readish v
		return $ k { keySize = Just sz }
	addfield 'm' k v = do
		mtime <- readish v
		return $ k { keyMtime = Just mtime }
	addfield 'S' k v = do
		chunksize <- readish v
		return $ k { keyChunkSize = Just chunksize }
	addfield 'C' k v = case readish v of
		Just chunknum | chunknum > 0 ->
			return $ k { keyChunkNum = Just chunknum }
		_ -> Nothing
	addfield _ _ _ = Nothing

instance ToJSON Key where
	toJSON = toJSON . key2file

instance FromJSON Key where
	parseJSON (String t) = maybe mempty pure $ file2key $ T.unpack t
	parseJSON _ = mempty

instance Proto.Serializable Key where
	serialize = key2file
	deserialize = file2key

instance Arbitrary Key where
	arbitrary = Key
		<$> (listOf1 $ elements $ ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "-_\r\n \t")
		<*> (listOf1 $ elements ['A'..'Z']) -- BACKEND
		<*> ((abs <$>) <$> arbitrary) -- size cannot be negative
		<*> arbitrary
		<*> ((abs <$>) <$> arbitrary) -- chunksize cannot be negative
		<*> ((succ . abs <$>) <$> arbitrary) -- chunknum cannot be 0 or negative

instance Hashable Key where
	hashIO32 = hashIO32 . key2file
	hashIO64 = hashIO64 . key2file

prop_isomorphic_key_encode :: Key -> Bool
prop_isomorphic_key_encode k = Just k == (file2key . key2file) k

prop_isomorphic_key_decode :: FilePath -> Bool
prop_isomorphic_key_decode f
	| normalfieldorder = maybe True (\k -> key2file k == f) (file2key f)
	| otherwise = True
  where
	-- file2key will accept the fields in any order, so don't
	-- try the test unless the fields are in the normal order
	normalfieldorder = fields `isPrefixOf` "smSC"
	fields = map (f !!) $ filter (< length f) $ map succ $
		elemIndices fieldSep f
