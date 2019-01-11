{- git-annex Keys
 -
 - Copyright 2011-2019 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Key (
	Key(..),
	AssociatedFile(..),
	stubKey,
	buildKeyFile,
	keyFileParser,
	file2key,
	key2file,
	file2key',
	key2file',
	nonChunkKey,
	chunkKeyOffset,
	isChunkKey,
	isKeyPrefix,
	splitKeyNameExtension,

	prop_isomorphic_key_encode,
	prop_isomorphic_key_decode
) where

import qualified Data.Text as T
import qualified Data.ByteString as S
import qualified Data.ByteString.Char8 as S8
import qualified Data.ByteString.Lazy as L
import Data.ByteString.Builder
import Data.ByteString.Builder.Extra
import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.ByteString.Char8 as A8
import Foreign.C.Types

import Common
import Types.Key
import Utility.QuickCheck
import Utility.Bloom
import Utility.Aeson
import qualified Utility.SimpleProtocol as Proto

stubKey :: Key
stubKey = Key
	{ keyName = mempty
	, keyVariety = OtherKey mempty
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

{- Builds a ByteString that is suitable for use as a filename representing
 - a Key. The name field is always shown last, separated by doubled fieldSeps,
 - and is the only field allowed to contain the fieldSep. -}
buildKeyFile :: Key -> Builder
buildKeyFile k = byteString (formatKeyVariety (keyVariety k))
	<> 's' ?: (integerDec <$> keySize k)
	<> 'm' ?: (integerDec . (\(CTime t) -> fromIntegral t) <$> keyMtime k)
	<> 'S' ?: (integerDec <$> keyChunkSize k)
	<> 'C' ?: (integerDec <$> keyChunkNum k)
	<> sepbefore (sepbefore (byteString (keyName k)))
  where
	sepbefore s = char7 fieldSep <> s
	c ?: (Just b) = sepbefore (char7 c <> b)
	_ ?: Nothing = mempty

key2file :: Key -> FilePath
key2file = decodeBL' . key2file'

key2file' :: Key -> L.ByteString
key2file' = toLazyByteStringWith (safeStrategy 128 smallChunkSize) L.empty . buildKeyFile

{- This is a strict parser for security reasons; a key
 - can contain only 4 fields, which all consist only of numbers.
 - Any key containing other fields, or non-numeric data will fail
 - to parse.
 -
 - If a key contained non-numeric fields, they could be used to
 - embed data used in a SHA1 collision attack, which would be a
 - problem since the keys are committed to git.
 -}
keyFileParser :: A.Parser Key
keyFileParser = do
	-- key variety cannot be empty
	v <- (parseKeyVariety <$> A8.takeWhile1 (/= fieldSep))
	s <- parsesize
	m <- parsemtime
	cs <- parsechunksize
	cn <- parsechunknum
	_ <- A8.char fieldSep
	_ <- A8.char fieldSep
	n <- A.takeByteString
	if validKeyName v n
		then return $ Key
			{ keyName = n
			, keyVariety = v
			, keySize = s
			, keyMtime = m
			, keyChunkSize = cs
			, keyChunkNum = cn
			}
		else fail "invalid keyName"
  where
	parseopt p = (Just <$> (A8.char fieldSep *> p)) <|> pure Nothing
	parsesize = parseopt $ A8.char 's' *> A8.decimal
	parsemtime = parseopt $ CTime <$> (A8.char 'm' *> A8.decimal)
	parsechunksize = parseopt $ A8.char 'S' *> A8.decimal
	parsechunknum = parseopt $ A8.char 'C' *> A8.decimal

file2key :: FilePath -> Maybe Key
file2key = file2key' . encodeBS'

file2key' :: S.ByteString -> Maybe Key
file2key' b = eitherToMaybe $ A.parseOnly keyFileParser b

{- This splits any extension out of the keyName, returning the 
 - keyName minus extension, and the extension (including leading dot).
 -}
splitKeyNameExtension :: Key -> (S.ByteString, S.ByteString)
splitKeyNameExtension = splitKeyNameExtension' . keyName

splitKeyNameExtension' :: S.ByteString -> (S.ByteString, S.ByteString)
splitKeyNameExtension' keyname = S8.span (/= '.') keyname

{- Limits the length of the extension in the keyName to mitigate against
 - SHA1 collision attacks.
 -
 - In such an attack, the extension of the key could be made to contain
 - the collision generation data, with the result that a signed git commit
 - including such keys would not be secure.
 -
 - The maximum extension length ever generated for such a key was 8
 - characters; 20 is used here to give a little future wiggle-room. 
 - The SHA1 common-prefix attack needs 128 bytes of data.
 -}
validKeyName :: KeyVariety -> S.ByteString -> Bool
validKeyName kv name
	| hasExt kv = 
		let ext = snd $ splitKeyNameExtension' name
		in S.length ext <= 20
	| otherwise = True

instance Arbitrary Key where
	arbitrary = Key
		<$> (encodeBS <$> (listOf1 $ elements $ ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "-_\r\n \t"))
		<*> (parseKeyVariety . encodeBS <$> (listOf1 $ elements ['A'..'Z'])) -- BACKEND
		<*> ((abs <$>) <$> arbitrary) -- size cannot be negative
		<*> ((abs . fromInteger <$>) <$> arbitrary) -- mtime cannot be negative
		<*> ((abs <$>) <$> arbitrary) -- chunksize cannot be negative
		<*> ((succ . abs <$>) <$> arbitrary) -- chunknum cannot be 0 or negative

instance Hashable Key where
	hashIO32 = hashIO32 . key2file'
	hashIO64 = hashIO64 . key2file'

instance ToJSON' Key where
	toJSON' = toJSON' . key2file

instance FromJSON Key where
	parseJSON (String t) = maybe mempty pure $ file2key $ T.unpack t
	parseJSON _ = mempty

instance Proto.Serializable Key where
	serialize = key2file
	deserialize = file2key

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
