{- git-annex Keys
 -
 - Copyright 2011-2019 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Key (
	Key(..),
	AssociatedFile(..),
	stubKey,
	buildKey,
	keyParser,
	serializeKey,
	serializeKey',
	deserializeKey,
	deserializeKey',
	nonChunkKey,
	chunkKeyOffset,
	isChunkKey,
	isKeyPrefix,
	splitKeyNameExtension,

	prop_isomorphic_key_encode
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

{- Builds a ByteString from a Key.
 -
 - The name field is always shown last, separated by doubled fieldSeps,
 - and is the only field allowed to contain the fieldSep.
 -}
buildKey :: Key -> Builder
buildKey k = byteString (formatKeyVariety (keyVariety k))
	<> 's' ?: (integerDec <$> keySize k)
	<> 'm' ?: (integerDec . (\(CTime t) -> fromIntegral t) <$> keyMtime k)
	<> 'S' ?: (integerDec <$> keyChunkSize k)
	<> 'C' ?: (integerDec <$> keyChunkNum k)
	<> sepbefore (sepbefore (byteString (keyName k)))
  where
	sepbefore s = char7 fieldSep <> s
	c ?: (Just b) = sepbefore (char7 c <> b)
	_ ?: Nothing = mempty

serializeKey :: Key -> String
serializeKey = decodeBL' . serializeKey'

serializeKey' :: Key -> L.ByteString
serializeKey' = toLazyByteStringWith (safeStrategy 128 smallChunkSize) L.empty . buildKey

{- This is a strict parser for security reasons; a key
 - can contain only 4 fields, which all consist only of numbers.
 - Any key containing other fields, or non-numeric data will fail
 - to parse.
 -
 - If a key contained non-numeric fields, they could be used to
 - embed data used in a SHA1 collision attack, which would be a
 - problem since the keys are committed to git.
 -}
keyParser :: A.Parser Key
keyParser = do
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

deserializeKey :: String -> Maybe Key
deserializeKey = deserializeKey' . encodeBS'

deserializeKey' :: S.ByteString -> Maybe Key
deserializeKey' = eitherToMaybe . A.parseOnly keyParser

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
 - characters, but they may be unicode which could use up to 4 bytes each,
 - so 32 bytes. 64 bytes is used here to give a little future wiggle-room. 
 - The SHA1 common-prefix attack needs 128 bytes of data.
 -}
validKeyName :: KeyVariety -> S.ByteString -> Bool
validKeyName kv name
	| hasExt kv = 
		let ext = snd $ splitKeyNameExtension' name
		in S.length ext <= 64
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
	hashIO32 = hashIO32 . serializeKey'
	hashIO64 = hashIO64 . serializeKey'

instance ToJSON' Key where
	toJSON' = toJSON' . serializeKey

instance FromJSON Key where
	parseJSON (String t) = maybe mempty pure $ deserializeKey $ T.unpack t
	parseJSON _ = mempty

instance Proto.Serializable Key where
	serialize = serializeKey
	deserialize = deserializeKey

prop_isomorphic_key_encode :: Key -> Bool
prop_isomorphic_key_encode k = Just k == (deserializeKey . serializeKey) k
