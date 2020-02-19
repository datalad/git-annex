{- git-annex Keys
 -
 - Copyright 2011-2020 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Key (
	Key,
	KeyData(..),
	AssociatedFile(..),
	fromKey,
	mkKey,
	alterKey,
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

import Data.Char
import qualified Data.Text as T
import qualified Data.ByteString as S
import qualified Data.Attoparsec.ByteString as A

import Common
import Types.Key
import Utility.QuickCheck
import Utility.Bloom
import Utility.Aeson
import qualified Utility.SimpleProtocol as Proto

-- Gets the parent of a chunk key.
nonChunkKey :: Key -> Key
nonChunkKey k
	| fromKey keyChunkSize k == Nothing && fromKey keyChunkNum k == Nothing = k
	| otherwise = alterKey k $ \d -> d
		{ keyChunkSize = Nothing
		, keyChunkNum = Nothing
		}

-- Where a chunk key is offset within its parent.
chunkKeyOffset :: Key -> Maybe Integer
chunkKeyOffset k = (*)
	<$> fromKey keyChunkSize k
	<*> (pred <$> fromKey keyChunkNum k)

isChunkKey :: Key -> Bool
isChunkKey k = isJust (fromKey keyChunkSize k) && isJust (fromKey keyChunkNum k)

serializeKey :: Key -> String
serializeKey = decodeBS' . serializeKey'

serializeKey' :: Key -> S.ByteString
serializeKey' = keySerialization

deserializeKey :: String -> Maybe Key
deserializeKey = deserializeKey' . encodeBS'

deserializeKey' :: S.ByteString -> Maybe Key
deserializeKey' = eitherToMaybe . A.parseOnly keyParser

instance Arbitrary KeyData where
	arbitrary = Key
		<$> (encodeBS <$> (listOf1 $ elements $ ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9'] ++ "-_\r\n \t"))
		<*> (parseKeyVariety . encodeBS <$> (listOf1 $ elements ['A'..'Z'])) -- BACKEND
		<*> ((abs <$>) <$> arbitrary) -- size cannot be negative
		<*> ((abs . fromInteger <$>) <$> arbitrary) -- mtime cannot be negative
		<*> ((abs <$>) <$> arbitrary) -- chunksize cannot be negative
		<*> ((succ . abs <$>) <$> arbitrary) -- chunknum cannot be 0 or negative

-- AssociatedFile cannot be empty, and cannot contain a NUL
-- (but can be Nothing). 
instance Arbitrary AssociatedFile where
  	arbitrary = (AssociatedFile . fmap conv <$> arbitrary)
		`suchThat` (/= AssociatedFile (Just S.empty))
		`suchThat` (\(AssociatedFile f) -> maybe True (S.notElem 0) f)
	  where
		-- Generating arbitrary unicode leads to encoding errors
		-- when LANG=C, so limit to ascii.
		conv = toRawFilePath . filter isAscii

instance Arbitrary Key where
	arbitrary = mkKey . const <$> arbitrary

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

