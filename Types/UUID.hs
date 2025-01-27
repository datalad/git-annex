{- git-annex UUID type
 -
 - Copyright 2011-2023 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, GeneralizedNewtypeDeriving #-}

module Types.UUID where

import qualified Data.ByteString as B
import qualified Data.ByteString.Short as SB
import qualified Data.Text as T
import qualified Data.Map as M
import qualified Data.UUID as U
import Data.Maybe
import Data.String
import Data.ByteString.Builder
import Control.DeepSeq
import qualified Data.Semigroup as Sem

import Git.Types (ConfigValue(..))
import Utility.FileSystemEncoding
import Utility.QuickCheck
import Utility.Aeson
import qualified Utility.SimpleProtocol as Proto

-- A UUID is either an arbitrary opaque string, or UUID info may be missing.
data UUID = NoUUID | UUID B.ByteString
	deriving (Eq, Ord, Show, Read)

instance NFData UUID where
	rnf NoUUID = ()
	rnf (UUID b) = rnf b

class FromUUID a where
	fromUUID :: UUID -> a

class ToUUID a where
	toUUID :: a -> UUID

instance FromUUID UUID where
	fromUUID = id

instance ToUUID UUID where
	toUUID = id

instance FromUUID B.ByteString where
	fromUUID (UUID u) = u
	fromUUID NoUUID = B.empty

instance ToUUID B.ByteString where
	toUUID b
		| B.null b = NoUUID
		| otherwise = UUID b

instance FromUUID SB.ShortByteString where
	fromUUID (UUID u) = SB.toShort u
	fromUUID NoUUID = SB.empty

instance ToUUID SB.ShortByteString where
	toUUID b
		| SB.null b = NoUUID
		| otherwise = UUID (SB.fromShort b)

instance FromUUID String where
	fromUUID s = decodeBS (fromUUID s)

instance ToUUID String where
	toUUID s = toUUID (encodeBS s)

instance FromUUID ConfigValue where
	fromUUID s = (ConfigValue (fromUUID s))

instance ToUUID ConfigValue where
	toUUID (ConfigValue v) = toUUID v
	toUUID NoConfigValue = NoUUID

-- There is no matching FromUUID U.UUID because a git-annex UUID may
-- be NoUUID or perhaps contain something not allowed in a canonical UUID.
instance ToUUID U.UUID where
	toUUID = toUUID . U.toASCIIBytes

instance ToJSON' UUID where
	toJSON' (UUID u) = toJSON' u
	toJSON' NoUUID = toJSON' ""

instance FromJSON UUID where
	parseJSON (String t)
		| isUUID s = pure (toUUID s)
		| otherwise = mempty
	  where
		s = T.unpack t
	parseJSON _ = mempty

buildUUID :: UUID -> Builder
buildUUID (UUID b) = byteString b
buildUUID NoUUID = mempty

isUUID :: String -> Bool
isUUID = isJust . U.fromString

-- A description of a UUID.
newtype UUIDDesc = UUIDDesc B.ByteString
	deriving (Eq, Sem.Semigroup, Monoid, IsString)

fromUUIDDesc :: UUIDDesc -> String
fromUUIDDesc (UUIDDesc d) = decodeBS d

toUUIDDesc :: String -> UUIDDesc
toUUIDDesc = UUIDDesc . encodeBS

type UUIDDescMap = M.Map UUID UUIDDesc

instance Proto.Serializable UUID where
	serialize = fromUUID
	deserialize = Just . toUUID

instance Arbitrary UUID where
	arbitrary = frequency [(1, return NoUUID), (3, UUID <$> arb)]
	  where
		arb = encodeBS <$> listOf1 (elements uuidchars)
		uuidchars = '-' : ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']
