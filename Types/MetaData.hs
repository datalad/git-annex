{- git-annex general metadata
 -
 - Copyright 2014 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types.MetaData (
	MetaData,
	MetaField,
	MetaValue,
	CurrentlySet(..),
	MetaSerializable,
	toMetaField,
	fromMetaField,
	toMetaValue,
	toMetaValue',
	fromMetaValue,
	newMetaData,
	updateMetaData,
	getCurrentMetaData,
	getAllMetaData,
	serialize,
	deserialize,
	prop_updateMetaData_sane,
	prop_metadata_serialize
) where

import Common
import Utility.Base64
import Utility.QuickCheck

import qualified Data.Set as S
import qualified Data.Map as M
import Data.Char

newtype MetaData = MetaData (M.Map MetaField (S.Set MetaValue))
	deriving (Show, Eq)

{- A metadata value can be currently be set (True), or may have been
 - set before and we're remembering it no longer is (False). -}
newtype CurrentlySet = CurrentlySet Bool
	deriving (Show, Eq, Ord, Arbitrary)

newtype MetaField = MetaField String
	deriving (Show, Eq, Ord)

data MetaValue = MetaValue CurrentlySet String
	deriving (Show, Ord)

{- Metadata values are compared equal whether currently set or not. -}
instance Eq MetaValue where
	MetaValue _ a == MetaValue _ b = a == b

{- MetaData is serialized to a format like:
 -
 - field1 +val1 +val2 -val3 field2 +val4 +val5
 -}
class MetaSerializable v where
	serialize :: v -> String
	deserialize :: String -> Maybe v

instance MetaSerializable MetaData where
	serialize (MetaData m) = unwords $ concatMap go $ M.toList m
	  where
		go (f, vs) = serialize f : map serialize (S.toList vs)
	deserialize = Just . getfield newMetaData . words
	  where
		getfield m [] = m
		getfield m (w:ws) = maybe m (getvalues m ws) (deserialize w)
		getvalues m [] _ = m
		getvalues m l@(w:ws) f = case deserialize w of
			Just v -> getvalues (updateMetaData f v m) ws f
			Nothing -> getfield m l

instance MetaSerializable MetaField where
	serialize (MetaField f) = f
	deserialize = Just . MetaField

{- Base64 problimatic values. -}
instance MetaSerializable MetaValue where
	serialize (MetaValue isset v) =
		serialize isset ++
		if any isSpace v || "!" `isPrefixOf` v
			then '!' : toB64 v
			else v
	deserialize (isset:'!':v) = MetaValue
		<$> deserialize [isset]
		<*> fromB64Maybe v
	deserialize (isset:v) = MetaValue 
		<$> deserialize [isset]
		<*> pure v
	deserialize [] = Nothing

instance MetaSerializable CurrentlySet where
	serialize (CurrentlySet True) = "+"
	serialize (CurrentlySet False) = "-"
	deserialize "+" = Just (CurrentlySet True)
	deserialize "-" = Just (CurrentlySet False)
	deserialize _ = Nothing

{- Fields cannot be empty, contain whitespace, or start with "+-" as
 - that would break the serialization. -}
toMetaField :: String -> Maybe MetaField
toMetaField f
	| legalField f = Just $ MetaField f
	| otherwise = Nothing

legalField :: String -> Bool
legalField f
	| null f = False
	| any isSpace f = False
	| any (`isPrefixOf` f) ["+", "-"] = False
	| otherwise = True

toMetaValue :: String -> MetaValue
toMetaValue = MetaValue (CurrentlySet True)

toMetaValue' :: CurrentlySet -> String -> MetaValue
toMetaValue' = MetaValue

fromMetaField :: MetaField -> String
fromMetaField (MetaField f) = f

fromMetaValue :: MetaValue -> String
fromMetaValue (MetaValue _ f) = f

newMetaData :: MetaData
newMetaData = MetaData M.empty

{- Can be used to set a value, or to unset it, depending on whether
 - the MetaValue has CurrentlySet or not. -}
updateMetaData :: MetaField -> MetaValue -> MetaData -> MetaData
updateMetaData f v (MetaData m) = MetaData $
	M.insertWith' S.union f (S.singleton v) m

{- Gets only currently set values -}
getCurrentMetaData :: MetaField -> MetaData -> S.Set MetaValue
getCurrentMetaData f m = S.filter isSet (getAllMetaData f m)

isSet :: MetaValue -> Bool
isSet (MetaValue (CurrentlySet isset) _) = isset

{- Gets currently set values, but also values that have been unset. -}
getAllMetaData :: MetaField -> MetaData -> S.Set MetaValue
getAllMetaData f (MetaData m) = fromMaybe S.empty (M.lookup f m)

{- Avoid putting too many fields in the map; extremely large maps make
 - the seriaization test slow due to the sheer amount of data.
 - It's unlikely that more than 100 fields of metadata will be used. -}
instance Arbitrary MetaData where
	arbitrary = do
		size <- arbitrarySizedBoundedIntegral `suchThat` (< 500)
		MetaData . M.fromList <$> vector size

instance Arbitrary MetaValue where
	arbitrary = MetaValue <$> arbitrary <*> arbitrary

instance Arbitrary MetaField where
	arbitrary = MetaField <$> arbitrary `suchThat` legalField 

prop_updateMetaData_sane :: MetaData -> MetaField -> MetaValue -> Bool
prop_updateMetaData_sane m f v = and
	[ S.member v $ getAllMetaData f m'
	, not (isSet v) || S.member v (getCurrentMetaData f m')
	]
  where
	m' = updateMetaData f v m

prop_metadata_serialize :: MetaField -> MetaValue -> MetaData -> Bool
prop_metadata_serialize f v m = and
	[ deserialize (serialize f) == Just f
	, deserialize (serialize v) == Just v
	, deserialize (serialize m') == Just m'
	]
  where
  	m' = removeemptyfields m
	removeemptyfields (MetaData x) = MetaData $ M.filter (not . S.null) x
