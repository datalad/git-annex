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
	mkMetaValue,
	unsetMetaValue,
	fromMetaValue,
	fromMetaData,
	newMetaData,
	updateMetaData,
	unionMetaData,
	hasUniqueMetaData,
	currentMetaData,
	currentMetaDataValues,
	getAllMetaData,
	serialize,
	deserialize,
	prop_metadata_sane,
	prop_metadata_serialize
) where

import Common
import Utility.Base64
import Utility.QuickCheck

import qualified Data.Set as S
import qualified Data.Map as M
import Data.Char

newtype MetaData = MetaData (M.Map MetaField (S.Set MetaValue))
	deriving (Show, Eq, Ord)

{- A metadata value can be currently be set (True), or may have been
 - set before and we're remembering it no longer is (False). -}
newtype CurrentlySet = CurrentlySet Bool
	deriving (Show, Eq, Ord, Arbitrary)

newtype MetaField = MetaField String
	deriving (Show, Eq, Ord)

data MetaValue = MetaValue CurrentlySet String
	deriving (Show)

{- Metadata values compare and order the same whether currently set or not. -}
instance Eq MetaValue where
	MetaValue _ a == MetaValue _ b = a == b
instance Ord MetaValue where
	compare (MetaValue _ x) (MetaValue _ y) = compare x y

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

mkMetaValue :: CurrentlySet -> String -> MetaValue
mkMetaValue = MetaValue

unsetMetaValue :: MetaValue -> MetaValue
unsetMetaValue (MetaValue _ s) = MetaValue (CurrentlySet False) s

fromMetaField :: MetaField -> String
fromMetaField (MetaField f) = f

fromMetaValue :: MetaValue -> String
fromMetaValue (MetaValue _ f) = f

fromMetaData :: MetaData -> [(MetaField, S.Set MetaValue)]
fromMetaData (MetaData m) = M.toList m

newMetaData :: MetaData
newMetaData = MetaData M.empty

{- Can be used to set a value, or to unset it, depending on whether
 - the MetaValue has CurrentlySet or not. -}
updateMetaData :: MetaField -> MetaValue -> MetaData -> MetaData
updateMetaData f v (MetaData m) = MetaData $
	M.insertWith' S.union f (S.singleton v) m

{- New metadata overrides old._-}
unionMetaData :: MetaData -> MetaData -> MetaData
unionMetaData (MetaData old) (MetaData new) = MetaData $
	M.unionWith S.union new old

{- Checks if m contains any fields with values that are not
 - the same in comparewith. Note that unset and set values are
 - considered to be the same, so if m sets a value and comparewith
 - unsets it, m is not unique. However, if m changes the value,
 - or adds a new value, it is unique. -}
hasUniqueMetaData :: MetaData -> MetaData -> Bool
hasUniqueMetaData (MetaData comparewith) (MetaData m) =
	any uniquefield (M.toList m)
  where
  	uniquefield :: (MetaField, S.Set MetaValue) -> Bool
	uniquefield (f, v) = maybe True (uniquevalue v) (M.lookup f comparewith)
	uniquevalue v1 v2 = not $ S.null $ S.difference v1 v2

isSet :: MetaValue -> Bool
isSet (MetaValue (CurrentlySet isset) _) = isset

{- Gets only currently set values -}
currentMetaDataValues :: MetaField -> MetaData -> S.Set MetaValue
currentMetaDataValues f m = S.filter isSet (getAllMetaData f m)

currentMetaData :: MetaData -> MetaData
currentMetaData (MetaData m) = removeEmptyFields $ MetaData $
	M.map (S.filter isSet) m

removeEmptyFields :: MetaData -> MetaData
removeEmptyFields (MetaData m) = MetaData $ M.filter (not . S.null) m

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

prop_metadata_sane :: MetaData -> MetaField -> MetaValue -> Bool
prop_metadata_sane m f v = and
	[ S.member v $ getAllMetaData f m'
	, not (isSet v) || S.member v (currentMetaDataValues f m')
	, not (hasUniqueMetaData m m)
	, hasUniqueMetaData newMetaData m'
	, not (hasUniqueMetaData m' newMetaData)
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
  	m' = removeEmptyFields m
