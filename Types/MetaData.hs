{- git-annex general metadata
 -
 - Copyright 2014 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types.MetaData (
	MetaData(..),
	MetaField(..),
	MetaValue(..),
	CurrentlySet(..),
	serialize,
	deserialize,
	MetaSerializable,
	toMetaField,
	mkMetaField,
	mkMetaFieldUnchecked,
	fromMetaField,
	toMetaValue,
	mkMetaValue,
	unsetMetaValue,
	unsetMetaData,
	fromMetaValue,
	fromMetaData,
	emptyMetaData,
	updateMetaData,
	unionMetaData,
	combineMetaData,
	differenceMetaData,
	isSet,
	currentMetaData,
	currentMetaDataValues,
	metaDataValues,
	ModMeta(..),
	modMeta,
	parseModMeta,
	parseMetaData,
	prop_metadata_sane,
	prop_metadata_serialize
) where

import Common
import Utility.Base64
import Utility.QuickCheck

import qualified Data.Set as S
import qualified Data.Map as M
import Data.Char
import qualified Data.CaseInsensitive as CI

newtype MetaData = MetaData (M.Map MetaField (S.Set MetaValue))
	deriving (Show, Eq, Ord)

{- A metadata value can be currently be set (True), or may have been
 - set before and we're remembering it no longer is (False). -}
newtype CurrentlySet = CurrentlySet Bool
	deriving (Read, Show, Eq, Ord, Arbitrary)

{- Fields are case insensitive. -}
newtype MetaField = MetaField (CI.CI String)
	deriving (Read, Show, Eq, Ord)

data MetaValue = MetaValue CurrentlySet String
	deriving (Read, Show)

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
	deserialize = Just . getfield emptyMetaData . words
	  where
		getfield m [] = m
		getfield m (w:ws) = maybe m (getvalues m ws) (deserialize w)
		getvalues m [] _ = m
		getvalues m l@(w:ws) f = case deserialize w of
			Just v -> getvalues (updateMetaData f v m) ws f
			Nothing -> getfield m l

instance MetaSerializable MetaField where
	serialize (MetaField f) = CI.original f
	deserialize = Just . mkMetaFieldUnchecked

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

mkMetaField :: String -> Either String MetaField
mkMetaField f = maybe (Left $ badField f) Right (toMetaField f)

badField :: String -> String
badField f = "Illegal metadata field name, \"" ++ f ++ "\""

{- Does not check that the field name is valid. Use with caution. -}
mkMetaFieldUnchecked :: String -> MetaField
mkMetaFieldUnchecked = MetaField . CI.mk

toMetaField :: String -> Maybe MetaField
toMetaField f
	| legalField f = Just $ MetaField $ CI.mk f
	| otherwise = Nothing

{- Fields cannot be empty, contain whitespace, or start with "+-" as
 - that would break the serialization.
 -
 - Additionally, fields should not contain any form of path separator, as
 - that would break views.
 -
 - So, require they have an alphanumeric first letter, with the remainder
 - being either alphanumeric or a small set of whitelisted common punctuation.
 -}
legalField :: String -> Bool
legalField [] = False
legalField (c1:cs)
	| not (isAlphaNum c1) = False
	| otherwise = all legalchars cs
  where
	legalchars c
		| isAlphaNum c = True
		| otherwise = c `elem` "_-."

toMetaValue :: String -> MetaValue
toMetaValue = MetaValue (CurrentlySet True)

mkMetaValue :: CurrentlySet -> String -> MetaValue
mkMetaValue = MetaValue

unsetMetaValue :: MetaValue -> MetaValue
unsetMetaValue (MetaValue _ s) = MetaValue (CurrentlySet False) s

{- Marks all MetaValues as no longer currently set. -}
unsetMetaData :: MetaData -> MetaData
unsetMetaData (MetaData m) = MetaData $ M.map (S.map unsetMetaValue) m

fromMetaField :: MetaField -> String
fromMetaField (MetaField f) = CI.original f

fromMetaValue :: MetaValue -> String
fromMetaValue (MetaValue _ f) = f

fromMetaData :: MetaData -> [(MetaField, S.Set MetaValue)]
fromMetaData (MetaData m) = M.toList m

emptyMetaData :: MetaData
emptyMetaData = MetaData M.empty

{- Can be used to set a value, or to unset it, depending on whether
 - the MetaValue has CurrentlySet or not. -}
updateMetaData :: MetaField -> MetaValue -> MetaData -> MetaData
updateMetaData f v (MetaData m) = MetaData $
	M.insertWith' S.union f (S.singleton v) m

{- New metadata overrides old._-}
unionMetaData :: MetaData -> MetaData -> MetaData
unionMetaData (MetaData old) (MetaData new) = MetaData $
	M.unionWith S.union new old

combineMetaData :: [MetaData] -> MetaData
combineMetaData = foldl' unionMetaData emptyMetaData

differenceMetaData :: MetaData -> MetaData -> MetaData
differenceMetaData (MetaData m) (MetaData excludem) = MetaData $
	M.differenceWith diff m excludem
  where
	diff sl sr =
		let s = S.difference sl sr
		in if S.null s then Nothing else Just s

isSet :: MetaValue -> Bool
isSet (MetaValue (CurrentlySet isset) _) = isset

{- Gets only currently set values -}
currentMetaDataValues :: MetaField -> MetaData -> S.Set MetaValue
currentMetaDataValues f m = S.filter isSet (metaDataValues f m)

currentMetaData :: MetaData -> MetaData
currentMetaData (MetaData m) = removeEmptyFields $ MetaData $
	M.map (S.filter isSet) m

removeEmptyFields :: MetaData -> MetaData
removeEmptyFields (MetaData m) = MetaData $ M.filter (not . S.null) m

{- Gets currently set values, but also values that have been unset. -}
metaDataValues :: MetaField -> MetaData -> S.Set MetaValue
metaDataValues f (MetaData m) = fromMaybe S.empty (M.lookup f m)

{- Ways that existing metadata can be modified -}
data ModMeta
	= AddMeta MetaField MetaValue
	| DelMeta MetaField MetaValue
	| SetMeta MetaField MetaValue -- removes any existing values
	| MaybeSetMeta MetaField MetaValue -- when field has no existing value
	deriving (Show)

{- Applies a ModMeta, generating the new MetaData.
 - Note that the new MetaData does not include all the 
 - values set in the input metadata. It only contains changed values. -}
modMeta :: MetaData -> ModMeta -> MetaData
modMeta _ (AddMeta f v) = updateMetaData f v emptyMetaData
modMeta _ (DelMeta f oldv) = updateMetaData f (unsetMetaValue oldv) emptyMetaData
modMeta m (SetMeta f v) = updateMetaData f v $
	foldr (updateMetaData f) emptyMetaData $
		map unsetMetaValue $ S.toList $ currentMetaDataValues f m
modMeta m (MaybeSetMeta f v)
	| S.null (currentMetaDataValues f m) = updateMetaData f v emptyMetaData
	| otherwise = emptyMetaData

{- Parses field=value, field+=value, field-=value, field?=value -}
parseModMeta :: String -> Either String ModMeta
parseModMeta p = case lastMaybe f of
	Just '+' -> AddMeta <$> mkMetaField f' <*> v
	Just '-' -> DelMeta <$> mkMetaField f' <*> v
	Just '?' -> MaybeSetMeta <$> mkMetaField f' <*> v
	_ -> SetMeta <$> mkMetaField f <*> v
  where
	(f, sv) = separate (== '=') p
	f' = beginning f
	v = pure (toMetaValue sv)

{- Parses field=value -}
parseMetaData :: String -> Either String (MetaField, MetaValue)
parseMetaData p = (,)
	<$> mkMetaField f
	<*> pure (toMetaValue v)
  where
	(f, v) = separate (== '=') p

{- Avoid putting too many fields in the map; extremely large maps make
 - the seriaization test slow due to the sheer amount of data.
 - It's unlikely that more than 100 fields of metadata will be used. -}
instance Arbitrary MetaData where
	arbitrary = do
		size <- arbitrarySizedBoundedIntegral `suchThat` (< 500)
		MetaData . M.filterWithKey legal . M.fromList <$> vector size
	  where
		legal k _v = legalField $ fromMetaField k

instance Arbitrary MetaValue where
	arbitrary = MetaValue 
		<$> arbitrary
		-- Avoid non-ascii metavalues because fully arbitrary
		-- strings may not be encoded using the filesystem
		-- encoding, which is norally applied to all input.
		<*> arbitrary `suchThat` all isAscii

instance Arbitrary MetaField where
	arbitrary = MetaField . CI.mk 
		<$> arbitrary `suchThat` legalField 

prop_metadata_sane :: MetaData -> MetaField -> MetaValue -> Bool
prop_metadata_sane m f v = and
	[ S.member v $ metaDataValues f m'
	, not (isSet v) || S.member v (currentMetaDataValues f m')
	, differenceMetaData m' emptyMetaData == m'
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
