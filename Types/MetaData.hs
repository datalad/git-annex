{- git-annex general metadata
 -
 - Copyright 2014-2019 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}

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
	RemoteMetaData(..),
	extractRemoteMetaData,
	splitRemoteMetaDataField,
	fromRemoteMetaData,
	prop_metadata_sane,
	prop_metadata_serialize
) where

import Common
import Utility.Base64
import Utility.QuickCheck
import Utility.Aeson
import Types.UUID

import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Data.HashMap.Strict as HM
import Data.Char
import qualified Data.CaseInsensitive as CI
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as B8

newtype MetaData = MetaData (M.Map MetaField (S.Set MetaValue))
	deriving (Read, Show, Eq, Ord)

instance ToJSON' MetaData where
	toJSON' (MetaData m) = object $ map go (M.toList m)
	  where
		go (MetaField f, s) = (textKey (CI.original f), toJSON' s)

instance FromJSON MetaData where
	parseJSON (Object o) = do
		l <- HM.toList <$> parseJSON (Object o)
		MetaData . M.fromList <$> mapM go l
	  where
		go (t, l) = case mkMetaField t of
			Left e -> fail e
			Right f -> (,) <$> pure f <*> parseJSON l
	parseJSON _ = fail "expected an object"

{- A metadata value can be currently be set (True), or may have been
 - set before and we're remembering it no longer is (False). -}
newtype CurrentlySet = CurrentlySet Bool
	deriving (Read, Show, Eq, Ord, Arbitrary)

{- Fields are case insensitive. -}
newtype MetaField = MetaField (CI.CI T.Text)
	deriving (Read, Show, Eq, Ord)

data MetaValue = MetaValue CurrentlySet B.ByteString
	deriving (Read, Show)

instance ToJSON' MetaValue where
	toJSON' (MetaValue _ v) = toJSON' v

instance FromJSON MetaValue where
	parseJSON (String v) = return $
		MetaValue (CurrentlySet True) (E.encodeUtf8 v)
	parseJSON _  = fail "expected a string"

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
	serialize :: v -> B.ByteString
	deserialize :: B.ByteString -> Maybe v

instance MetaSerializable MetaData where
	serialize (MetaData m) = B8.unwords $ concatMap go $ M.toList m
	  where
		go (f, vs) = serialize f : map serialize (S.toList vs)
	-- Note that B8.words cannot be used here, because UTF-8 encoded
	-- field names may contain bytes such as \160 that are whitespace.
	deserialize = Just . getfield emptyMetaData . B8.split ' '
	  where
		getfield m [] = m
		getfield m (w:ws) = maybe m (getvalues m ws) (deserialize w)
		getvalues m [] _ = m
		getvalues m l@(w:ws) f = case deserialize w of
			Just v -> getvalues (updateMetaData f v m) ws f
			Nothing -> getfield m l

instance MetaSerializable MetaField where
	serialize (MetaField f) = E.encodeUtf8 (CI.original f)
	deserialize = MetaField . CI.mk <$$> eitherToMaybe . E.decodeUtf8'

{- Base64 problematic values. -}
instance MetaSerializable MetaValue where
	serialize (MetaValue isset v) =
		serialize isset <>
		if B8.any (`elem` [' ', '\r', '\n']) v || "!" `B8.isPrefixOf` v
			then "!" <> toB64 v
			else v
	deserialize b = do
		(isset, b') <- B8.uncons b
		case B8.uncons b' of
			Just ('!', b'') -> MetaValue
				<$> deserialize (B8.singleton isset)
				<*> fromB64Maybe b''
			_ -> MetaValue
				<$> deserialize (B8.singleton isset)
				<*> pure b'

instance MetaSerializable CurrentlySet where
	serialize (CurrentlySet True) = "+"
	serialize (CurrentlySet False) = "-"
	deserialize "+" = Just (CurrentlySet True)
	deserialize "-" = Just (CurrentlySet False)
	deserialize _ = Nothing

mkMetaField :: T.Text -> Either String MetaField
mkMetaField f = maybe (Left $ badField f) Right (toMetaField f)

badField :: T.Text -> String
badField f = "Illegal metadata field name, \"" ++ T.unpack f ++ "\""

{- Does not check that the field name is valid. Use with caution. -}
mkMetaFieldUnchecked :: T.Text -> MetaField
mkMetaFieldUnchecked = MetaField . CI.mk

toMetaField :: T.Text -> Maybe MetaField
toMetaField f
	| legalField f = Just $ MetaField $ CI.mk f
	| otherwise = Nothing

{- Fields cannot be empty, contain whitespace, or start with "+-" as
 - that would break the serialization.
 -
 - Additionally, fields should not contain any form of path separator, as
 - that would break views.
 -
 - And, fields need to be valid JSON keys.
 -
 - So, require they have an alphanumeric first letter, with the remainder
 - being either alphanumeric or a small set of whitelisted common punctuation.
 -}
legalField :: T.Text -> Bool
legalField t = case T.uncons t of
	Nothing -> False
	Just (c1, t')
		| not (isAlphaNum c1) -> False
		| otherwise -> T.all legalchars t'
  where
	legalchars c
		| isAlphaNum c = True
		| otherwise = c `elem` legalFieldWhiteList

legalFieldWhiteList :: [Char]
legalFieldWhiteList = "_-."

toMetaValue :: B.ByteString -> MetaValue
toMetaValue = MetaValue (CurrentlySet True)

mkMetaValue :: CurrentlySet -> B.ByteString -> MetaValue
mkMetaValue = MetaValue

unsetMetaValue :: MetaValue -> MetaValue
unsetMetaValue (MetaValue _ s) = MetaValue (CurrentlySet False) s

{- Marks all MetaValues as no longer currently set. -}
unsetMetaData :: MetaData -> MetaData
unsetMetaData (MetaData m) = MetaData $ M.map (S.map unsetMetaValue) m

fromMetaField :: MetaField -> T.Text
fromMetaField (MetaField f) = CI.original f

fromMetaValue :: MetaValue -> B.ByteString
fromMetaValue (MetaValue _ f) = f

fromMetaData :: MetaData -> [(MetaField, S.Set MetaValue)]
fromMetaData (MetaData m) = M.toList m

emptyMetaData :: MetaData
emptyMetaData = MetaData M.empty

{- Can be used to set a value, or to unset it, depending on whether
 - the MetaValue has CurrentlySet or not. -}
updateMetaData :: MetaField -> MetaValue -> MetaData -> MetaData
updateMetaData f v = updateMetaData' f (S.singleton v)

updateMetaData' :: MetaField -> S.Set MetaValue -> MetaData -> MetaData
updateMetaData' f s (MetaData m) = MetaData $ M.insertWith S.union f s m

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

mapMetaData :: (S.Set MetaValue -> S.Set MetaValue) -> MetaData -> MetaData
mapMetaData f (MetaData m) = MetaData (M.map f m)

{- Ways that existing metadata can be modified -}
data ModMeta
	= AddMeta MetaField MetaValue
	| DelMeta MetaField (Maybe MetaValue)
	-- ^ delete value of a field. With Just, only that specific value
	-- is deleted; with Nothing, all current values are deleted.
	| DelAllMeta
	-- ^ delete all currently set metadata
	| SetMeta MetaField (S.Set MetaValue)
	-- ^ removes any existing values
	| MaybeSetMeta MetaField MetaValue
	-- ^ set when field has no existing value
	| ComposeModMeta ModMeta ModMeta
	-- ^ composing multiple modifications
	deriving (Show)

{- Applies a ModMeta, generating the new MetaData.
 - Note that the new MetaData does not include all the 
 - values set in the input metadata. It only contains changed values. -}
modMeta :: MetaData -> ModMeta -> MetaData
modMeta _ (AddMeta f v) = updateMetaData f v emptyMetaData
modMeta _ (DelMeta f (Just oldv)) =
	updateMetaData f (unsetMetaValue oldv) emptyMetaData
modMeta m (DelMeta f Nothing) = MetaData $ M.singleton f $
	S.fromList $ map unsetMetaValue $ S.toList $ currentMetaDataValues f m
modMeta m DelAllMeta = mapMetaData
	(S.fromList . map unsetMetaValue . S.toList)
	(currentMetaData m)
modMeta m (SetMeta f s) = updateMetaData' f s $
	foldr (updateMetaData f) emptyMetaData $
		map unsetMetaValue $ S.toList $ currentMetaDataValues f m
modMeta m (MaybeSetMeta f v)
	| S.null (currentMetaDataValues f m) = updateMetaData f v emptyMetaData
	| otherwise = emptyMetaData
modMeta m (ComposeModMeta a b) = unionMetaData (modMeta m a) (modMeta m b)

data RemoteMetaData = RemoteMetaData UUID MetaData
	deriving (Show, Eq, Ord)

{- Extracts only the fields prefixed with "uuid:", which belong to that
 - remote. -}
extractRemoteMetaData :: UUID -> MetaData -> RemoteMetaData
extractRemoteMetaData u (MetaData m) = RemoteMetaData u $ MetaData $
	M.mapKeys removeprefix $ M.filterWithKey belongsremote m
  where
	belongsremote (MetaField f) _v = prefix `T.isPrefixOf` CI.original f
	removeprefix (MetaField f) = MetaField $ 
		CI.mk $ T.drop prefixlen $ CI.original f
	prefix = remoteMetaDataPrefix u
	prefixlen = T.length prefix

splitRemoteMetaDataField :: MetaField -> Maybe (UUID, MetaField)
splitRemoteMetaDataField (MetaField f) = do
	let (su, sf) = T.break (== ':') (CI.original f)
	f' <- toMetaField ((T.drop 1 sf))
	return $ (toUUID (T.unpack su), f')

remoteMetaDataPrefix :: UUID -> T.Text
remoteMetaDataPrefix u = T.pack (fromUUID u) <> ":"

fromRemoteMetaData :: RemoteMetaData -> MetaData
fromRemoteMetaData (RemoteMetaData u (MetaData m)) = MetaData $
	M.mapKeys addprefix m
  where
	addprefix (MetaField f) = MetaField $ CI.mk $ prefix <> CI.original f
	prefix = remoteMetaDataPrefix u

{- Avoid putting too many fields in the map; extremely large maps make
 - the seriaization test slow due to the sheer amount of data.
 - It's unlikely that more than 10 fields of metadata will be used. -}
instance Arbitrary MetaData where
	arbitrary = MetaData . M.fromList <$> resize 10 (listOf arbitrary)
	  where

instance Arbitrary MetaValue where
	arbitrary = MetaValue 
		<$> arbitrary
		-- Avoid non-ascii MetaValues because fully arbitrary
		-- strings may not be encoded using the filesystem
		-- encoding, which is normally applied to all input.
		<*> (encodeBS <$> arbitrary `suchThat` all isAscii)

instance Arbitrary MetaField where
	arbitrary = MetaField . CI.mk
		-- Avoid non-ascii MetaFields because fully arbitrary
		-- strings may not be encoded using the filesystem
		-- encoding, which is normally applied to all input.
		<$> (T.pack <$> arbitrary `suchThat` all isAscii)
			`suchThat` legalField

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
