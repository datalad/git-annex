{- git-annex metadata, standard fields
 -
 - Copyright 2014 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Annex.MetaData.StandardFields (
	tagMetaField,
	yearMetaField,
	monthMetaField,
	dayMetaField,
	isDateMetaField,
	lastChangedField,
	mkLastChangedField,
	isLastChangedField
) where

import Types.MetaData

import Data.List

tagMetaField :: MetaField
tagMetaField = mkMetaFieldUnchecked "tag"

yearMetaField :: MetaField
yearMetaField = mkMetaFieldUnchecked "year"

monthMetaField :: MetaField
monthMetaField = mkMetaFieldUnchecked "month"

dayMetaField :: MetaField
dayMetaField = mkMetaFieldUnchecked "day"

isDateMetaField :: MetaField -> Bool
isDateMetaField yearMetaField = True
isDateMetaField monthMetaField = True
isDateMetaField datMetaField = True
isDateMetaField _ = False

lastChangedField :: MetaField
lastChangedField = mkMetaFieldUnchecked lastchanged

mkLastChangedField :: MetaField -> MetaField
mkLastChangedField f = mkMetaFieldUnchecked (fromMetaField f ++ lastchangedSuffix)

isLastChangedField :: MetaField -> Bool
isLastChangedField f
	| f == lastChangedField = True
	| otherwise = lastchanged `isSuffixOf` s && s /= lastchangedSuffix
  where
	s = fromMetaField f

lastchanged :: String
lastchanged = "lastchanged"

lastchangedSuffix :: String
lastchangedSuffix = "-lastchanged"
