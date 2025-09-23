{- git-annex metadata, standard fields
 -
 - Copyright 2014 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Annex.MetaData.StandardFields (
	tagMetaField,
	yearMetaField,
	monthMetaField,
	dayMetaField,
	isDateMetaField,
	lastChangedField,
	mkLastChangedField,
	isLastChangedField,
	itemIdField
) where

import Types.MetaData

import qualified Data.Text as T

tagMetaField :: MetaField
tagMetaField = mkMetaFieldUnchecked "tag"

yearMetaField :: MetaField
yearMetaField = mkMetaFieldUnchecked "year"

monthMetaField :: MetaField
monthMetaField = mkMetaFieldUnchecked "month"

dayMetaField :: MetaField
dayMetaField = mkMetaFieldUnchecked "day"

isDateMetaField :: MetaField -> Bool
isDateMetaField f
	| f == yearMetaField = True
	| f == monthMetaField = True
	| f == dayMetaField = True
	| otherwise = False

lastChangedField :: MetaField
lastChangedField = mkMetaFieldUnchecked lastchanged

mkLastChangedField :: MetaField -> MetaField
mkLastChangedField f = mkMetaFieldUnchecked (fromMetaField f <> lastchangedSuffix)

isLastChangedField :: MetaField -> Bool
isLastChangedField f
	| f == lastChangedField = True
	| otherwise = lastchanged `T.isSuffixOf` s && s /= lastchangedSuffix
  where
	s = fromMetaField f

lastchanged :: T.Text
lastchanged = "lastchanged"

lastchangedSuffix :: T.Text
lastchangedSuffix = "-lastchanged"

itemIdField :: MetaField
itemIdField = mkMetaFieldUnchecked "itemid"
