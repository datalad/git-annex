{- git-annex metadata, standard fields
 -
 - Copyright 2014 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Annex.MetaData.StandardFields (
	tagMetaField,
	yearMetaField,
	monthMetaField,
	lastChangedField,
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

lastChangedField :: MetaField -> MetaField
lastChangedField f = mkMetaFieldUnchecked (fromMetaField f ++ lastchanged)

isLastChangedField :: MetaField -> Bool
isLastChangedField f = lastchanged `isSuffixOf` s && s /= lastchanged
  where
	s = fromMetaField f

lastchanged :: String
lastchanged = "-lastchanged"
