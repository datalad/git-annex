{- types for metadata based branch views
 -
 - Copyright 2014-2023 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Types.View where

import Types.MetaData
import Utility.QuickCheck
import Utility.Monad
import qualified Git

import qualified Data.Set as S

{- A view is a list of fields with filters on their allowed values,
 - which are applied to files in a parent git branch. -}
data View = View
	{ viewParentBranch :: Git.Branch
	, viewComponents :: [ViewComponent]
	}
	deriving (Eq, Read, Show)

instance Arbitrary View where
	arbitrary = View (Git.Ref "foo")
		<$> resize 10 (listOf arbitrary)

data ViewComponent = ViewComponent
	{ viewField :: MetaField
	, viewFilter :: ViewFilter
	, viewVisible :: Bool
	}
	deriving (Eq, Read, Show)

instance Arbitrary ViewComponent where
	arbitrary = ViewComponent <$> arbitrary <*> arbitrary <*> arbitrary

data ViewFilter
	= FilterValues (S.Set MetaValue)
	| FilterGlob String
	| ExcludeValues (S.Set MetaValue)
	| FilterValuesOrUnset (S.Set MetaValue) ViewUnset
	| FilterGlobOrUnset String ViewUnset
	deriving (Eq, Read, Show)

newtype ViewUnset = ViewUnset String
	deriving (Eq, Read, Show)

instance Arbitrary ViewFilter where
	arbitrary = do
		s <- S.fromList <$> resize 10 (listOf arbitrary)
		ifM arbitrary
			( return (FilterValues s)
			, return (ExcludeValues s)
			)

mkViewComponent :: MetaField -> ViewFilter -> ViewComponent
mkViewComponent f vf = ViewComponent f vf (multiValue vf)

{- Can a ViewFilter match multiple different MetaValues? -}
multiValue :: ViewFilter -> Bool
multiValue (FilterValues s) = S.size s > 1
multiValue (FilterGlob _) = True
multiValue (ExcludeValues _) = False
multiValue (FilterValuesOrUnset _ _) = True
multiValue (FilterGlobOrUnset _ _) = True
