{- types for metadata based branch views
 -
 - Copyright 2014 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

{-# LANGUAGE CPP #-}

module Types.View where

import Common.Annex
import Types.MetaData
import Utility.QuickCheck

import qualified Data.Set as S

#ifdef WITH_TDFA
import Text.Regex.TDFA
#else
#endif

{- A view is a list of fields with filters on their allowed values. -}
type View = [ViewComponent]

data ViewComponent = ViewComponent
	{ viewField :: MetaField
	, viewFilter :: ViewFilter
	}
	deriving (Show, Eq)

instance Arbitrary ViewComponent where
	arbitrary = ViewComponent <$> arbitrary <*> arbitrary

{- Only files with metadata matching the view are displayed. -}
type FileView = FilePath
type MkFileView = FilePath -> FileView

data ViewFilter
	= FilterValues (S.Set MetaValue)
	| FilterGlob Glob

instance Show ViewFilter where
	show (FilterValues s) = show s
	show (FilterGlob g) = getGlob g

instance Eq ViewFilter where
	FilterValues x == FilterValues y = x == y
	FilterGlob x == FilterGlob y = x == y
	_ == _ = False

instance Arbitrary ViewFilter where
	arbitrary = do
		size <- arbitrarySizedBoundedIntegral `suchThat` (< 100)
		FilterValues . S.fromList <$> vector size

#ifdef WITH_TDFA
data Glob = Glob String Regex
#else
data Glob = Glob String
#endif

instance Eq Glob where
	a == b = getGlob a == getGlob b

getGlob :: Glob -> String
#ifdef WITH_TDFA
getGlob (Glob g _) = g
#else
getGlob (Glob g) = g
#endif
