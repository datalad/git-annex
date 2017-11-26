{- types for metadata based branch views
 -
 - Copyright 2014 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Types.View where

import Annex.Common
import Types.MetaData
import Utility.QuickCheck
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
	arbitrary = View <$> pure (Git.Ref "master") <*> arbitrary

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
	deriving (Eq, Read, Show)

instance Arbitrary ViewFilter where
	arbitrary = do
		size <- arbitrarySizedBoundedIntegral `suchThat` (< 100)
		s <- S.fromList <$> vector size
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
