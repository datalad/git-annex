{- types for metadata based branch views
 -
 - Copyright 2014 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Types.View where

import Common.Annex
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

{- Only files with metadata matching the view are displayed. -}
type FileView = FilePath
type MkFileView = FilePath -> FileView

data ViewFilter
	= FilterValues (S.Set MetaValue)
	| FilterGlob String
	deriving (Eq, Read, Show)

instance Arbitrary ViewFilter where
	arbitrary = do
		size <- arbitrarySizedBoundedIntegral `suchThat` (< 100)
		FilterValues . S.fromList <$> vector size

{- Can a ViewFilter match multiple different MetaValues? -}
multiValue :: ViewFilter -> Bool
multiValue (FilterValues s) = S.size s > 1
multiValue (FilterGlob _) = True
