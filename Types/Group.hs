{- git-annex repo groups
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Types.Group (
	Group,
	GroupMap(..),
	emptyGroupMap
) where

import Types.UUID

import qualified Data.Map as M
import qualified Data.Set as S

type Group = String

data GroupMap = GroupMap
	{ groupsByUUID :: M.Map UUID (S.Set Group)
	, uuidsByGroup :: M.Map Group (S.Set UUID)
	}

emptyGroupMap :: GroupMap
emptyGroupMap = GroupMap M.empty M.empty
