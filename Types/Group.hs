{- git-annex repo groups
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Types.Group (
	Group,
	GroupMap
) where

import Types.UUID

import qualified Data.Map as M
import qualified Data.Set as S

type Group = String

type GroupMap = M.Map UUID (S.Set Group)
