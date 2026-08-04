{- git-annex repo groups
 -
 - Copyright 2012-2026 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Types.Group (
	Group(..),
	fromGroup,
	toGroup,
	GroupMap(..),
	emptyGroupMap,
	groupUUIDs
) where

import Types.UUID
import Utility.FileSystemEncoding
import Annex.Balanced

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.ByteString as BS
import Data.Maybe

newtype Group = Group BS.ByteString
	deriving (Eq, Ord, Show, Read)

fromGroup :: Group -> String
fromGroup (Group g) = decodeBS g

toGroup :: String -> Group
toGroup = Group . encodeBS

data GroupMap = GroupMap
	{ groupsByUUID :: M.Map UUID (S.Set Group)
	, uuidsByGroup :: M.Map Group (S.Set UUID)
	, balancedPickerByGroup :: M.Map Group BalancedPicker
	}

emptyGroupMap :: GroupMap
emptyGroupMap = GroupMap M.empty M.empty M.empty

groupUUIDs :: Group -> GroupMap -> S.Set UUID
groupUUIDs g = fromMaybe S.empty . M.lookup g . uuidsByGroup
