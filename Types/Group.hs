{- git-annex repo groups
 -
 - Copyright 2012, 2019 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Types.Group (
	Group(..),
	fromGroup,
	toGroup,
	GroupMap(..),
	emptyGroupMap
) where

import Types.UUID
import Utility.FileSystemEncoding

import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.ByteString as S

newtype Group = Group S.ByteString
	deriving (Eq, Ord, Show)

fromGroup :: Group -> String
fromGroup (Group g) = decodeBS g

toGroup :: String -> Group
toGroup = Group . encodeBS

data GroupMap = GroupMap
	{ groupsByUUID :: M.Map UUID (S.Set Group)
	, uuidsByGroup :: M.Map Group (S.Set UUID)
	}

emptyGroupMap :: GroupMap
emptyGroupMap = GroupMap M.empty M.empty
