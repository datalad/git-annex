{- git-annex repository differences
 -
 - Copyright 2015 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

{-# LANGUAGE OverloadedStrings #-}

module Types.Difference (
	Difference(..),
	Differences(..),
	readDifferences,
	showDifferences,
	getDifferences,
	differenceConfigKey,
	differenceConfigVal,
	hasDifference,
	listDifferences,
) where

import Utility.PartialPrelude
import qualified Git
import qualified Git.Config
import Git.Types

import Data.Maybe
import Data.Monoid
import qualified Data.ByteString as B
import qualified Data.Set as S
import qualified Data.Semigroup as Sem
import Prelude

-- Describes differences from the v5 repository format.
--
-- The serialization is stored in difference.log, so avoid changes that
-- would break compatibility.
--
-- Not breaking compatibility is why a list of Differences is used, rather
-- than a record type. With a record type, adding a new field for some future
-- difference would serialize to a value that an older version could not
-- parse, even if that new field was not used. With the Differences list,
-- old versions can still parse it, unless the new Difference constructor 
-- is used.
--
-- The constructors intentionally do not have parameters; this is to
-- ensure that any Difference that can be expressed is supported.
-- So, a new repository version would be Version6, rather than Version Int.
data Difference
	= ObjectHashLower
	| OneLevelObjectHash
	| OneLevelBranchHash
	deriving (Show, Read, Eq, Ord, Enum, Bounded)

-- This type is used internally for efficient checking for differences,
-- but converted to S.Set Difference for serialization.
data Differences
	= Differences
		{ objectHashLower :: Bool
		, oneLevelObjectHash :: Bool
		, oneLevelBranchHash :: Bool
		}
	| UnknownDifferences

-- UnknownDifferences cannot be equal
instance Eq Differences where
	UnknownDifferences == _ = False
	_ == UnknownDifferences = False
	a == b = all (\f -> f a == f b)
		[ objectHashLower
		, oneLevelObjectHash
		, oneLevelBranchHash
		]

appendDifferences :: Differences -> Differences -> Differences
appendDifferences a@(Differences {}) b@(Differences {}) = a
	{ objectHashLower = objectHashLower a || objectHashLower b
	, oneLevelObjectHash = oneLevelObjectHash a || oneLevelObjectHash b
	, oneLevelBranchHash = oneLevelBranchHash a || oneLevelBranchHash b
	}
appendDifferences _ _ = UnknownDifferences

instance Sem.Semigroup Differences where
	(<>) = appendDifferences

instance Monoid Differences where
	mempty = Differences False False False

readDifferences :: String -> Differences
readDifferences = maybe UnknownDifferences mkDifferences . readish

showDifferences :: Differences -> String
showDifferences = show . S.fromList . listDifferences

getDifferences :: Git.Repo -> Differences
getDifferences r = mkDifferences $ S.fromList $
	mapMaybe getmaybe [minBound .. maxBound]
  where
	getmaybe d = case Git.Config.isTrueFalse' =<< Git.Config.getMaybe (differenceConfigKey d) r of
		Just True -> Just d
		_ -> Nothing

differenceConfigKey :: Difference -> ConfigKey
differenceConfigKey ObjectHashLower = tunable "objecthashlower"
differenceConfigKey OneLevelObjectHash = tunable "objecthash1"
differenceConfigKey OneLevelBranchHash = tunable "branchhash1"

differenceConfigVal :: Difference -> String
differenceConfigVal _ = Git.Config.boolConfig True

tunable :: B.ByteString -> ConfigKey
tunable k = ConfigKey ("annex.tune." <> k)

hasDifference :: Difference -> Differences -> Bool
hasDifference _ UnknownDifferences = False
hasDifference ObjectHashLower ds = objectHashLower ds
hasDifference OneLevelObjectHash ds = oneLevelObjectHash ds
hasDifference OneLevelBranchHash ds = oneLevelBranchHash ds

listDifferences :: Differences -> [Difference]
listDifferences d@(Differences {}) = map snd $
	filter (\(f, _) -> f d)
		[ (objectHashLower, ObjectHashLower)
		, (oneLevelObjectHash, OneLevelObjectHash)
		, (oneLevelBranchHash, OneLevelBranchHash)
		]
listDifferences UnknownDifferences = []

mkDifferences :: S.Set Difference -> Differences
mkDifferences s = Differences
	{ objectHashLower = check ObjectHashLower
	, oneLevelObjectHash = check OneLevelObjectHash
	, oneLevelBranchHash = check OneLevelBranchHash
	}
  where
	check f = f `S.member` s
