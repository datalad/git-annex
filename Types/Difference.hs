{- git-annex repository differences
 -
 - Copyright 2015 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Types.Difference (
	Difference(..),
	Differences(..),
	getDifferences,
	sanityCheckDifferences,
	differenceConfigKey,
	differenceConfigVal,
	hasDifference,
) where

import Utility.PartialPrelude
import qualified Git
import qualified Git.Config

import Data.List
import Data.Maybe
import Data.Monoid
import Control.Applicative

-- Describes differences from the v5 repository format.
--
-- The serilization is stored in difference.log, so avoid changes that
-- would break compatability.
--
-- Not breaking comparability is why a list of Differences is used, rather
-- than a sum type. With a sum type, adding a new field for some future
-- difference would serialize to a value that an older version could not
-- parse, even if that new field was not used. With the Differences list,
-- old versions can still parse it, unless the new Difference constructor 
-- is used.
data Difference
	= Version Int
	| ObjectHashLower Bool
	| ObjectHashDirectories Int
	| BranchHashDirectories Int
	deriving (Show, Read, Ord)

instance Eq Difference where
	Version a == Version b = a == b
	ObjectHashLower a == ObjectHashLower b = a == b
	ObjectHashDirectories a == ObjectHashDirectories b = a == b
	BranchHashDirectories a == BranchHashDirectories b = a == b
	_ == _ = False

data Differences
	= Differences [Difference]
	| UnknownDifferences
	deriving (Show, Read, Ord)

instance Eq Differences where
	Differences a == Differences b = simplify (defver:a) == simplify (defver:b)
	_ == _ = False

instance Monoid Differences where
	mempty = Differences []
	mappend (Differences l1) (Differences l2) = Differences (simplify (l1 ++ l2))
	mappend _ _ = UnknownDifferences

-- This is the default repository version that is assumed when no other one
-- is given. Note that [] == [Version 5]
defver :: Difference
defver = Version 5

-- Given [Version 6, Version 5], returns [Version 6]
simplify :: [Difference] -> [Difference]
simplify = go . sort
  where
	go [] = []
	go (d:[]) = [d]
	go (d1:d2:ds)
		| like d1 d2 = go (d2:ds)
		| otherwise = d1 : go (d2:ds)

	like (Version _) (Version _) = True
	like _ _ = False

getDifferences :: Git.Repo -> Differences
getDifferences r = checksane $ Differences $ catMaybes
	[ ObjectHashLower 
		<$> getmaybebool (differenceConfigKey (ObjectHashLower undefined))
	, ObjectHashDirectories 
		<$> getmayberead (differenceConfigKey (ObjectHashDirectories undefined))
	, BranchHashDirectories
		<$> getmayberead (differenceConfigKey (BranchHashDirectories undefined))
	]
  where
	getmaybe k = Git.Config.getMaybe k r
	getmayberead k = readish =<< getmaybe k
	getmaybebool k = Git.Config.isTrue =<< getmaybe k
	checksane = either error id . sanityCheckDifferences

differenceConfigKey :: Difference -> String
differenceConfigKey (Version _) = "annex.version"
differenceConfigKey (ObjectHashLower _) = tunable "objecthashlower"
differenceConfigKey (ObjectHashDirectories _) = tunable "objecthashdirectories"
differenceConfigKey (BranchHashDirectories _) = tunable "branchhashdirectories"

differenceConfigVal :: Difference -> String
differenceConfigVal (Version v) = show v
differenceConfigVal (ObjectHashLower b) = Git.Config.boolConfig b
differenceConfigVal (ObjectHashDirectories n) = show n
differenceConfigVal (BranchHashDirectories n) = show n

tunable :: String -> String
tunable k = "annex.tune." ++ k

sanityCheckDifferences :: Differences -> Either String Differences
sanityCheckDifferences d@(Differences l)
	| null problems = Right d
	| otherwise = Left (intercalate "; " problems)
  where
	problems = catMaybes (map check l)
	check (ObjectHashDirectories n)
		| n == 1 || n == 2 = Nothing
		| otherwise = Just $ "Bad value for objecthashdirectories -- should be 1 or 2, not " ++ show n
	check (BranchHashDirectories n)
		| n == 1 || n == 2 = Nothing
		| otherwise = Just $ "Bad value for branhhashdirectories -- should be 1 or 2, not " ++ show n
	check _ = Nothing
sanityCheckDifferences UnknownDifferences = Left "unknown differences detected; update git-annex"

hasDifference :: (Difference -> Bool) -> Differences -> Bool
hasDifference f (Differences l) = any f l
hasDifference _ UnknownDifferences = False
