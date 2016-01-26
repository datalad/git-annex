{- git-annex group log
 -
 - Copyright 2012 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Logs.Group (
	groupLog,
	groupChange,
	groupSet,
	lookupGroups,
	groupMap,
	groupMapLoad,
	getStandardGroup,
	inUnwantedGroup
) where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Time.Clock.POSIX

import Annex.Common
import Logs
import qualified Annex.Branch
import qualified Annex
import Logs.UUIDBased
import Types.Group
import Types.StandardGroups

{- Returns the groups of a given repo UUID. -}
lookupGroups :: UUID -> Annex (S.Set Group)
lookupGroups u = (fromMaybe S.empty . M.lookup u) . groupsByUUID <$> groupMap

{- Applies a set modifier to change the groups for a uuid in the groupLog. -}
groupChange :: UUID -> (S.Set Group -> S.Set Group) -> Annex ()
groupChange uuid@(UUID _) modifier = do
	curr <- lookupGroups uuid
	ts <- liftIO getPOSIXTime
	Annex.Branch.change groupLog $
		showLog (unwords . S.toList) .
			changeLog ts uuid (modifier curr) .
				parseLog (Just . S.fromList . words)
	
	-- The changed group invalidates the preferred content cache.
	Annex.changeState $ \s -> s
		{ Annex.groupmap = Nothing
		, Annex.preferredcontentmap = Nothing
		}
groupChange NoUUID _ = error "unknown UUID; cannot modify"

groupSet :: UUID -> S.Set Group -> Annex ()
groupSet u g = groupChange u (const g)

{- The map is cached for speed. -}
groupMap :: Annex GroupMap
groupMap = maybe groupMapLoad return =<< Annex.getState Annex.groupmap

{- Loads the map, updating the cache. -}
groupMapLoad :: Annex GroupMap
groupMapLoad = do
	m <- makeGroupMap . simpleMap .
		parseLog (Just . S.fromList . words) <$>
			Annex.Branch.get groupLog
	Annex.changeState $ \s -> s { Annex.groupmap = Just m }
	return m

makeGroupMap :: M.Map UUID (S.Set Group) -> GroupMap
makeGroupMap byuuid = GroupMap byuuid bygroup
  where
	bygroup = M.fromListWith S.union $
		concatMap explode $ M.toList byuuid
	explode (u, s) = map (\g -> (g, S.singleton u)) (S.toList s)

{- If a repository is in exactly one standard group, returns it. -}
getStandardGroup :: S.Set Group -> Maybe StandardGroup
getStandardGroup s = case mapMaybe toStandardGroup $ S.toList s of
	[g] -> Just g
	_ -> Nothing

inUnwantedGroup :: UUID -> Annex Bool
inUnwantedGroup u = elem UnwantedGroup 
	. mapMaybe toStandardGroup . S.toList <$> lookupGroups u
