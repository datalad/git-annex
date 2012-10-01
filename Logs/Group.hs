{- git-annex group log
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Logs.Group (
	groupSet,
	lookupGroups,
	groupMap,
) where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Time.Clock.POSIX

import Common.Annex
import qualified Annex.Branch
import qualified Annex
import Logs.UUIDBased
import Types.Group

{- Filename of group.log. -}
groupLog :: FilePath
groupLog = "group.log"

{- Returns the groups of a given repo UUID. -}
lookupGroups :: UUID -> Annex (S.Set Group)
lookupGroups u = (fromMaybe S.empty . M.lookup u) <$> groupMap

{- Changes the groups for a uuid in the groupLog. -}
groupSet :: UUID -> S.Set Group -> Annex ()
groupSet uuid@(UUID _) groups = do
	ts <- liftIO getPOSIXTime
	Annex.Branch.change groupLog $
		showLog (unwords . S.toList) . changeLog ts uuid groups . 
			parseLog (Just . S.fromList . words)
	Annex.changeState $ \s -> s { Annex.groupmap = Nothing }
groupSet NoUUID _ = error "unknown UUID; cannot modify group"

{- Read the groupLog into a map. The map is cached for speed. -}
groupMap :: Annex GroupMap
groupMap = do
	cached <- Annex.getState Annex.groupmap
	case cached of
		Just m -> return m
		Nothing -> do
			m <- simpleMap . parseLog (Just . S.fromList . words) <$>
				Annex.Branch.get groupLog
			Annex.changeState $ \s -> s { Annex.groupmap = Just m }
			return m
