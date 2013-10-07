{- git-annex scheduled activities log
 -
 - Copyright 2013 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Logs.Schedule (
	scheduleLog,
	scheduleSet,
	scheduleGet,
) where

import qualified Data.Map as M
import Data.Time.Clock.POSIX

import Common.Annex
import Types.ScheduledActivity
import qualified Annex.Branch
import Logs
import Logs.UUIDBased

scheduleSet :: UUID -> [ScheduledActivity] -> Annex ()
scheduleSet uuid@(UUID _) activities = do
	ts <- liftIO getPOSIXTime
	Annex.Branch.change scheduleLog $
		showLog id . changeLog ts uuid val . parseLog Just
  where
  	val = intercalate "; " $ map fromScheduledActivity activities
scheduleSet NoUUID _ = error "unknown UUID; cannot modify"

scheduleMap :: Annex (M.Map UUID [ScheduledActivity])
scheduleMap = simpleMap
	. parseLogWithUUID parser
	<$> Annex.Branch.get scheduleLog
  where
	parser _uuid = Just . mapMaybe toScheduledActivity . split "; "

scheduleGet :: UUID -> Annex [ScheduledActivity]
scheduleGet u = do
	m <- scheduleMap
	return $ fromMaybe [] $ M.lookup u m
