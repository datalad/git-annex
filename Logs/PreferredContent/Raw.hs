{- unparsed preferred content expressions
 -
 - Copyright 2012-2014 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Logs.PreferredContent.Raw where

import qualified Data.Map as M
import Data.Time.Clock.POSIX

import Annex.Common
import qualified Annex.Branch
import qualified Annex
import Logs
import Logs.UUIDBased
import Logs.MapLog
import Types.StandardGroups
import Types.Group

{- Changes the preferred content configuration of a remote. -}
preferredContentSet :: UUID -> PreferredContentExpression -> Annex ()
preferredContentSet = setLog preferredContentLog

requiredContentSet :: UUID -> PreferredContentExpression -> Annex ()
requiredContentSet = setLog requiredContentLog

setLog :: FilePath -> UUID -> PreferredContentExpression -> Annex ()
setLog logfile uuid@(UUID _) val = do
	ts <- liftIO getPOSIXTime
	Annex.Branch.change logfile $
		showLog id
		. changeLog ts uuid val
		. parseLog Just
	Annex.changeState $ \s -> s 
		{ Annex.preferredcontentmap = Nothing
		, Annex.requiredcontentmap = Nothing
		}
setLog _ NoUUID _ = error "unknown UUID; cannot modify"

{- Changes the preferred content configuration of a group. -}
groupPreferredContentSet :: Group -> PreferredContentExpression -> Annex ()
groupPreferredContentSet g val = do
	ts <- liftIO getPOSIXTime
	Annex.Branch.change groupPreferredContentLog $
		showMapLog id id 
		. changeMapLog ts g val 
		. parseMapLog Just Just
	Annex.changeState $ \s -> s { Annex.preferredcontentmap = Nothing }

preferredContentMapRaw :: Annex (M.Map UUID PreferredContentExpression)
preferredContentMapRaw = simpleMap . parseLog Just
	<$> Annex.Branch.get preferredContentLog

requiredContentMapRaw :: Annex (M.Map UUID PreferredContentExpression)
requiredContentMapRaw = simpleMap . parseLog Just
	<$> Annex.Branch.get requiredContentLog

groupPreferredContentMapRaw :: Annex (M.Map Group PreferredContentExpression)
groupPreferredContentMapRaw = simpleMap . parseMapLog Just Just
	<$> Annex.Branch.get groupPreferredContentLog
