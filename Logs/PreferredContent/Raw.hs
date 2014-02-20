{- unparsed preferred content expressions
 -
 - Copyright 2012 Joey Hess <joey@kitenet.net>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Logs.PreferredContent.Raw where

import qualified Data.Map as M
import Data.Time.Clock.POSIX

import Common.Annex
import qualified Annex.Branch
import qualified Annex
import Logs
import Logs.UUIDBased
import Types.StandardGroups

{- Changes the preferred content configuration of a remote. -}
preferredContentSet :: UUID -> PreferredContentExpression -> Annex ()
preferredContentSet uuid@(UUID _) val = do
	ts <- liftIO getPOSIXTime
	Annex.Branch.change preferredContentLog $
		showLog id . changeLog ts uuid val . parseLog Just
	Annex.changeState $ \s -> s { Annex.preferredcontentmap = Nothing }
preferredContentSet NoUUID _ = error "unknown UUID; cannot modify"

preferredContentMapRaw :: Annex (M.Map UUID PreferredContentExpression)
preferredContentMapRaw = simpleMap . parseLog Just
	<$> Annex.Branch.get preferredContentLog
