{- git-annex trust log, basics
 -
 - Copyright 2010-2012 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Logs.Trust.Basic (
	module X,
	trustSet,
	trustMapRaw,
) where

import Annex.Common
import Types.TrustLevel
import qualified Annex.Branch
import qualified Annex
import Logs
import Logs.UUIDBased
import Logs.Trust.Pure as X

{- Changes the trust level for a uuid in the trustLog. -}
trustSet :: UUID -> TrustLevel -> Annex ()
trustSet uuid@(UUID _) level = do
	c <- currentVectorClock
	Annex.Branch.change trustLog $
		buildLogOld buildTrustLevel .
			changeLog c uuid level .
				parseLogOld trustLevelParser
	Annex.changeState $ \s -> s { Annex.trustmap = Nothing }
trustSet NoUUID _ = error "unknown UUID; cannot modify"

{- Does not include forcetrust or git config values, just those from the
 - log file. -}
trustMapRaw :: Annex TrustMap
trustMapRaw = calcTrustMap <$> Annex.Branch.get trustLog
