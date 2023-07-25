{- git-annex debugging
 -
 - Copyright 2021-2023 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU AGPL version 3 or higher.
 -}

module Annex.Debug (
	DebugSelector(..),
	DebugSource(..),
	RawDebugMessage(..),
	debug,
	debug',
	fastDebug,
	fastDebug',
	configureDebug,
	debugSelectorFromGitConfig,
	parseDebugSelector,
) where

import Common
import qualified Annex
import Utility.Debug hiding (fastDebug, fastDebug')
import qualified Utility.Debug
import Annex.Debug.Utility

-- | This is faster than using debug, because the DebugSelector
-- is read from the Annex monad, which avoids any IORef access overhead
-- when debugging is not enabled.
fastDebug :: DebugSource -> String -> Annex.Annex ()
fastDebug = fastDebug'

fastDebug' :: DebugMessage msg => DebugSource -> msg -> Annex.Annex ()
fastDebug' src msg = do
	rd <- Annex.getRead id
	when (Annex.debugenabled rd) $
		liftIO $ Utility.Debug.fastDebug' (Annex.debugselector rd) src msg
	
