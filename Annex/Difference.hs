{- git-annex repository differences
 -
 - Copyright 2015 Joey Hess <id@joeyh.name>
 -
 - Licensed under the GNU GPL version 3 or higher.
 -}

module Annex.Difference (
	module Types.Difference,
	setDifferences,
) where

import Annex.Common
import Types.Difference
import Logs.Difference
import Config
import Annex.UUID
import Logs.UUID
import Annex.Version
import qualified Annex

import qualified Data.Map as M

-- Differences are only allowed to be tweaked when initializing a
-- repository for the first time, and then only if there is not another
-- known uuid. If the repository was cloned from elsewhere, it inherits
-- the existing settings.
--
-- Must be called before setVersion, so it can check if this is the first
-- time the repository is being initialized.
setDifferences :: Annex ()
setDifferences = do
	u <- getUUID
	otherds <- allDifferences <$> recordedDifferences
	ds <- mappend otherds . annexDifferences <$> Annex.getGitConfig
	when (ds /= mempty) $ do
		ds' <- ifM (isJust <$> getVersion)
			( do
				oldds <- recordedDifferencesFor u
				when (ds /= oldds) $
					warning "Cannot change tunable parameters in already initialized repository."
				return oldds
			, if otherds == mempty
				then ifM (any (/= u) . M.keys <$> uuidMap)
					( do
						warning "Cannot change tunable parameters in a clone of an existing repository."
						return mempty
					, return ds
					)
				else if otherds /= ds
					then do
						warning "The specified tunable parameters differ from values being used in other clones of this repository."
						return otherds
					else return ds
			)
		forM_ (listDifferences ds') $ \d ->
			setConfig (ConfigKey $ differenceConfigKey d) (differenceConfigVal d)
		recordDifferences ds' u
